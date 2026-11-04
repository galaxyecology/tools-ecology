#!/usr/bin/env python3
"""Download the reference data used by the SAM3 tool tests.

The wrapper needs a model checkpoint that is far too large to ship with the
tool, so for a local `planemo test` it is registered in a tool data table
(`test-data/huggingface.loc`, described by `tool_data_table_conf.xml.test`).
This script reads that table and downloads every row whose `path` column points
at a file that is not there yet; the column order is taken from the table
configuration, so it does not need to be updated when the table changes.

Usage::

    python test-data/download_reference_data.py --dry-run  # show the plan
    python test-data/download_reference_data.py --force    # download again
    python test-data/download_reference_data.py            # fetch missing

Downloads are streamed to `<path>.part` with a progress report and are resumed
from that file after a broken connection, so an interrupted 3.5 GB checkpoint
does not start from zero. The URL of a row is, in this order of precedence:

1. ``--url`` (only useful when the table has a single row),
2. ``SOURCES`` below, keyed by the ``value`` column,
3. derived from the ``value`` column when it is a Hugging Face repo id
   (``org/name``), i.e. ``https://huggingface.co/org/name/resolve/main/<file>``
   with ``<file>`` taken from the ``path`` column.
"""

import argparse
import contextlib
import json
import os
import re
import socket
import sys
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Dict, Iterator, List, Optional, Tuple
from xml.etree import ElementTree

HERE = Path(__file__).resolve().parent
DEFAULT_CONF = HERE.parent / "tool_data_table_conf.xml.test"
DEFAULT_TOOL_DATA_PATH = HERE
TABLE_NAME = "huggingface"

# Explicit locations for entries that cannot be derived from the table.
SOURCES: Dict[str, str] = {
    "sam3_1038lab": "https://huggingface.co/1038lab/sam3/resolve/main/sam3.pt",
}

HF_RESOLVE_URL = "https://huggingface.co/{repo}/resolve/main/{filename}"
HF_REPO_RE = re.compile(r"^[A-Za-z0-9][\w.-]*/[\w.-]+$")
USER_AGENT = "sam3-reference-data-downloader"
CHUNK_SIZE = 1024 * 1024


def parse_arguments(argv: Optional[List[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Download the reference data of a tool data table.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--conf",
        type=Path,
        default=DEFAULT_CONF,
        help="tool_data_table_conf.xml that describes the table",
    )
    parser.add_argument(
        "--table",
        default=TABLE_NAME,
        help="name of the table inside the configuration file",
    )
    parser.add_argument(
        "--tool-data-path",
        type=Path,
        default=DEFAULT_TOOL_DATA_PATH,
        help="Galaxy tool_data_path, i.e. the directory holding the .loc file",
    )
    parser.add_argument(
        "--loc",
        type=Path,
        default=argparse.SUPPRESS,
        help="the .loc file to read (default: the <file> entry of the table)",
    )
    parser.add_argument(
        "--dest",
        type=Path,
        default=Path("."),
        help="directory to download into; the file name is the one in the loc "
        "file (default: the current working directory)",
    )
    parser.add_argument(
        "--use-loc-paths",
        action="store_true",
        help="write to the path column of the .loc file instead of --dest",
    )
    parser.add_argument(
        "--url",
        default=None,
        help="download URL overriding the per-row resolution",
    )
    parser.add_argument(
        "--token",
        default=os.environ.get("HF_TOKEN"),
        help="access token for gated repositories (default: $HF_TOKEN)",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="download again even if the target file already exists",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="print the download plan without fetching anything",
    )
    parser.add_argument(
        "--ip",
        choices=("4", "6", "auto"),
        default="4",
        help="address family to prefer; IPv6 routes often time out, and every "
        "dead address costs one --timeout before the next is tried",
    )
    parser.add_argument(
        "--connect-timeout",
        type=float,
        default=20.0,
        help="overall deadline for the reachability check, in seconds",
    )
    parser.add_argument(
        "--no-preflight",
        dest="preflight",
        action="store_false",
        help="skip the reachability check and go straight to the download",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=60.0,
        help="seconds without data before a connection is given up on",
    )
    parser.add_argument(
        "--retries",
        type=int,
        default=5,
        help="attempts per file, resuming after each broken connection",
    )
    return parser.parse_args(argv)


def read_table(conf_path: Path, table_name: str) -> Tuple[List[str], str, str]:
    """Return (columns, loc file name, comment character) of a data table."""
    try:
        root = ElementTree.parse(conf_path).getroot()
    except OSError as error:
        sys.exit(f"Cannot read the table configuration: {error}")
    except ElementTree.ParseError as error:
        sys.exit(f"Invalid XML in {conf_path}: {error}")

    for table in root.findall("table"):
        if table.get("name") != table_name:
            continue
        column_text = table.findtext("columns") or ""
        columns = [column.strip() for column in column_text.split(",")]
        columns = [column for column in columns if column]
        file_element = table.find("file")
        loc_name = None
        if file_element is not None:
            loc_name = file_element.get("path")
        if not columns or not loc_name:
            sys.exit(
                f"Table '{table_name}' in {conf_path} lacks "
                "<columns> or <file>"
            )
        return columns, Path(loc_name).name, table.get("comment_char") or "#"

    sys.exit(f"No table named '{table_name}' in {conf_path}")


def read_loc(
    loc_path: Path, columns: List[str], comment_char: str
) -> List[Dict[str, str]]:
    """Parse a tab separated .loc file into a list of row dictionaries."""
    try:
        lines = loc_path.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        sys.exit(f"Cannot read the location file: {error}")

    rows = []
    for number, line in enumerate(lines, start=1):
        if not line.strip() or line.lstrip().startswith(comment_char):
            continue
        fields = [field.strip() for field in line.split("\t")]
        if len(fields) < len(columns):
            print(
                f"{loc_path}:{number}: expected {len(columns)} tab separated "
                f"fields, found {len(fields)} - skipping",
                file=sys.stderr,
            )
            continue
        rows.append(dict(zip(columns, fields)))
    return rows


def expand_path_column(value: str, loc_path: Path) -> Path:
    """Turn the path column into a Path.

    ${__HERE__} is Galaxy's placeholder for the directory holding the .loc file
    (``here = os.path.dirname(os.path.abspath(filename))`` in
    galaxy.tool_util.data), so it has to be expanded here, in exactly the same
    way, for the "is it already downloaded?" check to look at the same file.
    """
    here = loc_path.resolve().parent
    return Path(value.replace("${__HERE__}", str(here))).expanduser()


def resolve_url(row: Dict[str, str], override: Optional[str]) -> Optional[str]:
    """Work out where the file of a single table row comes from."""
    if override:
        return override
    value = row.get("value", "")
    if value in SOURCES:
        return SOURCES[value]
    if HF_REPO_RE.match(value) and row.get("path"):
        filename = Path(row["path"]).name
        return HF_RESOLVE_URL.format(repo=value, filename=filename)
    return None


def human_size(size: float) -> str:
    for unit in ("B", "kB", "MB", "GB"):
        if size < 1024 or unit == "GB":
            if unit == "B":
                return f"{size:.0f} {unit}"
            return f"{size:.1f} {unit}"
        size /= 1024
    return f"{size:.1f} GB"


def human_duration(seconds: float) -> str:
    minutes, seconds = divmod(int(seconds), 60)
    hours, minutes = divmod(minutes, 60)
    if hours:
        return f"{hours}h{minutes:02d}m"
    if minutes:
        return f"{minutes}m{seconds:02d}s"
    return f"{seconds}s"


def report(done: int, total: int, elapsed: float) -> str:
    """One line of progress: bytes, speed and, if known, the time left."""
    speed = done / elapsed if elapsed > 0 else 0.0
    line = f"  {human_size(done)}"
    if total:
        line += f" / {human_size(total)} ({100 * done / total:.0f}%)"
    line += f" at {human_size(speed)}/s"
    if total and speed > 0 and done < total:
        line += f", {human_duration((total - done) / speed)} left"
    return line


class Heartbeat(threading.Thread):
    """Report that a blocking call is still running, every few seconds.

    Opening the connection to a Hugging Face mirror can take minutes behind a
    slow proxy or a dead route, and nothing can be downloaded until it is open,
    so without this the script looks hung.
    """

    def __init__(self, message: str, interval: float = 5.0) -> None:
        super().__init__(daemon=True)
        self.message = message
        self.interval = interval
        self._stop_event = threading.Event()
        self._start_time = time.monotonic()

    def run(self) -> None:
        while not self._stop_event.wait(self.interval):
            elapsed = time.monotonic() - self._start_time
            line = f"  {self.message} ({elapsed:.0f}s)"
            print(line, file=sys.stderr, flush=True)

    def __enter__(self) -> "Heartbeat":
        self.start()
        return self

    def __exit__(self, *exception: object) -> None:
        self._stop_event.set()
        self.join()

    @property
    def elapsed(self) -> float:
        return time.monotonic() - self._start_time


@contextlib.contextmanager
def preferred_address_family(family: int) -> Iterator[None]:
    """Make socket.getaddrinfo() offer *family* addresses first.

    urlopen() walks the address list one ``timeout`` at a time, so a host that
    resolves to unreachable IPv6 addresses first stalls the download for
    ``timeout`` seconds per address. Filtering here keeps the fallback to the
    system order when the host has no address of the wanted family.
    """
    if not family:
        yield
        return

    original = socket.getaddrinfo

    def filtered(host, port, fam=0, type=0, proto=0, flags=0):
        results = original(host, port, fam, type, proto, flags)
        wanted = [info for info in results if info[0] == family]
        return wanted or results

    socket.getaddrinfo = filtered
    try:
        yield
    finally:
        socket.getaddrinfo = original


def read_meta(meta_path: Path) -> Dict[str, object]:
    """Return the sidecar note of a partial download, or an empty dict."""
    try:
        return json.loads(meta_path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}


def content_range_start(header: Optional[str]) -> Optional[int]:
    """Return the first byte position of a "bytes 100-199/200" header."""
    match = re.match(r"bytes\s+(\d+)-", header or "")
    return int(match.group(1)) if match else None


def download(
    url: str,
    target: Path,
    token: Optional[str],
    timeout: float,
    family: int = 0,
) -> None:
    """One download attempt, resuming a previous .part file when possible.

    The .part file is paired with a .part.json sidecar recording the URL and
    the size the server announced, so a leftover partial file is only
    continued when it really belongs to this download.
    """
    partial = Path(f"{target}.part")
    meta_path = Path(f"{target}.part.json")
    target.parent.mkdir(parents=True, exist_ok=True)

    offset = partial.stat().st_size if partial.exists() else 0
    meta = read_meta(meta_path)
    if offset and meta.get("url") not in (None, url):
        print("  discarding a partial file from another URL", file=sys.stderr)
        partial.unlink()
        meta_path.unlink(missing_ok=True)
        offset = 0

    headers = {"User-Agent": USER_AGENT}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    if offset:
        headers["Range"] = f"bytes={offset}-"

    print(f"  {url}\n  -> {target}", file=sys.stderr)
    host = urllib.parse.urlsplit(url).netloc or url
    labels = {socket.AF_INET: " over IPv4", socket.AF_INET6: " over IPv6"}
    label = labels.get(family, "")
    request = urllib.request.Request(url, headers=headers)
    message = f"connecting to {host}{label} - no response yet"
    with preferred_address_family(family):
        with Heartbeat(message) as heartbeat:
            response = urllib.request.urlopen(request, timeout=timeout)
            connect_time = heartbeat.elapsed
    if connect_time >= 1.0:
        print(f"  connected after {connect_time:.0f}s", file=sys.stderr)

    with response:
        status = getattr(response, "status", 200)
        resuming = bool(offset) and status == 206
        range_header = response.headers.get("Content-Range")
        range_start = content_range_start(range_header)
        if resuming and range_start != offset:
            resuming = False
        if offset and not resuming:
            print("  server cannot resume, starting over", file=sys.stderr)
        done = offset if resuming else 0
        length = response.headers.get("Content-Length")
        total = done + int(length) if length else 0
        known_total = meta.get("total")
        if resuming and known_total and total and known_total != total:
            partial.unlink()
            meta_path.unlink(missing_ok=True)
            raise OSError(
                f"the remote file changed size ({known_total} -> {total}), "
                "starting over"
            )

        if resuming:
            print(f"  resuming at {human_size(done)}", file=sys.stderr)
        elif total:
            print(f"  size: {human_size(total)}", file=sys.stderr)
        meta_path.write_text(
            json.dumps({"url": url, "total": total or known_total or 0}),
            encoding="utf-8",
        )

        started = time.monotonic()
        last_report = 0.0
        with open(partial, "ab" if resuming else "wb") as handle:
            while True:
                block = response.read(CHUNK_SIZE)
                if not block:
                    break
                handle.write(block)
                done += len(block)
                now = time.monotonic()
                if now - last_report >= 1.0:
                    last_report = now
                    line = report(done, total, now - started)
                    print(line, file=sys.stderr, flush=True)

        summary = report(done, total, time.monotonic() - started)
        print(f"  {summary} - done", file=sys.stderr)
    if total and done != total:
        raise OSError(f"incomplete download: {done} of {total} bytes")
    os.replace(partial, target)
    meta_path.unlink(missing_ok=True)


class Unreachable(Exception):
    """The URL cannot be downloaded at all - retrying will not help."""


def preflight(
    url: str, token: Optional[str], timeout: float, family: int
) -> None:
    """Check that url answers, within one overall deadline.

    urlopen() has no total timeout, it only bounds single socket operations, so
    a host with several dead addresses can stall for minutes. The probe runs in
    a daemon thread and is abandoned at the deadline, which also covers a slow
    DNS lookup.
    """
    result: Dict[str, object] = {}
    headers = {"User-Agent": USER_AGENT, "Range": "bytes=0-0"}
    if token:
        headers["Authorization"] = f"Bearer {token}"

    def probe() -> None:
        request = urllib.request.Request(url, headers=headers)
        try:
            with preferred_address_family(family):
                opened = urllib.request.urlopen(request, timeout=timeout)
                with opened as response:
                    result["status"] = getattr(response, "status", 200)
        except urllib.error.HTTPError as error:
            result["status"] = error.code
            result["reason"] = str(error)
        except (urllib.error.URLError, OSError) as error:
            result["error"] = error

    thread = threading.Thread(target=probe, daemon=True)
    thread.start()
    thread.join(timeout)

    if thread.is_alive():
        raise Unreachable(f"no response within {timeout:.0f}s")
    if "error" in result:
        raise Unreachable(str(result["error"]))
    status = int(result.get("status") or 0)
    if status in (401, 403):
        raise Unreachable(
            f"HTTP {status}: access denied - a gated repository needs a token "
            "(pass --token or set HF_TOKEN)"
        )
    if 400 <= status < 500 and status not in (408, 429):
        raise Unreachable(f"HTTP {status}")
    if status >= 500 or status == 429:
        # Transient: the download itself retries it, so this is only a note.
        print(f"  server answered HTTP {status}, will retry", file=sys.stderr)


def permanent(error: BaseException) -> bool:
    """True for errors that retrying cannot fix."""
    if isinstance(error, urllib.error.HTTPError):
        return 400 <= error.code < 500 and error.code not in (408, 429)
    return isinstance(error, (ValueError, FileNotFoundError))


def fetch(
    url: str,
    target: Path,
    token: Optional[str],
    timeout: float,
    retries: int,
    family: int = 0,
) -> bool:
    """Download with retries; every attempt resumes the .part file."""
    for attempt in range(1, max(retries, 1) + 1):
        try:
            download(url, target, token, timeout, family)
            return True
        except (urllib.error.URLError, OSError) as error:
            partial = Path(f"{target}.part")
            kept = partial.stat().st_size if partial.exists() else 0
            if permanent(error):
                print(f"  giving up: {error}", file=sys.stderr)
                return False
            if attempt == retries:
                print(
                    f"  giving up after {attempt} attempt(s): {error}",
                    file=sys.stderr,
                )
                return False
            wait = min(2**attempt, 30)
            print(
                f"  attempt {attempt} failed after {human_size(kept)}: {error}"
                f" - retrying in {wait}s",
                file=sys.stderr,
            )
            time.sleep(wait)
    return False


def main(argv: Optional[List[str]] = None) -> int:
    args = parse_arguments(argv)

    columns, loc_name, comment_char = read_table(args.conf, args.table)
    loc_path = getattr(args, "loc", "") or Path(args.tool_data_path) / loc_name
    loc_path = Path(loc_path)
    if not loc_path.exists():
        sys.exit(
            f"No location file at {loc_path}.\n"
            "Create one row for each reference file, with tab separated "
            f"columns {', '.join(columns)}, e.g.:\n"
            f"  {list(SOURCES)[0]}\tsam3.pt\tmask-generation\timage\tsam3\t1\t"
            "/tmp/sam3_models/sam3.pt"
        )

    rows = read_loc(loc_path, columns, comment_char)
    if not rows:
        sys.exit(f"{loc_path} has no data rows")

    family = {"4": socket.AF_INET, "6": socket.AF_INET6}.get(args.ip, 0)

    failures = 0
    downloads = 0
    misplaced = 0
    dest = args.dest.expanduser()
    for row in rows:
        identifier = row.get("value", "?")
        if not row.get("path"):
            print(f"{identifier}: no path column, skipping", file=sys.stderr)
            failures += 1
            continue
        listed = expand_path_column(row["path"], loc_path)
        target = listed if args.use_loc_paths else dest / listed.name

        url = resolve_url(row, args.url)
        if not url:
            print(
                f"{identifier}: cannot work out a download URL. Add it to "
                f"SOURCES in {Path(__file__).name} or pass --url.",
                file=sys.stderr,
            )
            failures += 1
            continue

        if args.dry_run:
            state = ""
            if target.exists() and target.stat().st_size:
                size = human_size(target.stat().st_size)
                state = f" (already present, {size})"
            print(
                f"{identifier}: would download {url}\n  -> {target}{state}",
                file=sys.stderr,
            )
            continue

        if target.exists() and target.stat().st_size and not args.force:
            print(
                f"{identifier}: {target} already present "
                f"({human_size(target.stat().st_size)}), skipping",
                file=sys.stderr,
            )
            continue
        listed_present = listed.is_file() and listed.stat().st_size
        if not args.force and target != listed and listed_present:
            print(
                f"{identifier}: {listed} is already there "
                f"({human_size(listed_present)}), as listed in "
                f"{loc_path.name}; leaving it alone (--force would download "
                f"it to {target} again)",
                file=sys.stderr,
            )
            continue

        print(f"{identifier}:", file=sys.stderr)
        if args.preflight:
            try:
                preflight(url, args.token, args.connect_timeout, family)
            except Unreachable as error:
                print(f"  not reachable: {error}", file=sys.stderr)
                print(
                    "  nothing was downloaded; check the network, the URL in "
                    f"{loc_path.name}, and --ip (currently {args.ip})",
                    file=sys.stderr,
                )
                failures += 1
                continue
        if fetch(url, target, args.token, args.timeout, args.retries, family):
            saved = human_size(target.stat().st_size)
            print(f"{identifier}: saved {saved}", file=sys.stderr)
            downloads += 1
            misplaced += target != listed
        else:
            failures += 1

    if misplaced:
        print(
            f"Downloaded {misplaced} file(s) into {Path.cwd()}: either point "
            f"the path column of {loc_path} at them, or rerun with "
            "--use-loc-paths to write to the paths the loc file lists",
            file=sys.stderr,
        )
    if failures:
        print(f"{failures} reference file(s) not available", file=sys.stderr)
        return 1
    print("All reference data is in place", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
