import argparse
import hashlib
import json
import os
from pathlib import Path
from typing import Any, Dict, List

import cv2
import numpy as np
import ultralytics
from pycocotools.coco import COCO
from ultralytics.models.sam import SAM3SemanticPredictor
from ultralytics.models.sam.predict import SAM3VideoSemanticPredictor

# -------- Constants --------
VIDEO_EXTENSIONS = {".mp4", ".avi", ".mov", ".mkv"}
DEFAULT_CONFIDENCE = 0.25
DEFAULT_VID_STRIDE = 5


# -------- Arguments --------
def parse_arguments() -> argparse.Namespace:
    """Parse and validate command-line arguments."""
    parser = argparse.ArgumentParser(
        description="SAM3 to COCO/YOLO exporter with semantic segmentation"
    )
    parser.add_argument(
        "--model", type=str, help="Output directory for annotations and images"
    )
    parser.add_argument(
        "--outdir",
        type=str,
        default="outputs/",
        help="Output directory for annotations and images",
    )
    parser.add_argument(
        "--prompts",
        type=str,
        required=True,
        help="Comma-separated list of class text prompts"
        "(e.g., 'human,elephant')",
    )
    parser.add_argument(
        "--conf",
        type=float,
        default=DEFAULT_CONFIDENCE,
        help="Confidence threshold for predictions",
    )
    parser.add_argument(
        "--vid_stride",
        type=int,
        default=DEFAULT_VID_STRIDE,
        help="Frame stride for video prediction (process every Nth frame)",
    )
    parser.add_argument(
        "--outputs",
        type=str,
        default="",
        help="Comma-separated output formats: 'coco', 'yolo_bbox', 'yolo_seg'",
    )
    parser.add_argument(
        "--name_file",
        type=str,
        default=None,
        help="Specific filename to process (optional)",
    )
    return parser.parse_args()


# -------- Functions --------


def is_video(file_path: str) -> bool:
    """Check if a file is a video based on its extension."""
    return Path(file_path).suffix.lower() in VIDEO_EXTENSIONS


def compute_file_hash(filepath: Path) -> str:
    """Compute SHA256 hash of a file."""
    hasher = hashlib.sha256()
    with open(filepath, "rb") as f:
        # Read file in chunks to handle large files efficiently
        for chunk in iter(lambda: f.read(8192), b""):
            hasher.update(chunk)
    return hasher.hexdigest()


def validate_coco_format(annotation_file: Path) -> bool:
    """Validate COCO JSON format."""
    try:
        COCO(str(annotation_file))
        print(f"✓ COCO file is valid: {annotation_file}")
        return True
    except Exception as e:
        print(f"COCO format error: {e}")
        return False


def create_coco_categories(text_prompts: List[str]) -> List[Dict[str, Any]]:
    """
    Create COCO categories from text prompts.
    Returns: List of category dictionaries
    """
    return [
        {"id": i + 1, "name": label}
        for i, label in enumerate(text_prompts)
    ]


def create_coco_output(
    results: List[Any], text_prompts: List[str], metadata: Dict[str, Any]
) -> Dict[str, Any]:
    """Convert SAM3 results to COCO format."""
    coco_output = {
        "info": metadata,
        "images": [],
        "annotations": [],
        "categories": create_coco_categories(text_prompts),
    }

    annotation_id = 1

    for image_idx, result in enumerate(results):
        if result.masks is None:
            continue

        height, width = result.orig_shape
        image_id = image_idx + 1
        filename = Path(result.path).name

        # Add image information
        coco_output["images"].append(
            {
                "id": image_id,
                "file_name": filename,
                "width": width,
                "height": height,
            }
        )

        # Add annotations for each detected object
        for polygon, bbox, class_id in zip(
            result.masks.xyn, result.boxes.xyxyn, result.boxes.cls
        ):
            # Flatten polygon coordinates
            polygon_flat = polygon.flatten().tolist()

            # Extract bounding box coordinates (normalized)
            x1, y1, x2, y2 = bbox[:4].tolist()

            # Calculate area using contour
            area = float(cv2.contourArea(polygon.astype(np.float32)))

            coco_output["annotations"].append(
                {
                    "id": annotation_id,
                    "image_id": image_id,
                    "category_id": int(class_id) + 1,
                    "segmentation": [polygon_flat],
                    "area": area,
                    "bbox": [x1, y1, x2, y2],
                    "iscrowd": 0,
                }
            )
            annotation_id += 1

    return coco_output


def create_yolo_bbox_annotation(box: np.ndarray, class_id: int) -> str:
    """Create YOLO bounding box annotation line."""
    x1, y1, x2, y2 = box[:4].tolist()

    # Convert to YOLO format (center_x, center_y, width, height)
    x_center = (x1 + x2) / 2
    y_center = (y1 + y2) / 2
    bbox_width = x2 - x1
    bbox_height = y2 - y1

    return (
    f"{class_id} {x_center:.6f} "
    f"{y_center:.6f} {bbox_width:.6f} "
    f"{bbox_height:.6f}"
    )


def create_yolo_seg_annotation(polygon: np.ndarray, class_id: int) -> str:
    """Create YOLO segmentation annotation line."""
    # Flatten polygon coordinates
    flattened = [f"{coord:.6f}" for point in polygon for coord in point]
    return f"{class_id} " + " ".join(flattened)


def create_yolo_output(
    annotation_type: str, results: List[Any], output_dir: Path
) -> None:
    """Export annotations in YOLO format for images."""
    # Create subdirectories for images and labels
    images_dir = output_dir / "images"
    labels_dir = output_dir / "labels"
    images_dir.mkdir(exist_ok=True, parents=True)
    labels_dir.mkdir(exist_ok=True, parents=True)

    for result in results:
        # Generate output filename based on source image
        image_name = Path(result.path).stem

        # Copy image to images directory
        image_src = Path(result.path)
        image_dst = images_dir / f"{image_name}{image_src.suffix}"

        import shutil

        shutil.copy2(image_src, image_dst)

        # Create label file path
        output_path = labels_dir / f"{image_name}.txt"

        lines = []

        # Process each detection
        for i, (box, class_id) in enumerate(
            zip(result.boxes.xyxyn, result.boxes.cls)
        ):
            class_id = int(class_id)

            if annotation_type == "bbox":
                line = create_yolo_bbox_annotation(box, class_id)
            else:  # segmentation
                if result.masks is None or not hasattr(result.masks, "xyn"):
                    continue
                polygon = result.masks.xyn[i]
                line = create_yolo_seg_annotation(polygon, class_id)

            lines.append(line)

        # Write annotations to file
        with open(output_path, "w") as f:
            f.write("\n".join(lines))

    print(f"✓ Created {len(results)} images and labels in {output_dir}")


def create_yolo_video_output(
    annotation_type: str,
    results: List[Any],
    output_dir: Path,
    video_path: str,
    stride: int,
) -> None:
    """Export annotations in YOLO format for videos with frame extraction."""
    # Create subdirectories for images and labels
    images_dir = output_dir / "images"
    labels_dir = output_dir / "labels"
    images_dir.mkdir(exist_ok=True, parents=True)
    labels_dir.mkdir(exist_ok=True, parents=True)

    # Open video file
    cap = cv2.VideoCapture(video_path)
    if not cap.isOpened():
        raise RuntimeError(f"Failed to open video: {video_path}")

    video_name = Path(video_path).stem

    # Initialize counters
    frame_idx = 1 if stride > 1 else 0
    saved_idx = 0

    print(f"Processing video frames (stride={stride})...")

    while cap.isOpened():
        ret, frame = cap.read()
        if not ret:
            break

        # Process only frames according to stride
        if frame_idx % stride == 0:
            frame_base = f"{video_name}_frame_{frame_idx:06d}"

            # Save frame as image
            img_path = images_dir / f"{frame_base}.jpg"
            cv2.imwrite(str(img_path), frame)

            # Create corresponding label file
            label_path = labels_dir / f"{frame_base}.txt"

            # Get prediction result for this frame
            if saved_idx >= len(results):
                print(f"Warning: No result available for frame {frame_idx}")
                break

            result = results[saved_idx]
            lines = []

            # Process detections if available
            if result.boxes is not None:
                for i, (box, class_id) in enumerate(
                    zip(result.boxes.xyxyn, result.boxes.cls)
                ):
                    class_id = int(class_id)

                    if annotation_type == "bbox":
                        line = create_yolo_bbox_annotation(box, class_id)
                    else:  # segmentation
                        if result.masks is None or not hasattr(
                            result.masks, "xyn"
                        ):
                            continue
                        polygon = result.masks.xyn[i]
                        line = create_yolo_seg_annotation(polygon, class_id)

                    lines.append(line)

            # Write label file
            with open(label_path, "w") as f:
                f.write("\n".join(lines))

            saved_idx += 1

            if saved_idx % 10 == 0:
                print(f"  Processed {saved_idx} frames...")

        frame_idx += 1

    cap.release()
    print(f"✓ Created {saved_idx} frames and labels in {output_dir}")


def create_metadata(
    text_prompts: List[str], conf_threshold: float, model_path: str
) -> Dict[str, Any]:
    """Create metadata dictionary for COCO export."""
    model_path = Path(model_path)

    return {
        "description": "SAM3 semantic segmentation export",
        "model": "sam3.pt",
        "model_sha256": compute_file_hash(
            model_path) if model_path.exists() else "N/A",
        "prompts": text_prompts,
        "confidence_threshold": conf_threshold,
        "ultralytics_version": ultralytics.__version__,
    }


# -------- Main --------


def main():

    # Parse arguments
    args = parse_arguments()

    # Parse text prompts
    text_prompts = [prompt.strip() for prompt in args.prompts.split(",")]
    print(f"\nClass prompts: {text_prompts}")
    print(f"args.name_file: {args.name_file}")

    # Setup paths relative to script location
    folder = Path("data_files").resolve()
    print(f"folder: {folder}")
    os.system("ls -al data_files/")
    # Get all files in the data folder
    file_paths = [str(f) for f in folder.glob("*") if f.is_file()]
    print(f"file_paths: {file_paths}")

    if not file_paths:
        print("Error: No files found in data_files directory")
        return

    # Use folder path as source for Ultralytics
    source_path = str(folder)

    # Setup output directories
    outdir = Path(args.outdir)
    outdir.mkdir(exist_ok=True, parents=True)

    outputs_annotated = outdir / "outputs_annotated"
    outputs_annotated.mkdir(parents=True, exist_ok=True)

    output_formats = [fmt.strip() for fmt in args.outputs.split(",")]
    print(f"Output formats: {output_formats}")

    # Configure predictor overrides
    overrides = {
        "conf": args.conf,
        "show_conf": False,
        "task": "segment",
        "mode": "predict",
        "model": args.model,
        "half": True,  # Use FP16 for faster inference
        "save": True,
        "save_dir": str(outputs_annotated),
    }

    # Determine if input is video or image and initialize appropriate predictor
    if is_video(file_paths[0]):
        print("\nVideo input detected → using SAM3VideoSemanticPredictor")
        overrides["vid_stride"] = args.vid_stride
        predictor = SAM3VideoSemanticPredictor(overrides=overrides)
    else:
        print("\nImage input detected → using SAM3SemanticPredictor")
        predictor = SAM3SemanticPredictor(overrides=overrides)

    # Patch predictor to include custom class names
    original_postprocess = predictor.postprocess

    def patched_postprocess(preds, img, orig_imgs):
        """Patch postprocess to add custom class names to results."""
        results = original_postprocess(preds, img, orig_imgs)
        # Add class names to each result
        for r in results:
            r.names = {i: name for i, name in enumerate(text_prompts)}
        return results

    predictor.postprocess = patched_postprocess

    # Run predictions
    # print(f"\n Running prediction on {source_path}...")
    results = predictor(source=source_path, text=text_prompts, stream=False)

    if not results:
        raise RuntimeError("SAM3 returned no results")

    print(f"✓ Processed {len(results)} result(s)")

    # Create metadata
    metadata = create_metadata(text_prompts, args.conf, args.model)

    # Export in requested formats
    print("\n" + "=" * 60)
    print("EXPORTING RESULTS")
    print("=" * 60)

    if "coco" in output_formats:
        print("\n→ Converting to COCO format...")
        coco_output = create_coco_output(results, text_prompts, metadata)

        annotation_file = outdir / "annotations.json"
        with open(annotation_file, "w") as f:
            json.dump(coco_output, f, indent=4)

        print(f"  Saved: {annotation_file}")
        validate_coco_format(annotation_file)

    if "yolo_bbox" in output_formats:
        print("\n→ Exporting YOLO bbox annotations...")
        yolo_bbox_dir = outdir / "yolo_bbox"
        yolo_bbox_dir.mkdir(parents=True, exist_ok=True)

        if is_video(file_paths[0]):
            create_yolo_video_output(
                "bbox", results, yolo_bbox_dir, file_paths[0], args.vid_stride
            )
        else:
            create_yolo_output("bbox", results, yolo_bbox_dir)
            print(f"  Saved to: {yolo_bbox_dir}")

    if "yolo_seg" in output_formats:
        print("\n→ Exporting YOLO seg annotations...")
        yolo_seg_dir = outdir / "yolo_seg"
        yolo_seg_dir.mkdir(parents=True, exist_ok=True)

        if is_video(file_paths[0]):
            create_yolo_video_output(
                "seg", results, yolo_seg_dir, file_paths[0], args.vid_stride
            )
        else:
            create_yolo_output("seg", results, yolo_seg_dir)
            print(f"  Saved to: {yolo_seg_dir}")

    print("\n" + "=" * 60)
    print("✓ EXPORT COMPLETED SUCCESSFULLY!")
    print("=" * 10)


if __name__ == "__main__":
    main()
