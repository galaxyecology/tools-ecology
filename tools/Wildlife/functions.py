import os
import shutil
from pathlib import Path

from PIL import Image

import cv2

import numpy as np

from supervision import ImageSink, crop_image


def list_photos_videos(dir_path, extensions):
    """
    List all photo and video files within a directory.

    Args:
        dir_path (str): Path to the directory containing photos and videos.
        extensions (tuple): Allowed file extensions (e.g., ('.jpg', '.mp4')).

    Returns:
        list: List of filenames matching the given extensions.
    """
    photos_videos = []
    for f in os.listdir(dir_path):
        if (
            os.path.isfile(os.path.join(dir_path, f))
            and not f.startswith(".")
            and f.endswith(extensions)
        ):
            photos_videos.append(f)
    return photos_videos


def clean_dir(dir_path):
    """
    Remove and recreate a directory.

    Args:
        dir_path (str): Path to the directory to reset.

    Note:
      This function deletes all existing content before recreating the folder.
    """
    if os.path.isdir(dir_path):
        shutil.rmtree(dir_path)
    os.makedirs(dir_path, exist_ok=True)


def save_cropped_images(detections, detections_dir, boxing_mode):
    """
    Save cropped images from detections and optionally images with 
    bounding boxes.

    Args:
        detections (list): List of detection entries.
            Each entry is expected to be a dict with:
                - "img_id" (str): path to the source image
                - "detections" (list): list of tuples containing:
                    (xyxy, _, detection_score, detection_class, _, _)
        detections_dir (str): Directory to save cropped images.
        boxing_mode (str): One of:
            - "no_image": keep cropped images in memory only
            - "all_image": save cropped and boxed images to disk

    Returns:
        dict: A dictionary with cropped image information.
              Keys are image filenames, values contain:
              [class_name, score, bounding_box, (optional) PIL.Image object]
    """
    detections_dict = {}

    # --- Case 1: no_image → Only keep cropped images in memory ---
    if boxing_mode == "no_image":
        for entry in detections:
            img = np.array(Image.open(entry["img_id"]).convert("RGB"))
            for i, (
                xyxy, _, detection_score, detection_class, _, _
            ) in enumerate(entry["detections"]):
                image_cropped = crop_image(img, xyxy)
                image_name = (
                    f"{detection_class}_{i}_"
                    f"{os.path.basename(entry['img_id'])}"
                )
                detections_dict[image_name] = [
                    detection_class,
                    detection_score,
                    list(map(float, xyxy)),
                    Image.fromarray(image_cropped),
                ]
        return detections_dict

    # --- Case 2: all_image → Save cropped and boxed images ---
    boxed_dir = Path("outputs/boxed_images")
    boxed_dir.mkdir(parents=True, exist_ok=True)
    detections_dir = Path(detections_dir)
    detections_dir.mkdir(parents=True, exist_ok=True)

    with ImageSink(target_dir_path=detections_dir, overwrite=False) as sink:
        for entry in detections:
            img_path = entry["img_id"]
            img = np.array(Image.open(img_path).convert("RGB"))
            img_boxed = img.copy()

            for i, (
                xyxy, _, detection_score, detection_class, _, _
                ) in enumerate(entry["detections"]):
                # --- Crop and save the detection ---
                image_cropped = crop_image(img, xyxy)
                image_name = (
                    f"{detection_class}_{i}_"
                    f"{os.path.basename(img_path)}"
                )
                sink.save_image(
                    cv2.cvtColor(image_cropped, cv2.COLOR_RGB2BGR)
                    , image_name
                )
                detections_dict[image_name] = [
                    detection_class,
                    detection_score,
                    list(map(float, xyxy)),
                ]

                # --- Draw bounding boxes on the image ---
                x1, y1, x2, y2 = map(int, xyxy)
                color = (0, 255, 0)
                cv2.rectangle(img_boxed, (x1, y1), (x2, y2), color, 2)
                cv2.putText(
                    img_boxed,
                    f"{detection_class} {detection_score:.2f}",
                    (x1, max(y1 - 10, 10)),
                    cv2.FONT_HERSHEY_SIMPLEX,
                    0.6,
                    color,
                    2,
                )

            # --- Save the multi-box image ---
            boxed_path = boxed_dir / f"boxed_{os.path.basename(img_path)}"
            cv2.imwrite(
                str(boxed_path),
                cv2.cvtColor(img_boxed, cv2.COLOR_RGB2BGR),
            )
    return detections_dict
