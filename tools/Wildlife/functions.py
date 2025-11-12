import os
import shutil
from pathlib import Path

import cv2
import numpy as np
from PIL import Image
from supervision import ImageSink, crop_image


def list_photos_videos(dir_path, extensions):
    """
    Lists all photos and videos within a directory.
    Args:
        dir_path (str): Path of the directory containing the photos and videos.
        extensions (list): List of allowed photo and video extensions.
    Output:
        photos_videos (list): List of all filenames of all photos and videos 
        within the directory.
    """
    photos_videos = []
    for f in os.listdir(dir_path):
        if (
            os.path.isfile(os.path.join(dir_path, f))
            and not f.startswith(".")
            and f.endswith(extensions)
        ):
            photos_videos += [f]
    return photos_videos


def clean_dir(dir_path):
    """
    Erases directory if it exists and creates a new empty one.
    Arg:
        dir_path (str): Path of the directory to clean.
    """
    if os.path.isdir(dir_path):
        shutil.rmtree(dir_path)
    os.makedirs(dir_path)


def save_cropped_images(detections, detections_dir, boxing_mode):
    """
    Sauvegarde les images croppées et, selon boxing_mode, 
                        les images avec bounding boxes.
    - boxing_mode = "no_image"   → ne sauvegarde rien
    - boxing_mode = "all_image"  → sauvegarde toutes les détections et affiche
                                        toutes les boxes sur l'image originale
    """
    detections_dict = {}

    # --- Cas "no_image" : garder juste en mémoire ---
    if boxing_mode == "no_image":
        for entry in detections:
            img = np.array(Image.open(entry["img_id"]).convert("RGB"))
            for i, (xyxy, _, detection_score, detection_class, _, _) \
            in enumerate(entry["detections"]):
                image_cropped = crop_image(img, xyxy)
                image_name = (
                  f"{detection_class}_{i}_{os.path.basename(entry['img_id'])}"
                )
                detections_dict[image_name] = [
                    detection_class,
                    detection_score,
                    list(map(float, xyxy)),
                    Image.fromarray(image_cropped),
                ]
        return detections_dict

    # --- Cas "all_image" ---
    boxed_dir = Path("outputs/boxed_images")
    boxed_dir.mkdir(parents=True, exist_ok=True)
    detections_dir = Path(detections_dir)
    detections_dir.mkdir(parents=True, exist_ok=True)

    with ImageSink(target_dir_path=detections_dir, overwrite=False) as sink:
        for entry in detections:
            img_path = entry["img_id"]
            img = np.array(Image.open(img_path).convert("RGB"))
            img_boxed = img.copy()  # image pour multi-box visuel

            for i, (xyxy, _, detection_score, detection_class, _, _) \
            in enumerate(entry["detections"]):
                # --- Crop et sauvegarde ---
                image_cropped = crop_image(img, xyxy)
                image_name = \
                    f"{detection_class}_{i}_{os.path.basename(img_path)}"
                sink.save_image(
                    cv2.cvtColor(image_cropped, cv2.COLOR_RGB2BGR), image_name
                )
                detections_dict[image_name] = [
                    detection_class,
                    detection_score,
                    list(map(float, xyxy)),
                ]

                # --- Dessiner la bbox sur l'image multi-box ---
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

            # --- Sauvegarder l'image multi-box ---
            boxed_path = boxed_dir / f"boxed_{os.path.basename(img_path)}"
            cv2.imwrite(str(boxed_path),\
                cv2.cvtColor(img_boxed, cv2.COLOR_RGB2BGR))

    return detections_dict
