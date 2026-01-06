# ==============================
# Wildlife Detection & Classification
# ==============================

import argparse
import os
import shutil
from datetime import datetime
from pathlib import Path

from PIL import Image

from PytorchWildlife.models import detection as pw_detection

import cv2

from functions import clean_dir, list_photos_videos, save_cropped_images

import magic

import numpy as np

import pandas as pd

from supervision import ImageSink
from supervision.utils import video as video_utils

import torch

from tqdm import tqdm

from transformers import (
    AutoImageProcessor,
    AutoModelForImageClassification,
    pipeline,
)

# ============================================================
# CONFIGURATION
# ============================================================

parser = argparse.ArgumentParser(
    description="Wildlife Detection & Classification using MegaDetector"
    "+ Hugging Face model"
)

parser.add_argument("model_name", type=str,
                    help="Hugging Face model name for feature extraction")
parser.add_argument(
    "classifier_model", type=str,
    help="Path to fine-tuned classifier weights (.safetensors)"
)
parser.add_argument("json_model", type=str,
                    help="Path to classifier config (.json)")
parser.add_argument(
    "type_mapping", type=str, choices=[" id2label", " label2id"],
    help="Label mapping direction"
)
parser.add_argument(
    "boxing_mode", type=str, choices=[" no_image", " all_image"],
    help="Bounding box output mode"
)
parser.add_argument(
    "path_input", type=str,
    help="Comma-separated list of input files (images or videos)"
    )
parser.add_argument("detection_threshold", type=float,
                    help="Detection confidence threshold (0-1)")
parser.add_argument("stride", type=int,
                    help="Frame extraction stride")
parser.add_argument("images_max", type=int,
                    help="Maximum number of images to process per folder")
parser.add_argument("run_dir", type=str,
                    help="Output directory for predictions")
parser.add_argument("name_file", nargs=argparse.REMAINDER,
                    help="Optional custom names for input files")

args = parser.parse_args()

# Assign variables
model_name = args.model_name.strip()
classifier_model = args.classifier_model
json_model = args.json_model
type_mapping = args.type_mapping.strip()
boxing_mode = args.boxing_mode.strip()
path_input = args.path_input
detection_threshold = args.detection_threshold
stride = args.stride
images_max = args.images_max
run_dir = args.run_dir.strip()
name_file = [n.strip() for n in args.name_file]

# Output directories
predictions_dir = Path(run_dir)
predictions_dir.mkdir(parents=True, exist_ok=True)

boxed_images_dir = predictions_dir / "boxed_images"
boxed_images_dir.mkdir(exist_ok=True)

os.makedirs("classifier_model_dir", exist_ok=True)
os.system(f"cp {classifier_model} classifier_model_dir/model.safetensors")
os.system(f"cp {json_model} classifier_model_dir/config.json")

extensions_photos = (".jpg", ".jpeg", ".png", ".JPG", ".JPEG", ".PNG")
extensions_videos = (".avi", ".mov", ".mp4", ".AVI", ".MOV", ".MP4", ".dat")

print(f"Loaded input names: {name_file}")

input_files = [Path(p.strip()) for p in path_input.split(",")]
file_to_name = {str(f): n for f, n in zip(input_files, name_file)}


# ============================================================
# MODEL INITIALIZATION
# ============================================================

device = "cuda" if torch.cuda.is_available() else "cpu"
print(f"Using device: {device}")

detection_model = pw_detection.MegaDetectorV5(
    device=device,
    pretrained=True,
)
image_processor = AutoImageProcessor.from_pretrained(model_name)
classifier = AutoModelForImageClassification.from_pretrained(
    "./classifier_model_dir",
    use_fast=True
)

pipe = pipeline(
    "image-classification",
    model=classifier,
    image_processor=image_processor,
    device=device,
)

# Label mapping
if type_mapping.lower() == "id2label":
    labels = classifier.config.id2label
    taxons = list(labels.values())
else:
    labels = classifier.config.label2id
    taxons = list(labels.keys())

taxons_all = taxons + ["human", "vehicle"]
print(f"Active boxing mode: {boxing_mode}")


# ============================================================
# DETECTION + CLASSIFICATION
# ============================================================

def predict_images(images_dir, detections_dir, predictions, boxing_mode):
    """
    Perform object detection and species classification on images.

    Args:
        images_dir (str): Directory containing extracted frames.
        detections_dir (str): Directory for cropped detections.
        predictions (pd.DataFrame): DataFrame to store predictions.
        boxing_mode (str): "no_image" or "all_image".

    Returns:
        pd.DataFrame: Updated predictions DataFrame.
    """
    print(f"Running detection... (boxing_mode={boxing_mode})")

    detections_list = detection_model.batch_image_detection(
        data_path=images_dir,
        batch_size=8,
        det_conf_thres=detection_threshold,
    )

    detections_dict = save_cropped_images(
        detections=detections_list,
        detections_dir=detections_dir,
        boxing_mode=boxing_mode,
    )

    if boxing_mode == "no_image":
        detections_images = list(detections_dict.keys())
    else:
        detections_images = list_photos_videos(
            detections_dir, extensions_photos + extensions_videos
        )

    # --- Classification ---
    for detection in tqdm(
        detections_images,
        desc="Classifying...",
        unit="image",
        colour="yellow",
    ):
        info = detections_dict[detection]
        det_class, det_score, xyxy = info[:3]

        filename = os.path.basename(detection)
        frame = next(
            (int(p[1:]) for p in filename.split("_")
             if p.startswith("F") and p[1:].isdigit()),
            0,
        )

        if det_class == 1:  # human
            scores = [0] * len(taxons) + [1, 0]
        elif det_class == 2:  # vehicle
            scores = [0] * len(taxons) + [0, 1]
        else:
            image = info[3] if boxing_mode == "no_image" else Image.open(
                os.path.join(detections_dir, detection)
            )
            inputs = image_processor(
                images=image,
                return_tensors="pt",
            ).to(device)
            logits = pipe.model(**inputs).logits
            softmax_scores = (
                torch.nn.functional.softmax(logits, dim=-1)
                .cpu()
                .tolist()[0]
            )
            scores = softmax_scores + [0, 0]

        prediction_row = pd.DataFrame(
            [[detection, filename, frame]
                + list(det_score * np.array(scores))],
            columns=list(predictions.columns),
        )

        predictions = pd.concat([predictions, prediction_row])

    if boxing_mode == "all_image":
        clean_dir(images_dir)
        clean_dir(detections_dir)

    return predictions


# ============================================================
# TEMPORARY DIRECTORIES
# ============================================================

images_dir = Path("./temp/images")
detections_dir = Path("./temp/detections")
clean_dir(images_dir)
clean_dir(detections_dir)

timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
predictions = pd.DataFrame(
    columns=["Filepath", "Filename", "Frame"] + taxons_all
)

print(f"Detection threshold: {detection_threshold} | Stride: {stride}")


# ============================================================
# FRAME EXTRACTION & PROCESSING
# ============================================================

images_count = 0

for filepath in tqdm(
    input_files,
    desc="Extracting frames...",
    unit="file",
    colour="green",
):
    if not filepath.exists():
        print(f"WARNING: file not found: {filepath}")
        continue

    filename = filepath.name
    print(f"Processing file: {filename}")

    if images_count > images_max:
        predictions = predict_images(
            images_dir,
            detections_dir,
            predictions,
            boxing_mode
        )
        images_count = 0

    mime = magic.from_file(str(filepath), mime=True)

    if mime.startswith("image"):
        shutil.copy(str(filepath), images_dir / f"F0_{filename}.JPG")
        images_count += 1

    else:
        video = cv2.VideoCapture(str(filepath))
        fps = video.get(cv2.CAP_PROP_FPS)

        for idx, frame in enumerate(
            video_utils.get_video_frames_generator(
                source_path=str(filepath),
                stride=int(stride * fps)
            )
        ):
            ImageSink(images_dir, overwrite=False).save_image(
                image=frame,
                image_name=f"F{idx+1}_{filename}.JPG"
            )
            images_count += 1

if images_count > 0:
    predictions = predict_images(
        images_dir,
        detections_dir,
        predictions,
        boxing_mode,
    )


# ============================================================
# POST-PROCESSING & CSV EXPORT
# ============================================================

# Compute top prediction and confidence per detection
predictions["Prediction"] = predictions[taxons_all].apply(
    lambda x: "blank" if sum(x) == 0 else taxons_all[np.argmax(x)],
    axis=1
)
predictions["Confidence score"] = predictions[taxons_all].apply(
    lambda x: 0 if sum(x) == 0 else np.max(x),
    axis=1
)


def map_to_namefile(filepath):
    for input_path, custom_name in file_to_name.items():
        if Path(input_path).name in Path(filepath).name:
            return custom_name
    return Path(filepath).stem  # Fallback


predictions["Filename"] = predictions["Filepath"].apply(map_to_namefile)

if "Frame" in predictions.columns:
    predictions = predictions.sort_values(
        by=["Filename", "Frame"]
    ).reset_index(drop=True)
else:
    predictions = predictions.sort_values(by="Filename").reset_index(drop=True)

predictions_csv = predictions.drop(columns=["Filepath"])

output_file_grouped = predictions_dir / "output_predictions.csv"
predictions_csv.to_csv(output_file_grouped, index=False)
print(f"Detailed predictions saved: {output_file_grouped}")

# ============================================================
# SUMMARY CSV (RECAP)
# ============================================================

valid = predictions[predictions["Prediction"] != "blank"].copy()

recap_rows = []
for (filename, species), df in valid.groupby(["Filename", "Prediction"]):
    frames_sorted = sorted(df["Frame"].tolist())
    recap_rows.append({
        "Filename": filename,
        "Species": species,
        "Frames": frames_sorted,
        "Count": len(df),
        "Confidence mean": round(df["Confidence score"].mean(), 4),
        "Confidence min": round(df["Confidence score"].min(), 4),
        "Confidence max": round(df["Confidence score"].max(), 4),
    })

recap_df = (
    pd.DataFrame(recap_rows)
    .sort_values(by=["Filename", "Species"])
    .reset_index(drop=True)
)
output_file_recap = predictions_dir / "output_predictions_recap.csv"
recap_df.to_csv(output_file_recap, index=False)

print(f"Summary predictions saved: {output_file_recap}")
