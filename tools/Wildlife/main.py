# ==============================
# Wildlife Detection & Classification (clean version)
# ==============================

import os
import shutil
import sys
from collections import Counter
from datetime import datetime
from pathlib import Path

import cv2
import magic
import numpy as np
import pandas as pd
import torch
from PIL import Image
from PytorchWildlife.models import detection as pw_detection
from supervision import ImageSink
from supervision.utils import video as video_utils
from tqdm import tqdm
from transformers import AutoImageProcessor, AutoModelForImageClassification, pipeline

from functions import clean_dir, list_photos_videos, save_cropped_images


# -----------------------------
# CONFIGURATION
# -----------------------------
model_name = sys.argv[1].strip()  # Hugging Face model name
classifier_model = sys.argv[2]
json_model = sys.argv[3]
type_mapping = sys.argv[4]  # Id2Label or Label2Id
boxing_mode = sys.argv[5].strip()  # "no_image" ou "all_image"
path_input = sys.argv[6]
detection_threshold = float(sys.argv[7])
stride = int(sys.argv[8])
images_max = int(sys.argv[9])
run_dir = sys.argv[10].strip()

# Liste de noms à donner aux fichiers
# name_file = [n.strip() for n in sys.argv[11:]]
name_file = ["lynx.png", "video.mp4"]

predictions_dir = Path(run_dir)
predictions_dir.mkdir(parents=True, exist_ok=True)
boxed_images_dir = predictions_dir / "boxed_images"
boxed_images_dir.mkdir(exist_ok=True)

os.makedirs("classifier_model_dir", exist_ok=True)
os.system(f"cp {classifier_model} classifier_model_dir/model.safetensors")
os.system(f"cp {json_model} classifier_model_dir/config.json")

extensions_photos = (".jpg", ".jpeg", ".png", ".JPG", ".JPEG", ".PNG")
extensions_videos = (".avi", ".mov", ".mp4", ".AVI", ".MOV", ".MP4", ".dat")

print(f"NAME FILE: {name_file}")
print(f"PATH INPUT: {path_input}")

input_files = [Path(p.strip()) for p in path_input.split(",")]
print(f"Fichiers d'entrée: {[str(f) for f in input_files]}")

if len(input_files) != len(name_file):
    raise ValueError("Le nombre de fichiers d'entrée ne correspond pas au nombre de noms fournis")

# Associer chemin d’entrée au nom personnalisé
file_to_name = {str(f): n for f, n in zip(input_files, name_file)}


# -----------------------------
# INITIALIZE MODELS
# -----------------------------
device = "cuda" if torch.cuda.is_available() else "cpu"
detection_model = pw_detection.MegaDetectorV5(device=device, pretrained=True)

image_processor = AutoImageProcessor.from_pretrained(model_name)
classifier = AutoModelForImageClassification.from_pretrained("./classifier_model_dir")

pipe = pipeline(
    "image-classification",
    model=classifier,
    image_processor=image_processor,
    device=device,
)

labels = classifier.config.id2label if type_mapping.lower() == "id2label" else classifier.config.label2id
taxons = list(labels.values()) if type_mapping.lower() == "id2label" else list(labels.keys())
taxons_all = taxons + ["human", "vehicle"]

print(f"Boxing mode actif : {boxing_mode}")


# -----------------------------
# DETECTION + CLASSIFICATION
# -----------------------------
def predict_images(images_dir, detections_dir, predictions, boxing_mode):
    """Détection et classification sur images"""
    print(f"Détection... (boxing_mode={boxing_mode})")

    detections_list = detection_model.batch_image_detection(
        data_path=images_dir, batch_size=16, det_conf_thres=detection_threshold
    )

    detections_dict = save_cropped_images(
        detections=detections_list, detections_dir=detections_dir, boxing_mode=boxing_mode
    )

    if boxing_mode == "no_image":
        detections_images = list(detections_dict.keys())
    else:
        detections_images = list_photos_videos(detections_dir, extensions_photos + extensions_videos)

    for detection in tqdm(detections_images, desc="Classifying...", unit="step", colour="yellow"):
        info = detections_dict[detection]
        det_class, det_score, xyxy = info[:3]
        filename = os.path.basename(detection)

        # Trouver le frame number
        frame = next((int(p[1:]) for p in filename.split("_") if p.startswith("F") and p[1:].isdigit()), 0)
        filepath = filename

        if det_class == 1:
            scores = [0]*len(taxons) + [1, 0]
        elif det_class == 2:
            scores = [0]*len(taxons) + [0, 1]
        else:
            image = info[3] if boxing_mode == "no_image" else Image.open(os.path.join(detections_dir, detection))
            inputs = image_processor(images=image, return_tensors="pt").to(device)
            logits = pipe.model(**inputs).logits
            scores = torch.nn.functional.softmax(logits, dim=-1).cpu().tolist()[0] + [0, 0]

        prediction_row = pd.DataFrame(
            [[filepath, filename, frame] + list(det_score * np.array(scores))],
            columns=list(predictions.columns),
        )
        predictions = pd.concat([predictions, prediction_row])

    if boxing_mode == "all_image":
        clean_dir(images_dir)
        clean_dir(detections_dir)

    return predictions


# -----------------------------
# TEMP DIRS
# -----------------------------
images_dir = Path("./temp/images")
detections_dir = Path("./temp/detections")
clean_dir(images_dir)
clean_dir(detections_dir)

timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
predictions = pd.DataFrame(columns=["Filepath", "Filename", "Frame"] + taxons_all)

print(f"USING DETECTION THRESHOLD {detection_threshold} AND STRIDE {stride}")


# -----------------------------
# EXTRACTION FRAMES & TRAITEMENT
# -----------------------------
images_count = 0

for filepath in tqdm(input_files, desc="Extracting frames...", unit="file", colour="green"):
    if not filepath.exists():
        print(f"WARNING: File not found: {filepath}")
        continue

    filename = filepath.name
    print(f"FILE: {filename}")

    if images_count > images_max:
        predictions = predict_images(images_dir, detections_dir, predictions, boxing_mode)
        images_count = 0

    mime = magic.from_file(str(filepath), mime=True)

    if mime.startswith("image"):
        shutil.copy(str(filepath), images_dir / f"F0_{filename}.JPG")
        images_count += 1
    else:
        video = cv2.VideoCapture(str(filepath))
        fps = video.get(cv2.CAP_PROP_FPS)

        for idx, frame in enumerate(
            video_utils.get_video_frames_generator(source_path=str(filepath), stride=int(stride * fps))
        ):
            ImageSink(images_dir, overwrite=False).save_image(image=frame, image_name=f"F{idx+1}_{filename}.JPG")
            images_count += 1

if images_count > 0:
    predictions = predict_images(images_dir, detections_dir, predictions, boxing_mode)

# -----------------------------
# POST-PROCESSING & CSV EXPORT
# -----------------------------
predictions["Prediction"] = predictions[taxons_all].apply(
    lambda x: "blank" if sum(x) == 0 else taxons_all[np.argmax(x)], axis=1
)
predictions["Confidence score"] = predictions[taxons_all].apply(
    lambda x: 0 if sum(x) == 0 else np.max(x), axis=1
)

# Associer chaque Filepath au nom_file correspondant
def map_to_namefile(filepath):
    for input_path, custom_name in file_to_name.items():
        if Path(input_path).name in Path(filepath).name:
            return custom_name
    return Path(filepath).stem  # fallback

# Ajoute une colonne Filename basée sur name_file
predictions["Filename"] = predictions["Filepath"].apply(map_to_namefile)

# Tri par Filename et Frame
if "Frame" in predictions.columns:
    predictions = predictions.sort_values(by=["Filename", "Frame"]).reset_index(drop=True)
else:
    predictions = predictions.sort_values(by="Filename").reset_index(drop=True)

# Supprimer la colonne Filepath avant export
predictions_csv = predictions.drop(columns=["Filepath"])

# Sauvegarde du CSV détaillé
output_file_grouped = predictions_dir / "output_predictions.csv"
predictions_csv.to_csv(output_file_grouped, index=False)
print(f"Grouped predictions saved: {output_file_grouped}")

# ============================================================
# CSV RECAP : résumé par fichier (toujours Filename = name_file)
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

recap_df = pd.DataFrame(recap_rows).sort_values(by=["Filename", "Species"]).reset_index(drop=True)
output_file_recap = predictions_dir / "output_predictions_recap.csv"
recap_df.to_csv(output_file_recap, index=False)
print(f"Recap predictions saved: {output_file_recap}")
