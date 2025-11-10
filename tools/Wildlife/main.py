# changer le nom filename
# 
# ==============================
# Wildlife Detection & Classification 
# ==============================

import os
import sys
import shutil
import cv2
from datetime import datetime
import numpy as np
import pandas as pd
import torch
import magic

from tqdm import tqdm
from pathlib import Path
from PIL import Image
from PytorchWildlife.models import detection as pw_detection
from supervision import ImageSink
from supervision.utils import video as video_utils
from transformers import AutoImageProcessor, AutoModelForImageClassification, pipeline
from functions import clean_dir, list_photos_videos, save_cropped_images
from collections import Counter



# -----------------------------
# CONFIGURATION
# -----------------------------
model_name=sys.argv[1].strip()  # Hugging Face model name
classifier_model = sys.argv[2]  # model perso
json_model = sys.argv[3]
type_mapping = sys.argv[4] # Id2Label or Label2Id
boxing_mode = sys.argv[5].strip()   # options: "no_image", "all_image"
path_input =sys.argv[6] # Images ou videos 
input_files = [Path(p.strip()) for p in path_input.split(',')]
print(f"Fichiers d'entree: {[str(f) for f in input_files]}")

detection_threshold = float(sys.argv[7]) # Minimum detection score
stride = int(sys.argv[8]) # Frame extraction interval for videos
images_max = int(sys.argv[9]) 

run_dir = sys.argv[10].strip()
predictions_dir = Path(run_dir) 
os.makedirs(predictions_dir, exist_ok=True)

boxed_images_dir = predictions_dir/"boxed_images/"
os.makedirs(boxed_images_dir, exist_ok=True)

os.makedirs("classifier_model_dir", exist_ok=True)
os.system(f"cp {classifier_model} classifier_model_dir/model.safetensors")
os.system(f"cp {json_model} classifier_model_dir/config.json")

extensions_photos = ('.jpg', '.JPG', '.jpeg', '.JPEG', '.png', '.PNG')
extensions_videos = ('.avi', '.AVI', '.mov', '.MOV', '.mp4', '.MP4',".dat")
name_file = sys.argv[11]
print(f"NAME FILE {name_file}")

# -----------------------------
# INITIALIZE MODELS
# -----------------------------
device = "cuda" if torch.cuda.is_available() else "cpu"
detection_model = pw_detection.MegaDetectorV5(device=device, pretrained=True)

image_processor = AutoImageProcessor.from_pretrained(model_name)
classifier = AutoModelForImageClassification.from_pretrained("./classifier_model_dir")
pipe = pipeline("image-classification", model=classifier, image_processor=image_processor, device=device)

if type_mapping.lower() == "id2label":
    labels = classifier.config.id2label
else:
    labels = classifier.config.label2id

taxons = list(labels.values()) if type_mapping.lower() == "id2label" else list(labels.keys())
taxons_count = len(taxons)
taxons_all = taxons + ["human", "vehicle"]

print(f"Boxing mode actif : {boxing_mode}")


# -----------------------------
# DETECTION AND CLASSIFICATION
# -----------------------------
def predict_images(images_dir, detections_dir, predictions, boxing_mode):
    """
    Performs detection and classification on images.
    """
    print(f"Detecting... (boxing_mode={boxing_mode})")

    # --- Step 1: Run object detection ---
    detections_list = detection_model.batch_image_detection(
        data_path=images_dir,
        batch_size=16, #32
        det_conf_thres=detection_threshold
    )

    detections_dict = save_cropped_images(
        detections=detections_list,
        detections_dir=detections_dir,
        boxing_mode=boxing_mode
    )

    # --- Step 2: Build list of images to classify ---
    if boxing_mode == "no_image":
        detections_images = list(detections_dict.keys())
    else:
        detections_images = list_photos_videos(detections_dir, extensions_photos + extensions_videos)
    
    # --- Step 3: Classification loop ---
    for detection in tqdm(detections_images, desc="Classifying...", unit="step", colour="yellow"):
        detection_info = detections_dict[detection]
        detection_class = detection_info[0]
        detection_score = detection_info[1]
        xyxy = detection_info[2]
        filename = os.path.basename(detection)
        frame = None

        for part in filename.split("_"):
            if part.startswith("F"):
                try:
                    frame = int(part[1:])
                    break
                except ValueError:
                    continue
        if frame is None:
            frame = 0

        # filepath reste juste le filename pour simplifier
        filepath = filename
        print(f"Traitement de la détection {detection} de classe {detection_class} avec score {detection_score}, xyxy {xyxy}")

        # --- Classification depending on detection type ---
        if detection_class == 1:
            detection_class_str = "human"
            classification_scores = [0] * taxons_count + [1, 0]
        elif detection_class == 2:
            detection_class_str = "vehicle"
            classification_scores = [0] * taxons_count + [0, 1]
        elif detection_class == 0:
            detection_class_str = "animal"

        if boxing_mode == "no_image":
            detection_image = detection_info[3]
        else:
            detection_image = Image.open(os.path.join(detections_dir, detection))

        # --- Run classification model ---
        inputs = image_processor(images=detection_image, return_tensors="pt").to(device)
        logits = pipe.model(**inputs).logits
        classification_scores = torch.nn.functional.softmax(logits, dim=-1)
        classification_scores = classification_scores.cpu().tolist()[0] + [0, 0]

        # --- Save the prediction row ---
        prediction = pd.DataFrame(
            [[filepath, filename, frame] +
             list(detection_score * np.array(classification_scores))],
            columns=list(predictions.columns)
        )
        predictions = pd.concat([predictions, prediction])

    # --- Step 4: Cleanup ---
    if boxing_mode == "all_image":
        clean_dir(images_dir)
        clean_dir(detections_dir)

    return predictions


# -----------------------------
# PREPARE TEMP DIRECTORIES
# -----------------------------
images_dir = Path("./temp/images")
detections_dir = Path("./temp/detections")
clean_dir(images_dir)
clean_dir(detections_dir)

timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
predictions = pd.DataFrame(
    columns=["Filepath", "Filename", "Frame"] + taxons_all
)


# -----------------------------
# PROCESS FILES DIRECTLY
# -----------------------------
print(f"USING DETECTION THRESHOLD {detection_threshold} AND STRIDE {stride}")

images_count = 0

for filepath in tqdm(input_files, desc="Extracting frames...", unit="file", colour="green"):
    if not filepath.exists():
        print(f"WARNING: File not found: {filepath}")
        continue
    image_counter = 1
    video_counter = 1

    filename = filepath.name
    print(f"FILE {filename}, FILEPATH {filepath}")

    if images_count > images_max:
        predictions = predict_images(images_dir, detections_dir, predictions, boxing_mode=boxing_mode)
        images_count = 0

    mime = magic.from_file(str(filepath), mime=True)
    if mime.startswith("image"):
        frame_idx = 0
        shutil.copy(str(filepath), os.path.join(images_dir, f"F{frame_idx}_{filename}.JPG"))
        images_count += 1
    else:
        video = cv2.VideoCapture(str(filepath))
        fps = video.get(cv2.CAP_PROP_FPS)

        for idx, frame in enumerate(video_utils.get_video_frames_generator(
            source_path=str(filepath), stride=int(stride * fps)
        )):
            frame_idx = idx + 1
            ImageSink(target_dir_path=images_dir, overwrite=False).save_image(
                image=frame, image_name=f"F{frame_idx}_{filename}.JPG"
            )
            images_count += 1

# Traiter les images restantes
if images_count > 0:
    predictions = predict_images(images_dir, detections_dir, predictions, boxing_mode=boxing_mode)


# -----------------------------
# POST-PROCESSING & OUTPUT
# -----------------------------
predictions["Prediction"] = predictions[taxons_all].apply(
    lambda x: "blank" if sum(x) == 0 else taxons_all[np.argmax(x)], axis=1
)
predictions["Confidence score"] = predictions[taxons_all].apply(
    lambda x: 0 if sum(x) == 0 else np.max(x), axis=1
)

# --- Nettoyage du Filename uniquement pour le CSV ---
def clean_filename(name):
    import re
    base = Path(name).name
    base = re.sub(r"^\d+_\d+_F\d+_", "", base)
    base = re.sub(r"^F\d+_", "", base)
    base = re.sub(r"\.JPG$", "", base, flags=re.IGNORECASE)
    base = re.sub(r"\.jpg\.JPG$", ".jpg", base, flags=re.IGNORECASE)
    return base

# CSV principal avec prédictions détaillées
predictions_csv = predictions.copy()
predictions_csv["Filename"] = predictions_csv["Filename"].apply(clean_filename)

# Tri par Filename puis par Frame (en supposant qu'il y a une colonne 'Frame')
if "Frame" in predictions_csv.columns:
    predictions_csv = predictions_csv.sort_values(by=["Filename", "Frame"]).reset_index(drop=True)
else:
    predictions_csv = predictions_csv.sort_values(by="Filename").reset_index(drop=True)

# Retrait de la colonne Filepath
predictions_csv = predictions_csv.drop(columns=["Filepath"])

output_file_grouped = predictions_dir / "output_predictions.csv"
predictions_csv.to_csv(output_file_grouped, index=False)
print(f"Grouped predictions saved: {output_file_grouped}")


# ============================================================
# SECOND CSV: "recap" VERSION (compact résumé par vidéo)
# ============================================================

predictions["Prediction"] = predictions[taxons_all].apply(
    lambda x: "blank" if sum(x) == 0 else taxons_all[np.argmax(x)], axis=1
)
predictions["Confidence score"] = predictions[taxons_all].apply(
    lambda x: 0 if sum(x) == 0 else np.max(x), axis=1
)

predictions['Filename'] = predictions['Filename'].apply(clean_filename)

# Filtrer les détections valides
predictions_valid = predictions[predictions["Prediction"] != "blank"].copy()

def sort_frames(frames_list):
    # Convertir chaque élément en int
    return sorted([int(f) for f in frames_list])


# Regrouper par fichier et espèce
recap_rows = []

for (filename, species), df_group in predictions_valid.groupby(["Filename", "Prediction"]):
    # Trier les frames du groupe numériquement
    frames_sorted = sort_frames(df_group["Frame"].tolist())

    
    # Compter le nombre d'occurrences de chaque frame et prendre la valeur la plus fréquente
    frame_counts = Counter(df_group["Frame"])
    count = frame_counts.most_common(1)[0][1]

    conf_mean = df_group["Confidence score"].mean()
    conf_min = df_group["Confidence score"].min()
    conf_max = df_group["Confidence score"].max()

    recap_rows.append({
        "Filename": filename,
        "Species": species,
        "Frames": frames_sorted,
        "Count": count,
        "Confidence mean": round(conf_mean, 4),
        "Confidence min": round(conf_min, 4),
        "Confidence max": round(conf_max, 4),
    })

predictions_recap = pd.DataFrame(recap_rows)

# Tri final par Filename (et par Species si nécessaire)
predictions_recap = predictions_recap.sort_values(by=["Filename", "Species"]).reset_index(drop=True)

output_file_recap = predictions_dir / "output_predictions_recap.csv"
predictions_recap.to_csv(output_file_recap, index=False)
print(f"Recap predictions saved: {output_file_recap}")

