# Les fuchiers n'ont pas le meme path donct je dois pas prendre leur dir masi diretment le path 
# On se trouve le dossier de sortie 
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
model_name=sys.argv[1] # Hugging Face model name
classifier_model = sys.argv[2] # model perso
json_model = sys.argv[3]
type_mapping = sys.argv[4] # Id2Label or Label2Id
boxing_mode = sys.argv[5]  # options: "no_image", "all_image"
path_input =sys.argv[6] # Images ou videos 
path_list = path_input.split(',') 
data_dir = str(Path(path_list[0]).parent)
print(f"DATA_dir {path_input} ET {data_dir} ")

detection_threshold = float(sys.argv[7]) # Minimum detection score
stride = int(sys.argv[8]) # Frame extraction interval for videos
images_max = int(sys.argv[9]) 

run_dir = sys.argv[10]
predictions_dir = Path(run_dir) 
os.makedirs(predictions_dir, exist_ok=True)

boxed_images_dir = predictions_dir / "boxed_images"
os.makedirs(boxed_images_dir, exist_ok=True)

os.makedirs("classifier_model_dir", exist_ok=True)
os.system(f"cp {classifier_model} classifier_model_dir/model.safetensors")
os.system(f"cp {json_model} classifier_model_dir/config.json")

extensions_photos = ('.jpg', '.JPG', '.jpeg', '.JPEG', '.png', '.PNG')
extensions_videos = ('.avi', '.AVI', '.mov', '.MOV', '.mp4', '.MP4',".dat")

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
def predict_images(images_dir, detections_dir, data_dir, predictions, boxing_mode):
    """
    Performs detection and classification on images.
    Automatically handles saving behavior depending on boxing_mode:
      - "no_image"  → no cropped images saved, but full CSV is produced
      - "all_image" → keeps all detections
    """
    print(f"Detecting... (boxing_mode={boxing_mode})")

    # --- Step 1: Run object detection ---
    detections_list = detection_model.batch_image_detection(
        data_path=images_dir,
        batch_size=16,
        det_conf_thres=detection_threshold
    )

    detections_dict = save_cropped_images(
        detections=detections_list,
        detections_dir=detections_dir,
        boxing_mode=boxing_mode
    )

    # --- Step 2: Build list of images to classify ---
    if boxing_mode == "no_image":
        detections_images = list(detections_dict.keys())  # in-memory images
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

        filepath = os.path.join(data_dir, filename)
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
            detection_image = detection_info[3]  # déjà en mémoire
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
filepaths_all = []


# -----------------------------
# LOOP THROUGH DATA
# -----------------------------
print(f"USING DETECTION THRESHOLD {detection_threshold} AND STRIDE {stride}")
data_subdirs = [x[0] for x in os.walk(data_dir)]

for data_subdir in data_subdirs:
    print(f"PROCESSING FOLDER {data_subdir}")
    images_count = 0
    files = list_photos_videos(data_subdir, extensions_photos + extensions_videos)

    for file in tqdm(files, desc="Extracting frames...", unit="step", colour="green"):
        filepath = os.path.join(data_subdir, file)
        print(f"FILE {file}, FILEPATH {filepath}")
        filepaths_all.append(filepath)

        if images_count > images_max:
            predictions = predict_images(images_dir, detections_dir, data_subdir, predictions, boxing_mode=boxing_mode)
            images_count = 0

        mime = magic.from_file(filepath, mime=True)
        if mime.startswith("image"):
            frame_idx = 0
            shutil.copy(filepath, os.path.join(images_dir, f"F{frame_idx}_{file}.JPG"))
            images_count += 1
        else:
            video = cv2.VideoCapture(filepath)
            fps = video.get(cv2.CAP_PROP_FPS)

            for idx, frame in enumerate(video_utils.get_video_frames_generator(
                source_path=filepath, stride=int(stride * fps)
            )):
                frame_idx = idx + 1  
                ImageSink(target_dir_path=images_dir, overwrite=False).save_image(
                    image=frame, image_name=f"F{frame_idx}_{file}.JPG"
                )
                images_count += 1

    predictions = predict_images(images_dir, detections_dir, data_subdir, predictions, boxing_mode=boxing_mode)
    images_count = 0


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
    # enlever préfixes type 0_0_F12_
    base = re.sub(r"^\d+_\d+_F\d+_", "", base)
    # enlever préfixes type F12_
    base = re.sub(r"^F\d+_", "", base)
    # enlever double extensions .JPG ou .jpg.JPG
    base = re.sub(r"\.JPG$", "", base, flags=re.IGNORECASE)
    base = re.sub(r"\.jpg\.JPG$", ".jpg", base, flags=re.IGNORECASE)
    return base

# On applique uniquement pour le CSV, pas pour le reste du script
predictions_csv = predictions.copy()
predictions_csv["Filename"] = predictions_csv["Filename"].apply(clean_filename)
predictions_csv = predictions_csv.drop(columns=["Filepath"])
predictions_csv = predictions_csv.sort_values(by="Filename").reset_index(drop=True)

# Sauvegarde du CSV nettoyé
output_file_grouped = predictions_dir / "output_predictions.csv"
predictions_csv.to_csv(output_file_grouped, index=False)
print(f"Grouped predictions saved: {output_file_grouped}")

# ============================================================
# SECOND CSV: "recap" VERSION (compact résumé par vidéo)
# ============================================================

# Recalcul des colonnes de prédiction et confiance
predictions["Prediction"] = predictions[taxons_all].apply(
    lambda x: "blank" if sum(x) == 0 else taxons_all[np.argmax(x)], axis=1
)
predictions["Confidence score"] = predictions[taxons_all].apply(
    lambda x: 0 if sum(x) == 0 else np.max(x), axis=1
)

predictions['Filename'] = predictions['Filename'].apply(clean_filename)


# Filtrer les détections valides (pas "blank")
predictions_valid = predictions[predictions["Prediction"] != "blank"].copy()

# Regrouper par fichier (Filename) et espèce (Prediction)
recap_rows = []
for (filename, species), df_group in predictions_valid.groupby(["Filename", "Prediction"]):
    frames = df_group["Frame"].tolist()  # garder les répétitions
    # Count = nombre de fois que la frame la plus fréquente apparaît
    frame_counts = Counter(frames)
    count = frame_counts.most_common(1)[0][1]

    conf_mean = df_group["Confidence score"].mean()
    conf_min = df_group["Confidence score"].min()
    conf_max = df_group["Confidence score"].max()

    recap_rows.append({
        "Filename": filename,
        "Species": species,
        "Frames": ",".join(map(str, frames)),  # garder toutes les répétitions
        "Count": count,
        "Confidence mean": round(conf_mean, 4),
        "Confidence min": round(conf_min, 4),
        "Confidence max": round(conf_max, 4),
    })

predictions_recap = pd.DataFrame(recap_rows)
predictions_recap = predictions_recap.sort_values(by="Filename").reset_index(drop=True)

# Sauvegarde finale
output_file_recap = predictions_dir / "output_predictions_recap.csv"
predictions_recap.to_csv(output_file_recap, index=False)
print(f"Recap predictions saved: {output_file_recap}")



