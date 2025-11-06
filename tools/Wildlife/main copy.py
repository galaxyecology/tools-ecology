# ==============================
# Wildlife Detection & Classification 
# ==============================

import argparse
import os
import shutil
import cv2
from datetime import datetime
import numpy as np
import pandas as pd
import torch
from tqdm import tqdm
from pathlib import Path
from PIL import Image
from PytorchWildlife.models import detection as pw_detection

import magic

from supervision import ImageSink
from supervision.utils import video as video_utils
from transformers import AutoImageProcessor, AutoModelForImageClassification, pipeline
from functions import clean_dir, list_photos_videos, save_cropped_images

import sys


#! python main.py <model_name> <classifier_model> <json_model> <type_mapping> <data_dir> <detection_threshold> <stride> <images_max>
model_name= 'facebook/dinov2-large' # Hugging Face model name
classifier_model = "galaxy_inputs/model/model.safetensors.data" # model perso
json_model = "galaxy_inputs/config/config.json"
type_mapping = "id2label" # Id2Label or Label2Id
path_input ="galaxy_inputs/testvideo/video.mp4" # Images ou videos 
path_list = path_input.split(',') 
data_dir = str(Path(path_list[0]).parent)
print(f"DATA_dir {path_input} ET {data_dir} ")

detection_threshold = 0.2 # Minimum detection score
stride = 4 # Frame extraction interval for videos
images_max = 3000

run_dir = "outputs/"
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
detection_model = pw_detection.MegaDetectorV5(device=device, pretrained=True) # MegaDetector v5 download by zenodo

# ? model_name = dinov2-large comment relié pour qu'il soit prit en compre 
image_processor = AutoImageProcessor.from_pretrained(model_name) # Processor = pre traitement pour le backbone

classifier = AutoModelForImageClassification.from_pretrained("./classifier_model_dir") # Backbone et Head donc obligatoire

pipe = pipeline("image-classification", model=classifier, image_processor=image_processor, device=device) 

if type_mapping.lower() == "id2label":
    labels = classifier.config.id2label
else :
    labels = classifier.config.label2id

taxons = list(labels.values()) if type_mapping.lower() == "id2label" else list(labels.keys())
taxons_count = len(taxons)
taxons_all = taxons + ["human", "vehicle"] #? Utiliter human et vehicule ? Puis il ya une faute a vehicule 


# -----------------------------
# DETECTION AND CLASSIFICATION FUNCTION
# -----------------------------

def predict_images(images_dir, detections_dir, data_dir, predictions):
    """
    Performs detection and classification on images and adds the results to the predictions dataframe.
    Args:
        images_dir (str):Path of the directory containing the images (either photos or frames extracted from videos).
        detections_dir (str): Path of the directory where to save the cropped detections.
        data_dir (str): Path of the data directory.
        predictions (dataframe): Dataframe containing previous predictions.
    Output:
        predictions (dataframe): Predictions dataframe updated with the new predictions.
    """
    
    print("Detecting...")
    
    detections_list = detection_model.batch_image_detection(data_path=images_dir, batch_size=32, det_conf_thres=detection_threshold) #! batch_size depend de la RAM et GPU dispo, plus c'est grand plus c'est rapide (nb d'image traité en meme temps)
    detections_dict = save_cropped_images(detections=detections_list, detections_dir=detections_dir)
    detections_images = list_photos_videos(detections_dir, extensions_photos + extensions_videos)

    for detection in tqdm(detections_images, desc="Classifying...", unit="step", colour="yellow"):
        filename = detection.split("_", 3)[3][:-4]
        filepath = os.path.join(data_dir, filename)
        frame = detection.split("_")[2][1:]
        detection_info = detections_dict[detection]
        detection_class = detection_info[0]
        detection_score = detection_info[1]

        if detection_class == 1:
            detection_class_str = "human"
            classification_scores = [0] * taxons_count + [1, 0]
        elif detection_class == 2:
            detection_class_str = "vehicle"
            classification_scores = [0] * taxons_count + [0, 1]
        elif detection_class == 0:
            detection_class_str = "animal"
            detection_image = Image.open(os.path.join(detections_dir, detection))
            inputs = image_processor(images=detection_image, return_tensors="pt").to(device)
            logits = pipe.model(**inputs).logits
            classification_scores = torch.nn.functional.softmax(logits, dim=-1)
            classification_scores = classification_scores.cpu().tolist()[0] + [0, 0]

        prediction = pd.DataFrame([[filepath, filename, frame, detection_class_str, detection_score] + list(detection_score * np.array(classification_scores))],columns=list(predictions.columns))
        predictions = pd.concat([predictions, prediction])

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
predictions = pd.DataFrame(columns=["Filepath", "Filename", "Frame", "Detection class", "Detection score"] + taxons_all)
filepaths_all = []


# -----------------------------
# LOOP THOUGH DATA SUBDIRECTORIES DETECt AND CLASSIFY
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
            predictions = predict_images(images_dir, detections_dir, data_subdir, predictions)
            images_count = 0

        mime= magic.from_file(filepath, mime=True)

        if mime.startswith("image"):
            shutil.copy(filepath, os.path.join(images_dir, f"F0_{file}.JPG"))
            images_count += 1
        else:
            video = cv2.VideoCapture(filepath)
            fps = video.get(cv2.CAP_PROP_FPS)
            for idx, frame in enumerate(video_utils.get_video_frames_generator(source_path=filepath, stride=int(stride * fps))):
                ImageSink(target_dir_path=images_dir, overwrite=False).save_image(
                    image=frame, image_name=f"F{idx}_{file}.JPG"
                )
                images_count += 1

    predictions = predict_images(images_dir, detections_dir, data_subdir, predictions)
    images_count = 0


# -----------------------------
# CONSOLIDATE PREDICTIONS
# -----------------------------
print("CONSOLIDATING PREDICTIONS")
gpby_dict = {"Filepath": "first", "Filename": "first"}
for taxon in taxons + ["human", "vehicle"]:
    gpby_dict[taxon] = ["mean"]

predictions_grouped = predictions.groupby("Filepath").agg(gpby_dict)
predictions_grouped["Prediction"] = predictions_grouped[taxons_all].apply(
    lambda x: "blank" if sum(x) == 0 else taxons_all[np.argmax(x)], axis=1
)
predictions_grouped["Confidence score"] = predictions_grouped[taxons_all].apply(
    lambda x: "blank" if sum(x) == 0 else np.max(x), axis=1
)
predictions_grouped.columns = [c[0] for c in predictions_grouped.columns]

for filepath in filepaths_all:
    if filepath not in predictions_grouped["Filepath"].values:
        prediction_blank = pd.DataFrame(
            [[filepath, os.path.basename(filepath)] + [0] * (taxons_count + 2) + ["blank", 1 - detection_threshold]],
            columns=list(predictions_grouped.columns),
        )
        predictions_grouped = pd.concat([predictions_grouped, prediction_blank])

predictions_grouped = predictions_grouped.reset_index(drop=True).sort_values(by="Filepath")
output_file = predictions_dir / "output_predictions.csv"
predictions_grouped.to_csv(output_file, index=False)
print(f"PREDICTIONS SAVED: {predictions_dir}  AND {output_file}")