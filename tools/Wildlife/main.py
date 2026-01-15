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

from PytorchWildlife.data import datasets as pw_data
from torch.utils.data import DataLoader
import time
from yolov5.utils.general import xywh2xyxy, scale_boxes
import torchvision

from PytorchWildlife.models.detection.ultralytics_based.megadetectorv5 import (
    MegaDetectorV5,
)


def batch_image_detection2(
    self,
    data_path,
    batch_size: int = 16,
    det_conf_thres: float = 0.2,
    id_strip: str = None,
) -> list[dict]:
    """
    Perform detection on a batch of images.

    Args:
        data_path (str): Path containing all images for inference.
        batch_size (int, optional): Batch size for inference. Defaults to 16.
        det_conf_thres (float, optional): 
            Confidence threshold for predictions. Defaults to 0.2.
        id_strip (str, optional): 
            Characters to strip from img_id. Defaults to None.

    Returns:
        list[dict]: List of detection results for all images.
    """
    # Custom batch image detection for MegaDetectorV5
    # Uses DataLoader + custom NMS and returns normalized bounding boxes.
    # Must be attached to MegaDetectorV5 before model instantiation.
    dataset = pw_data.DetectionImageFolder(
        data_path,
        transform=self.transform,
    )

    # Creating a DataLoader for batching and parallel processing of the images
    loader = DataLoader(
        dataset,
        batch_size=batch_size,
        shuffle=False,
        pin_memory=True,
        num_workers=0,
        drop_last=False,
    )

    results = []
    with tqdm(total=len(loader)) as pbar:
        for batch_index, (imgs, paths, sizes) in enumerate(loader):
            imgs = imgs.to(self.device)
            predictions = self.model(imgs)[0].detach().cpu()
            predictions = non_max_suppression2(
                predictions, conf_thres=det_conf_thres
            )

            batch_results = []
            for i, pred in enumerate(predictions):
                if pred.size(0) == 0:
                    continue
                pred = pred.numpy()
                size = sizes[i].numpy()
                path = paths[i]
                original_coords = pred[:, :4].copy()
                pred[:, :4] = scale_boxes(
                    [self.IMAGE_SIZE] * 2, pred[:, :4], size
                ).round()
                # Normalize the coordinates for timelapse compatibility
                normalized_coords = [
                    [x1 / size[1], y1 / size[0], x2 / size[1], y2 / size[0]]
                    for x1, y1, x2, y2 in pred[:, :4]
                ]
                res = self.results_generation(pred, path, id_strip)
                res["normalized_coords"] = normalized_coords
                batch_results.append(res)
            pbar.update(1)
            results.extend(batch_results)
        return results


MegaDetectorV5.batch_image_detection2 = batch_image_detection2


def non_max_suppression2(
    prediction,
    conf_thres=0.25,
    iou_thres=0.45,
    classes=None,
    agnostic=False,
    multi_label=False,
    labels=(),
    max_det=300,
):
    """Runs Non-Maximum Suppression (NMS) on inference results

    Returns:
         list of detections, on (n,6) tensor per image [xyxy, conf, cls]
    """

    nc = prediction.shape[2] - 5  # number of classes
    xc = prediction[..., 4] > conf_thres  # candidates

    # Checks
    assert 0 <= conf_thres <= 1, (
        f"Invalid Confidence threshold {conf_thres}, "
        "valid values are between 0.0 and 1.0"
    )
    assert (
        0 <= iou_thres <= 1
    ), f"Invalid IoU {iou_thres}, valid values are between 0.0 and 1.0"

    # Settings
    # (pixels) minimum and maximum box width and height
    min_wh, max_wh = 2, 4096
    max_nms = 30000  # maximum number of boxes into torchvision.ops.nms()
    time_limit = time_NMS  # seconds to quit after
    redundant = True  # require redundant detections
    multi_label &= nc > 1  # multiple labels per box (adds 0.5ms/img)
    merge = False  # use merge-NMS

    t = time.time()
    output = [
        torch.zeros((0, 6), device=prediction.device)
    ] * prediction.shape[0]
    for xi, x in enumerate(prediction):  # image index, image inference
        # Apply constraints
        x = x[xc[xi]]  # confidence

        # Cat apriori labels if autolabelling
        if labels and len(labels[xi]):
            l = labels[xi]
            v = torch.zeros((len(l), nc + 5), device=x.device)
            v[:, :4] = l[:, 1:5]  # box
            v[:, 4] = 1.0  # conf
            v[range(len(l)), l[:, 0].long() + 5] = 1.0  # cls
            x = torch.cat((x, v), 0)

        # If none remain process next image
        if not x.shape[0]:
            continue

        # Compute conf
        x[:, 5:] *= x[:, 4:5]  # conf = obj_conf * cls_conf

        # Box (center x, center y, width, height) to (x1, y1, x2, y2)
        box = xywh2xyxy(x[:, :4])

        # Detections matrix nx6 (xyxy, conf, cls)
        if multi_label:
            i, j = (x[:, 5:] > conf_thres).nonzero(as_tuple=False).T
            x = torch.cat((box[i], x[i, j + 5, None], j[:, None].float()), 1)
        else:  # best class only
            conf, j = x[:, 5:].max(1, keepdim=True)
            x = torch.cat((box, conf, j.float()), 1)[
                conf.view(-1) > conf_thres
            ]

        # Filter by class
        if classes is not None:
            x = x[(x[:, 5:6] == torch.tensor(classes, device=x.device)).any(1)]

        # Apply finite constraint
        # if not torch.isfinite(x).all():
        #     x = x[torch.isfinite(x).all(1)]

        # Check shape
        n = x.shape[0]  # number of boxes
        if not n:  # no boxes
            continue
        elif n > max_nms:  # excess boxes
            # sort by confidence
            x = x[x[:, 4].argsort(descending=True)[:max_nms]]

        # Batched NMS
        c = x[:, 5:6] * (0 if agnostic else max_wh)  # classes
        # boxes (offset by class), scores
        boxes, scores = x[:, :4] + c, x[:, 4]
        i = torchvision.ops.nms(boxes, scores, iou_thres)  # NMS
        if i.shape[0] > max_det:  # limit detections
            i = i[:max_det]
        if merge and (
            1 < n < 3e3
        ):  # Merge NMS (boxes merged using weighted mean)
            # update boxes as boxes(i,4) = weights(i,n) * boxes(n,4)
            iou = box_iou(boxes[i], boxes) > iou_thres  # iou matrix
            weights = iou * scores[None]  # box weights
            x[i, :4] = torch.mm(weights, x[:, :4]).float() / weights.sum(
                1, keepdim=True
            )  # merged boxes
            if redundant:
                i = i[iou.sum(1) > 1]  # require redundancy

        output[xi] = x[i]
        if (time.time() - t) > time_limit:
            print(f"WARNING: NMS time limit {time_limit}s exceeded")
            break  # time limit exceeded

    return output


# ============================================================
# CONFIGURATION
# ============================================================

parser = argparse.ArgumentParser(
    description="Wildlife Detection & Classification using MegaDetector"
    "+ Hugging Face model"
)

parser.add_argument(
    "model_name",
    type=str,
    help="Hugging Face model name for feature extraction",
)
parser.add_argument(
    "classifier_model",
    type=str,
    help="Path to fine-tuned classifier weights (.safetensors)",
)
parser.add_argument(
    "json_model", type=str, help="Path to classifier config (.json)"
)
parser.add_argument(
    "type_mapping",
    type=str,
    choices=[" id2label", " label2id"],
    help="Label mapping direction",
)
parser.add_argument(
    "boxing_mode",
    type=str,
    choices=[" no_image", " all_image"],
    help="Bounding box output mode",
)
parser.add_argument(
    "path_input",
    type=str,
    help="Comma-separated list of input files (images or videos)",
)
parser.add_argument(
    "detection_threshold",
    type=float,
    help="Detection confidence threshold (0-1)",
)
parser.add_argument("stride", type=int, help="Frame extraction stride")
parser.add_argument(
    "images_max",
    type=int,
    help="Maximum number of images to process per folder",
)
parser.add_argument(
    "time_NMS",
    type=int,
    help="Time limit for the process NMS (non maximum suppression)",
)
parser.add_argument(
    "run_dir", type=str, help="Output directory for predictions"
)
parser.add_argument(
    "name_file",
    nargs=argparse.REMAINDER,
    help="Optional custom names for input files",
)

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
time_NMS = args.time_NMS
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
image_processor = AutoImageProcessor.from_pretrained(model_name, use_fast=True)
classifier = AutoModelForImageClassification.from_pretrained(
    "./classifier_model_dir"
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

    detections_list = detection_model.batch_image_detection2(
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
            (
                int(p[1:])
                for p in filename.split("_")
                if p.startswith("F") and p[1:].isdigit()
            ),
            0,
        )

        if det_class == 1:  # human
            scores = [0] * len(taxons) + [1, 0]
        elif det_class == 2:  # vehicle
            scores = [0] * len(taxons) + [0, 1]
        else:
            image = (
                info[3]
                if boxing_mode == "no_image"
                else Image.open(os.path.join(detections_dir, detection))
            )
            inputs = image_processor(
                images=image,
                return_tensors="pt",
            ).to(device)
            logits = pipe.model(**inputs).logits
            softmax_scores = (
                torch.nn.functional.softmax(logits, dim=-1).cpu().tolist()[0]
            )
            scores = softmax_scores + [0, 0]

        prediction_row = pd.DataFrame(
            [
                [detection, filename, frame]
                + list(det_score * np.array(scores))
            ],
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
            images_dir, detections_dir, predictions, boxing_mode
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
                source_path=str(filepath), stride=int(stride * fps)
            )
        ):
            ImageSink(images_dir, overwrite=False).save_image(
                image=frame, image_name=f"F{idx+1}_{filename}.JPG"
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
    lambda x: "blank" if sum(x) == 0 else taxons_all[np.argmax(x)], axis=1
)
predictions["Confidence score"] = predictions[taxons_all].apply(
    lambda x: 0 if sum(x) == 0 else np.max(x), axis=1
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
    recap_rows.append(
        {
            "Filename": filename,
            "Species": species,
            "Frames": frames_sorted,
            "Count": len(df),
            "Confidence mean": round(df["Confidence score"].mean(), 4),
            "Confidence min": round(df["Confidence score"].min(), 4),
            "Confidence max": round(df["Confidence score"].max(), 4),
        }
    )

recap_df = (
    pd.DataFrame(recap_rows)
    .sort_values(by=["Filename", "Species"])
    .reset_index(drop=True)
)
output_file_recap = predictions_dir / "output_predictions_recap.csv"
recap_df.to_csv(output_file_recap, index=False)

print(f"Summary predictions saved: {output_file_recap}")
