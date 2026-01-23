#!/usr/bin/env python3
import argparse
import os
import json
from pathlib import Path

import numpy as np
from ultralytics.models.sam.predict import SAM3VideoSemanticPredictor


def parse_args():
    parser = argparse.ArgumentParser(
        description="SAM3 Video Semantic Segmentation (Galaxy tool)"
    )

    parser.add_argument(
        "source",
        type=str,
        help="Path to image or video"
    )

    parser.add_argument(
        "text",
        type=str,
        help="Text prompt (class to segment)"
    )

    parser.add_argument(
        "vid_stride",
        type=int,
        default=5,
        help="Process 1 frame every N frames (video only)"
    )

    parser.add_argument(
        "conf",
        type=float,
        default=0.25,
        help="Confidence threshold"
    )

    parser.add_argument(
        "model",
        type=str,
        help="Path to sam3.pt model"
    )

    parser.add_argument(
        "outdir",
        type=str,
        default="sam3_outputs",
        help="Output directory"
    )

    return parser.parse_args()


def main():
    args = parse_args()

    source_path = args.source.strip()
    text_prompt = args.text.strip()
    vid_stride = args.vid_stride
    conf_thres = args.conf
    model_path = args.model
    outdir = Path(args.outdir)

    outdir.mkdir(parents=True, exist_ok=True)

    overrides = dict(
        conf=conf_thres,
        show_conf=False,
        task="segment",
        mode="predict",
        model=model_path,
        half=True,
        vid_stride=vid_stride,
        save=True,  # Galaxy gère les outputs
        save_dir=str(outdir)
        
    )

    predictor = SAM3VideoSemanticPredictor(overrides=overrides)

    results = predictor(
        source=source_path,
        text=text_prompt,
        stream=False
    )


if __name__ == "__main__":
    main()

    
