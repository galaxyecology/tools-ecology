import argparse
import csv
import os

import cv2
import magic
import numpy as np
from torch import load, save
from ultralytics import YOLO
import zipfile


def write_features_csv(model, r, frame, id_frame, csv_writer):
    # Loop through each bounding box and its corresponding mask
    for box, mask in zip(r.boxes, r.masks):
        x1, y1, x2, y2 = box.xyxyn[
            0
        ].tolist()  # Extract box coordinates (normalized xyxy format)
        class_id = box.cls[0].tolist()  # Extract class ID
        conf = box.conf[0].tolist()  # Extract confidence score
        mask_xy = mask.xy[0].tolist()  # Extract mask coordinates
        color = calculate_average_color_in_region(
            frame, mask_xy
        )  # Calculate average color in the masked region

        # Write the information to the CSV file
        csv_writer.writerow(
            [id_frame, model.names[class_id], class_id, x1, y1, x2, y2, conf, color]
        )


def calculate_average_color_in_region(image, xy):
    image_array = np.array(image)
    xy = np.array(xy)
    xy = np.round(xy).astype(int)

    # Extract the region of the image based on the xy coordinates
    region = image_array[xy[:, 1], xy[:, 0]]

    # Calculate the average color in the region
    average_color = np.mean(region, axis=0)

    return average_color.astype(int)


def calculate_normalized_coverage_rate(image_shape, xy):
    image_shape = np.array(image_shape)
    xy = np.array(xy)

    # Calculate the area
    contour_area = len(xy)
    total_area = np.prod(image_shape)

    # return the normalized coverage rate
    return contour_area / total_area


def write_features_csv(model, r, frame, id_frame, csv_writer):
    # Loop through each bounding box and its corresponding mask
    for box, mask in zip(r.boxes, r.masks):
        x1, y1, x2, y2 = box.xyxyn[
            0
        ].tolist()  # Extract box coordinates (normalized xyxy format)
        class_id = box.cls[0].tolist()  # Extract class ID
        conf = box.conf[0].tolist()  # Extract confidence score
        mask_xy = mask.xy[0].tolist()  # Extract mask coordinates
        color = calculate_average_color_in_region(
            frame, mask_xy
        )  # Calculate average color in the masked region
        normalized_coverage_rate = calculate_normalized_coverage_rate(
            frame.shape, mask_xy
        )  # Calculate normalized coverage rate

        # Write the information to the CSV file
        csv_writer.writerow(
            [
                id_frame,
                model.names[class_id],
                class_id,
                x1,
                y1,
                x2,
                y2,
                conf,
                color,
                normalized_coverage_rate,
            ]
        )


def isVideo(file_path):
    detector = magic.Magic(mime=True)
    file_type = detector.from_file(file_path)

    # Check if the file is an image
    if file_type.startswith("image"):
        
        return False, str(file_type.split('/')[1])
    # Check if the file is a video
    elif file_type.startswith("video"):


        return True, str(file_type.split('/')[1])
    else:
        raise ValueError("The file is neither a photo nor a video")


def save_image(image, file_path):
    # Save the image
    rgb_image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    success = cv2.imwrite(file_path, rgb_image)

    # Check if the image saving was successful
    if success:
        print(f"Image saved successfully at: {file_path}")
    else:
        print(f"Failed to save image at: {file_path}")

def zip_folder(folder_path, zip_path):
    with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
        for root, _, files in os.walk(folder_path):
            for file in files:
                absolute_path = os.path.join(root, file)
                relative_path = os.path.relpath(absolute_path, folder_path)
                zipf.write(absolute_path, relative_path)


def main(
    input_path,
    weights_path,
    conf,
    output_csv_path,
    dir,
    project_path,
    save_csv,
    save_prediction,
    save_annotation,
    isvideo
):
    
        # Load YOLO Model
        model = YOLO(weights_path)

        # Open CSV file for writing
        with open(output_csv_path, "w", newline="") as csvfile:
            csv_writer = csv.writer(csvfile)
            csv_writer.writerow(
                [
                    "Frame",
                    "Class",
                    "id_classe",
                    "X1n",
                    "Y1n",
                    "X2n",
                    "Y2n",
                    "Confidence",
                    "color(RGB)",
                    "coverage_rate",
                ]
            )
            # Check if the input is a video
            video = isvideo

            # Run YOLO on the input
            results = model(
                input_path,
                conf=conf,
                save=save_prediction,
                save_txt=save_annotation,
                project=project_path,
            )

            

            if video:
                # If input is a video, process each frame
                cap = cv2.VideoCapture(input_path)

                # Extract the video file name from input_path
                video_file_name = os.path.basename(input_path)
                name, _ = os.path.splitext(video_file_name)
                try:
                    while True:
                        success, frame = cap.read()

                        if success:
                            id_frame = int(cap.get(cv2.CAP_PROP_POS_FRAMES))
                            print(id_frame)
                            frame_rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)

                            if results[id_frame - 1].masks:
                                if save_annotation:
                                    output_file_path = os.path.join(
                                        results[0].save_dir,
                                        "images",
                                        f"{name}_{id_frame}.jpg",
                                    )
                                    output_directory = os.path.dirname(output_file_path)
                                    os.makedirs(output_directory, exist_ok=True)
                                    save_image(frame_rgb, output_file_path)
                                if save_csv:
                                    write_features_csv(
                                        model,
                                        results[id_frame - 1],
                                        frame_rgb,
                                        id_frame,
                                        csv_writer,
                                    )
                        else:
                            break

                except Exception as e:
                    print(f"An error occurred while processing video frames: {e}")
                finally:
                    cap.release()

            else:
                # If input is an image, process the image once
                img_bgr = cv2.imread(input_path)
                img_rgb = cv2.cvtColor(img_bgr, cv2.COLOR_BGR2RGB)

                id_frame = 1
                for r in results:
                    if save_annotation:
                        output_file_path = os.path.join(
                            results[0].save_dir, "images", f"image_{id_frame}.jpg"
                        )
                        output_directory = os.path.dirname(output_file_path)
                        os.makedirs(output_directory, exist_ok=True)
                        save_image(img_rgb, output_file_path)

                    if save_csv:
                        write_features_csv(model, r, img_rgb, id_frame, csv_writer)

            # Display success messages
            if save_annotation:
                print(
                    f"Annotations saved successfully to {os.path.join(dir, results[0].save_dir, 'labels')} for labels and {os.path.join(dir, results[0].save_dir, 'images')} for images"
                )
            if save_csv:
                print(f"CSV saved successfully to {output_csv_path}")




if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument('input_file', type=str, help='input file')
    parser.add_argument('model_path', type=str, help='model_path')
    parser.add_argument('conf', type=float, default=0.5, help='confidence')
    parser.add_argument('dir', type=str)
    parser.add_argument('save_csv', type=bool)
    parser.add_argument('save_prediction', type=bool)
    parser.add_argument('save_annotation', type=bool)

    args = parser.parse_args()


    wights_path = args.dir + "/best.pt" if args.model_path == "default" else args.model_path

    save(load(wights_path), "model.pt")

    if not os.path.exists("results"):
        os.makedirs("results")

    isvideo, ext = isVideo(args.input_file)
    fp = "inputFile." + ext
    os.system(f"cp {args.input_file} {fp}")

    main(
        fp, # input file
        "model.pt", #model
        args.conf/100, # conf
        "results/output.csv", # csv out
        args.dir, #tool dir
        "results", # save folder
        args.save_csv, # save csv ?
        args.save_prediction, # save prediction ?
        args.save_annotation, # save annotation ?
        isvideo # is the data a video ?
    )

    os.remove(fp)
    os.remove("model.pt")
    zip_folder("results", 'results.zip')
