import argparse
from ultralytics import YOLO
from torch import load, save, cuda
import yaml
import os



def train_model(base_model, yaml_file, nb_epoch, nb_batch, optimizer,lr0,lrf,momentum,seed):

    # Check for CUDA device and set it
    device = 'cuda' if cuda.is_available() else 'cpu'
    # Initialize YOLO model

    model = YOLO(base_model).to(device)

    # Utilize the path to the YAML file when calling model.train()
    model.train(batch=nb_batch, data=yaml_file, epochs=nb_epoch, optimizer=optimizer ,lr0=lr0 ,lrf=lrf , momentum=momentum, seed=seed)

    
if __name__ == "__main__":
    
    parser = argparse.ArgumentParser(description="YOLOv8 Training Script")
    parser.add_argument("names", type=str, help="names")
    parser.add_argument("nb_epoch", type=int, help="Number of epochs")
    parser.add_argument("nb_batch", type=int, help="Batch size")
    parser.add_argument("model_path", type=str, help="model")
    parser.add_argument('dir', type=str)
    parser.add_argument('training_data', type=str)

    parser.add_argument('optimizer', type=str)
    parser.add_argument('lr0', type=float)
    parser.add_argument('lrf', type=float)
    parser.add_argument('momentum', type=float)
    parser.add_argument('seed', type=int)



    args = parser.parse_args()
    
    names = args.names.split(',')

    data = {'train': 'dataset/train/images',
            'val': 'dataset/valid/images',
            # 'test': 'dataset/test/images',
            'nc': len(names),
            'names': names,
            'path': os.getcwd() + '/dataset'
            }

    # Write the data to the data.yaml file
    with open('data.yaml', 'w') as f:
        yaml.dump(data, f)

    wights_path = args.dir + "/best.pt" if args.model_path == "default" else args.model_path
    print(wights_path)
    save(load(wights_path), "model.pt")

    os.system(f"unzip {args.training_data} -d dataset")

    train_model("model.pt",'data.yaml', args.nb_epoch, args.nb_batch, args.optimizer,args.lr0,args.lrf,args.momentum,args.seed)

    os.system(f"cp runs/detect/train/weights/best.pt best.pt")
    os.system(f"cp runs/detect/train/results.csv results.csv")
    os.system(f"cp runs/detect/train/confusion_matrix.png confusion_matrix.png")

    print('here')

