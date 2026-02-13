# main_script.py
import sys
from PIL import Image
from ultralytics import YOLO

import os

use_cuda = False
use_CPU = True

task = "segment"

save = False
show = False



def load_networks():
    if use_CPU :
        return YOLO(model_path)
    else:
        pass

def segmentation(input_images, result_images, conf):
    model = load_networks()
    
    results = model.predict(
        input_images,
        task = task,
        save = save ,
        show = show,
        conf = conf,
    )

    # Show the results
    for r in results:
        im_array = r.plot()  # plot a BGR numpy array of predictions
        im = Image.fromarray(im_array[..., ::-1])  # RGB PIL image
        im.show()  # show image
        im.save(result_images)  # save image
        


if __name__ == "__main__":


    input_images = sys.argv[1]
    result_images = sys.argv[2]
    conf = float(sys.argv[3])/100
    model_path = sys.argv[4] 
    use_gpu = bool(sys.argv[4])

    os.rename(input_images, str(input_images[:-3]+"png"))
    input_images = input_images[:-3]+"png"
    
    segmentation(input_images, result_images, conf)
