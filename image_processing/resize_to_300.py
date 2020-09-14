from PIL import Image, ImageOps
import os
import shutil
      

directory = str(os.getcwd()) + '/full_res/'
for filename in os.listdir(directory):
    if filename.endswith('.jpg'):
        img_square = Image.open(directory + filename)
        # save square image at 300x300 in directory: 300_res/
        img_300 = img_square.resize((300,300))
        old_name = str(os.path.splitext(filename)[0])
        new_name = old_name.replace("full_res", '300x300.jpg')
        path_300 = '300_res/' + new_name
        img_300.save(path_300)
        # move full_res image to directory: full_res_processed/
        source = str(directory) + str(filename)
        destination = str(os.getcwd()) + '/full_res_processed/' + str(filename)
        shutil.move(source, destination)
        # print success message
        print(new_name + ' is 300x300')