from PIL import Image
import os
import shutil

directory = os.getcwd() + '/full_res/'
for filename in os.listdir(directory):
    if filename.endswith('.png'):
        print(filename)
        img = Image.open(directory + filename)
        img.load() # required for img.split()
        
        # replace alpha channel background
        background = Image.new("RGB", img.size, (255, 255, 255))
        background.paste(img, mask=img.split()[3]) # 3 is the alpha channel

        # save RBG image as jpg
        old_name = str(os.path.splitext(filename)[0])
        background.save(directory + old_name + '.jpg', 'JPEG', quality=100)
        
        # move .png file to processed/ (no longer needed)
        source = str(directory) + str(filename)
        destination = str(os.getcwd()) + '/processed/' + str(filename)
        shutil.move(source, destination)
        
        # success message
        print(filename + ' converted to jpg')