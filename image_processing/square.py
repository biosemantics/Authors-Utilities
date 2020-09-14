from PIL import Image, ImageOps, ImageFont, ImageDraw
import os
import shutil


# loop through raw (un-resized) images: adding white borders to correct dimension 
directory = os.getcwd()
for filename in os.listdir(directory + '/raw_images'):
    if filename.endswith('.png'):
        img_raw = Image.open(directory + '/raw_images/' + filename)
        
        # calculate border size needed to make square
        dims = img_raw.size
        max_dim = max(dims)
        min_dim = min(dims)
        border_size = int((max_dim - min_dim)/2)
        # get index of max dimension
        min_index = dims.index(min(img_raw.size))
       
        # add border to smaller dimension to make img square
        
        if min_index == 0:
            img_square = ImageOps.expand(img_raw, border=(border_size,0), fill='white')
        else:
            img_square = ImageOps.expand(img_raw, border=(0,border_size), fill='white')

        # add FNA Vol. 23 citation to bottom right corner
        citation = ImageDraw.Draw(img_square)
        font = ImageFont.truetype('ARI.ttf', size=20)
        # x,y need to be 498 690 for 747x747, i.e. citation is 249 x 57
        (x,y) = ((max_dim-230), (max_dim - 57))
        message = 'FNA Vol. 23\nOxford University Press'

        citation.text((x,y), message, fill='black', font=font)


        # save square image at full resolution in directory: full_res/
        path_full_res = 'full_res/' + str(os.path.splitext(filename)[0]) + '_full_res.png'
        img_square.save(path_full_res)

        # move img_raw to directory: processed/
        source = str(directory) + '/raw_images/' + str(filename)
        destination = str(directory) + '/processed/' + str(filename)
        shutil.move(source, destination)

        # success message
        print(filename + ' successfully fit to square dimensions')