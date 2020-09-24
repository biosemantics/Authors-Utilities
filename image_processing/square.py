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

        # Add citation text over white box, scaled to image size
        font_size = int(max_dim/37) # 20pt font @ 747x747 (first test image)
        position = (int(max_dim - 11.5*font_size), 
                    int(max_dim - 2.85*font_size))
        box_shape = [(position[0] - 5, position[1] - 5), 
                      (max_dim, max_dim)]
        
            # add white box (FNA Vol. 23 citation will go over this) 
        text_box = ImageDraw.Draw(img_square)
        text_box.rectangle(box_shape, fill='white')
        
            # add FNA Vol. 23 citation to bottom right corner
        citation = ImageDraw.Draw(img_square)
        font = ImageFont.truetype('ARI.ttf', size=font_size) # scale with image size
        message = 'FNA Vol. 23\nOxford University Press'
        citation.text(position, message, fill='black', font=font)


        # Add illustration title to top left corner
        title_name = str(os.path.splitext(filename)[0]).replace('_', ' ').capitalize()
        font_size_title = int(font_size*1.75)
        
            # add white box under text
            # use length of title to determine text box size 
        title_box_shape = [(0,0),
                           (int((len(title_name)*font_size)/1.1), 
                           int(font_size_title*1.25))]
        title_box = ImageDraw.Draw(img_square)
        title_box.rectangle(title_box_shape, fill='white')
            # insert text over white box
        title = ImageDraw.Draw(img_square)
        font = ImageFont.truetype('ARI.ttf', size=font_size_title)
        title.text((10,0), title_name, fill='black', font = font)

        # save square image at full resolution in directory: full_res/
        path_full_res = 'full_res/' + str(os.path.splitext(filename)[0]) + '_full_res.png'
        img_square.save(path_full_res)

        # move img_raw to directory: processed/
        source = str(directory) + '/raw_images/' + str(filename)
        destination = str(directory) + '/processed/' + str(filename)
        shutil.move(source, destination)

        # success message
        print(filename + ' successfully fit to square dimensions')