# make color palette from complete data set
# Noah Giebink

library(tidyverse)
# source functions from palette functions
source('palette_functions.R')

# prep data ----

# for param: measurements
measurements <- read_excel_allsheets('data/colour_between_species_2020Dec04.xlsm')

# all colors data set
# select only relevant columns
colors_all <- unique_id(measurements) %>% select(Species, Red, Green, Blue, id)

# calculate Lab space features
Lab_features <- convertColor(colors_all[2:4], 'sRGB', 'Lab', scale.in = 255)
colors_all <- cbind(colors_all, Lab_features)

# apply thresholds function to label colors 
colors_all <- thresholds(colors_all)


# Fine-tuning ----



# Visualization ----
# get sample color 
sample_brown = centroid_color(filter(colors_all, color=='brown'))
sample_green = centroid_color(filter(colors_all, color=='green'))
sample_red = centroid_color(filter(colors_all, color=='red'))
sample_white = centroid_color(filter(colors_all, color=='white'))
sample_yellow_brown = centroid_color(filter(colors_all, color=='yellow-brown'))
sample_yellow_green = centroid_color(filter(colors_all, color=='yellow-green'))
sample_discard = centroid_color(filter(colors_all, color=='discard'))

# change name of sample color to "sample" to use as reference point
colors_all_marked <- mark_samples(colors_all, sample_green, sample_brown, sample_red, 
                                       sample_white, sample_yellow_green, sample_yellow_brown,
                                       sample_discard)

x_min <- -25
x_max <- 30
y_min <- -5
y_max <- 60

color_space_all <- ggplot(colors_all_marked, aes(x=a, y=b, color=color)) +
  geom_point(alpha=1/2) +
  # facet_grid(rows=vars(color), cols=vars(L_bin)) +
  scale_color_manual(values=c(sample_brown, sample_discard, sample_green, sample_red, "red", sample_white,
                              sample_yellow_brown, sample_yellow_green)) +
  ylab('Blue-Yellow') +
  xlab('Green-Red') +
  ylim(c(y_min, y_max)) +
  xlim(c(x_min, x_max)) +
  geom_rect(xmin = 10, xmax = x_max, ymin = y_min, ymax = y_max, color = sample_red, alpha = 0) + 
  geom_rect(xmin = 0, xmax = 10, ymin = y_min, ymax = 30, color = sample_brown, alpha = 0) +
  geom_rect(xmin = x_min, xmax = -5, ymin = y_min, ymax = 30, color = sample_green, alpha = 0) +
  geom_rect(xmin = -15, xmax = -2, ymin = 30, ymax = y_max, color = sample_yellow_green, alpha = 0) +
  geom_rect(xmin  =  -2, xmax = 10, ymin = 30, ymax = y_max, color = sample_yellow_brown, alpha = 0) +
  theme(legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12)) +
  geom_point()

# save full color space
ggsave('./plots/color_space_full.png', color_space_all, width = 10, height = 8)
