# make color palette from complete data set
# Noah Giebink

library(tidyverse)
# source functions from palette functions
source('palette_functions.R')

# prep data ----

# for param: measurements
# measurements <- read_excel_allsheets('data/colour_between_species_2020Dec04.xlsm')
# 
# # all colors data set
# # select only relevant columns
# colors_all <- unique_id(measurements) %>% select(Species, Red, Green, Blue, id)
# 
# # calculate Lab space features
# Lab_features <- convertColor(colors_all[2:4], 'sRGB', 'Lab', scale.in = 255)
# colors_all <- cbind(colors_all, Lab_features)
# 
# # apply thresholds function to label colors 
# colors_all <- thresholds(colors_all)

# read in data 
measurements <- read_excel_allsheets('data/colour_between_species_2020Dec04.xlsm')

# keep just first two words of Species (genus, epithet)
measurements <- lapply(measurements, function(x) species_name(x))

# new RGB samples for some species -- update measurements with these
measurements_resampled <- read_excel_allsheets('data/resampled_data_2021-02-13.xlsm')
measurements_resampled <- lapply(measurements_resampled, function(x) select(x, Species, 
                                                                            Red = R,
                                                                            Green = G,
                                                                            Blue = B))

# remove scale measurements
measurements <- measurements[c('Leaf', 'Male Scale', 'Perigynium', 'Female Scale', 'Cataphyll')]
measurements <- measurements[-c(2,4)]
measurements_resampled <- measurements_resampled[-c(2,4)]

# combine measurements lists, apply unique IDs
measurements <- unique_id(measurements)
measurements_resampled <- unique_id(measurements_resampled)

# replace updated measurements with the resampled ones
colors_full <- update_measurements(measurements, measurements_resampled)
colors_full <- colors_full %>% select(Species, Red, Green, Blue, id)

# add features
# add CIELab for lightness (L channel),
# distinguishing red vs. greenness (a channel: 'red-green)
# and blue vs. yellowness (b channel: 'blue-yellow')
Lab_features <- convertColor(colors_full[2:4], 'sRGB', 'Lab', scale.in = 255)
colors_full <- cbind(colors_full, Lab_features)

# apply thresholds function to label colors 
colors_full <- thresholds(colors_full)

# Fine-tuning ----

# find natural breaks in color distribution histograms for white and brown (tan?)
# white
white <- filter(colors_full, color=='white')

ggplot(white, aes(x = b)) +
  geom_density(fill = centroid_color(white), color='black') +
  scale_x_continuous(breaks = seq(0,30,2)) +
  theme_pubr()

white <- mutate(white, bin = ifelse(b<18, 'white', 'brown'))


white_breaks <- ggplot(white, aes(x = b, fill = bin)) +
  geom_histogram(color='black') +
  scale_fill_manual(values=c(centroid_color(filter(white, bin=='brown')), 
                             centroid_color(filter(white, bin=='white')))) +
  theme_pubr()


# discard
# find natural break in 'a' where yellow-green becomes green
# bind to yellow-green
yellow_green <- colors_full %>% filter(color=='yellow-green')
discards <- colors_full %>% filter(color=='discard')
yellow_green2 <- bind_rows(yellow_green, discards)

yg <- mutate(yellow_green2, bin=ifelse(a > -9, 'yellow-green', 'green'))


yg_breaks <- ggplot(yg, aes(x = a, fill = bin)) +
  geom_histogram(color='black') +
  scale_fill_manual(values=c(centroid_color(filter(discards, bin=='green')), 
                             centroid_color(filter(discards, bin=='yellow-green')))) +
  theme_pubr()

# red
# find natural break in 'b' where red becomes yellow-brown

red <- colors_full %>% filter(color=='red')
red <- mutate(red, bin=ifelse(b<18, 'red', 'yellow-brown'))

ggplot(red, aes(x=b))+
  geom_histogram(color='black', binwidth=1)

red_breaks <- ggplot(red, aes(x=b, fill=bin))+
  geom_histogram(color='black', binwidth = 1) +
  scale_fill_manual(values=c(centroid_color(filter(red, bin=='red')),
                             centroid_color(filter(red, bin=='yellow-brown')))) +
  theme_pubr()

# brown
brown <- colors_full %>% filter(color=='brown')
brown <- mutate(brown, bin=ifelse(b<18, 'brown', 'yellow-brown'))

ggplot(brown, aes(x=b))+
  geom_histogram(color='black', binwidth=1)

red_breaks <- ggplot(red, aes(x=b, fill=bin))+
  geom_histogram(color='black', binwidth = 1) +
  scale_fill_manual(values=c(centroid_color(filter(red, bin=='red')),
                             centroid_color(filter(red, bin=='yellow-brown')))) +
  scale_x_continuous(breaks = seq(0,50,2)) +
  theme_pubr()

# Visualization ----
# get sample color 
sample_brown = centroid_color(filter(colors_full, color=='brown'))
sample_green = centroid_color(filter(colors_full, color=='green'))
sample_red = centroid_color(filter(colors_full, color=='red'))
sample_white = centroid_color(filter(colors_full, color=='white'))
sample_yellow_brown = centroid_color(filter(colors_full, color=='yellow-brown'))
sample_yellow_green = centroid_color(filter(colors_full, color=='yellow-green'))
sample_discard = centroid_color(filter(colors_full, color=='discard'))

# change name of sample color to "sample" to use as reference point
colors_full_marked <- mark_samples(colors_full, sample_green, sample_brown, sample_red, 
                                       sample_white, sample_yellow_green, sample_yellow_brown,
                                       sample_discard)

# set plot limits
x_min <- -25
x_max <- 30
y_min <- -5
y_max <- 60

color_space_full <- ggplot(colors_full_marked, aes(x=a, y=b, color=color)) +
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
  geom_point(data=filter(colors_full_marked, color=='sample'))


# save full color space
ggsave('./plots/color_space_full.png', color_space_all, width = 10, height = 8)
