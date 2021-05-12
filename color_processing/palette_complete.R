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


# build palette: split colors ----
# Split colors using both methods: linear SVM and thresholds

# apply thresholds function to label colors 
colors_full_thresholds <- thresholds(colors_full)

# predict with linear SVM
svm_final <- readRDS("model/svm_linear.rds")
colors_full_svm <- predict(svm_final, colors_full[,c('a','b')])
colors_full_svm <- data.frame(colors_full_svm)
colors_full_svm <- colors_full_svm %>% rename(color = colors_full_svm)
colors_full_svm <- bind_cols(colors_full, colors_full_svm)

# apply k-means to separate each color into light, medium, and dark
set.seed(42)

# thresholds data
km_thresholds <- colors_full_thresholds %>% group_by(color) %>% group_map(~ kmeans(.x$L, 3))

brown_full <- get_lmd(mutate(filter(colors_full_thresholds, color=='brown'), 
                             cluster = km_thresholds[[1]]$cluster), 
                      km_thresholds[[1]])
green_full <- get_lmd(mutate(filter(colors_full_thresholds, color=='green'), 
                             cluster = km_thresholds[[2]]$cluster), 
                      km_thresholds[[2]])
red_full <- get_lmd(mutate(filter(colors_full_thresholds, color=='red'), 
                             cluster = km_thresholds[[3]]$cluster), 
                      km_thresholds[[3]])
white_full <- get_lmd(mutate(filter(colors_full_thresholds, color=='white'), 
                             cluster = km_thresholds[[4]]$cluster), 
                      km_thresholds[[4]])
yb_full <- get_lmd(mutate(filter(colors_full_thresholds, color=='yellow-brown'), 
                             cluster = km_thresholds[[5]]$cluster), 
                      km_thresholds[[5]])
yg_full <- get_lmd(mutate(filter(colors_full_thresholds, color=='yellow-green'), 
                             cluster = km_thresholds[[6]]$cluster), 
                      km_thresholds[[6]])

colors_full_thresholds_lmd <- bind_rows(brown_full, green_full, red_full,
                                        white_full, yb_full, yg_full)

# svm data
km_svm <- colors_full_svm %>% group_by(color) %>% group_map(~ kmeans(.x$L, 3))

brown_full_svm<- get_lmd(mutate(filter(colors_full_svm, color=='brown'), 
                             cluster = km_svm[[1]]$cluster), 
                      km_svm[[1]])
green_full_svm<- get_lmd(mutate(filter(colors_full_svm, color=='green'), 
                             cluster = km_svm[[2]]$cluster), 
                      km_svm[[2]])
red_full_svm<- get_lmd(mutate(filter(colors_full_svm, color=='red'), 
                           cluster = km_svm[[3]]$cluster), 
                    km_svm[[3]])
yb_full_svm<- get_lmd(mutate(filter(colors_full_svm, color=='yellow-brown'), 
                          cluster = km_svm[[4]]$cluster), 
                   km_svm[[4]])
yg_full_svm <- get_lmd(mutate(filter(colors_full_svm, color=='yellow-green'), 
                          cluster = km_svm[[5]]$cluster), 
                   km_svm[[5]])

colors_full_svm_lmd <- bind_rows(brown_full_svm, green_full_svm, red_full_svm,
                                 yb_full_svm, yg_full_svm)


# # save full data sets for visualization step (already run)
# saveRDS(colors_full_thresholds_lmd, 'data/colors_full_thresholds_lmd.rds')
# saveRDS(colors_full_svm_lmd, 'data/colors_full_svm_lmd.rds')

# # if need to read back in
# colors_full_thresholds_lmd <- readRDS('data/colors_full_thresholds_lmd.rds')
# colors_full_svm_lmd <- readRDS('data/colors_full_svm_lmd.rds')

# build palette: sample final colors, tabulate ----

# # down sample each color-brightness combination to 10 samples for tabular 
# colors_down_svm <- colors_full_svm_lmd %>% 
#   group_by(color, brightness) %>% 
#   group_map(~ nn_downsample(.x, 'a', 'b')) %>% 
#   bind_rows() %>%
#   left_join(.,colors_full_svm_lmd)


# # IF down sampling to 10 per color-brightness combo:
# # need to do white separately for thresholds data because there are too few obs
# colors_nowhite_thresholds_lmd <- filter(colors_full_thresholds_lmd, color !='white')
# white_thresholds_lmd <- filter(colors_full_thresholds_lmd, color =='white')
# 
# colors_down_thresholds <- colors_nowhite_thresholds_lmd %>% 
#   group_by(color, brightness) %>% 
#   group_map(~ nn_downsample(.x, 'a', 'b')) %>%
#   bind_rows() %>%
#   left_join(.,colors_full_thresholds_lmd)
# white_down_thresholds <- white_thresholds_lmd %>% 
#   group_by(color, brightness) %>% 
#   group_map(~ nn_downsample(.x, 'a', 'b', size=9)) %>%
#   bind_rows() %>%
#   left_join(.,white_thresholds_lmd)
# colors_down_thresholds <- bind_rows(colors_down_thresholds, white_down_thresholds)

# use 5 samples per brightness-color combo
colors_down_svm <- colors_full_svm_lmd %>%
  group_by(color, brightness) %>%
  group_map(~ nn_downsample(.x, 'a', 'b', size=5)) %>%
  bind_rows() %>%
  left_join(.,colors_full_svm_lmd)
colors_down_thresholds <- colors_full_thresholds_lmd %>%
  group_by(color, brightness) %>%
  group_map(~ nn_downsample(.x, 'a', 'b', size=5)) %>%
  bind_rows() %>%
  left_join(.,colors_full_thresholds_lmd)


# tabulate color palettes

palette_svm <- rgb_palette(arrange(filter(colors_down_svm, color=='brown'), desc(L)), 
                           arrange(filter(colors_down_svm, color=='green'), desc(L)),
                           arrange(filter(colors_down_svm, color=='red'), desc(L)),
                           arrange(filter(colors_down_svm, color=='yellow-brown'), desc(L)),
                           arrange(filter(colors_down_svm, color=='yellow-green'), desc(L)),
                           color_names = list('Brown', 'Green', 'Red', 'Yellow-brown', 'Yellow-green'))

palette_thresholds <- rgb_palette(arrange(filter(colors_down_thresholds, color=='brown'), desc(L)), 
                                  arrange(filter(colors_down_thresholds, color=='green'), desc(L)),
                                  arrange(filter(colors_down_thresholds, color=='red'), desc(L)),
                                  arrange(filter(colors_down_thresholds, color=='white'), desc(L)),
                                  arrange(filter(colors_down_thresholds, color=='yellow-brown'), desc(L)),
                                  arrange(filter(colors_down_thresholds, color=='yellow-green'), desc(L)),
                                  color_names = list('Brown', 'Green', 'Red', 'White','Yellow-brown', 'Yellow-green'))


palettes <- ggarrange(palette_thresholds, palette_svm, ncol=1, nrow=2)


# Visualization ----
x_min <- -25
x_max <- 30
y_min <- -5
y_max <- 60

# mark samples
colors_full_svm_lmd$color <- as.character(colors_full_svm_lmd$color)
svm_marked <- mark_samples(colors_full_svm_lmd,
                                  centroid_color(filter(colors_full_svm_lmd, color == 'brown')),
                                  centroid_color(filter(colors_full_svm_lmd, color == 'green')),
                                  centroid_color(filter(colors_full_svm_lmd, color == 'red')),
                                  centroid_color(filter(colors_full_svm_lmd, color == 'yellow-brown')),
                                  centroid_color(filter(colors_full_svm_lmd, color == 'yellow-green')))
colors_full_thresholds_lmd$color <- as.character(colors_full_thresholds_lmd$color)
thresholds_marked <- mark_samples(colors_full_thresholds_lmd,
                                  centroid_color(filter(colors_full_thresholds_lmd, color == 'brown')),
                                  centroid_color(filter(colors_full_thresholds_lmd, color == 'green')),
                                  centroid_color(filter(colors_full_thresholds_lmd, color == 'red')),
                                  centroid_color(filter(colors_full_thresholds_lmd, color == 'white')),
                                  centroid_color(filter(colors_full_thresholds_lmd, color == 'yellow-brown')),
                                  centroid_color(filter(colors_full_thresholds_lmd, color == 'yellow-green')))


color_space_full_thresholds <- ggplot(thresholds_marked,  
                           aes(a, b, color = color))+
  geom_point(size = 1.25)+
  scale_color_manual(values=c(centroid_color(filter(colors_full_thresholds_lmd, color=='brown')),
                              centroid_color(filter(colors_full_thresholds_lmd, color=='green')),
                              centroid_color(filter(colors_full_thresholds_lmd, color=='red')),
                              'red',
                              centroid_color(filter(colors_full_thresholds_lmd, color=='white')),
                              centroid_color(filter(colors_full_thresholds_lmd, color=='yellow-brown')),
                              centroid_color(filter(colors_full_thresholds_lmd, color=='yellow-green'))))+
  geom_point(data = filter(thresholds_marked, color == 'sample'), size = 1.5)+
  labs(x = 'Green-Red', y = 'Blue-Yellow', color='Color')+
  ylim(c(y_min, y_max)) +
  xlim(c(x_min, x_max)) +
  geom_rect(xmin = 10.2, xmax = x_max, ymin = y_min, ymax = 24.8, 
            color = centroid_color(filter(colors_full_thresholds_lmd, color=='red')), 
            alpha = 0, size = 1.5) +
  geom_rect(xmin = -1.8, xmax = 9.8, ymin = y_min, ymax = 31.8, 
            color = centroid_color(filter(colors_full_thresholds_lmd, color=='brown')), 
            alpha = 0, size = 1.5) +
  geom_rect(xmin = 10.2, xmax = x_max, ymin = 25.2, ymax = y_max, 
            color = centroid_color(filter(colors_full_thresholds_lmd, color=='brown')), 
            alpha = 0, size = 1.5) +
  geom_rect(xmin = x_min, xmax = -2.2, ymin = y_min, ymax = 31.8, 
            color = centroid_color(filter(colors_full_thresholds_lmd, color=='green')), 
            alpha = 0, size = 1.5) +
  geom_rect(xmin = x_min, xmax = -12.2, ymin = 32.2, ymax = y_max, 
            color = centroid_color(filter(colors_full_thresholds_lmd, color=='green')), 
            alpha = 0, size = 1.5) +
  geom_rect(xmin = -11.8, xmax = -2.2, ymin = 32.2, ymax = y_max, 
            color = centroid_color(filter(colors_full_thresholds_lmd, color=='yellow-green')), 
            alpha = 0, size = 1.5) +
  geom_rect(xmin  =  -1.8, xmax = 9.8, ymin = 32.2, ymax = y_max, 
            color = centroid_color(filter(colors_full_thresholds_lmd, color=='yellow-brown')), 
            alpha = 0, size = 1.5) +
  geom_rect(xmin  =  -5, xmax = 0, ymin = y_min, ymax = 20, 
            color = centroid_color(filter(colors_full_thresholds_lmd, color=='white')), 
            alpha = 0, size = 1.5) +
  theme_pubr() +
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 

color_space_full_svm <- ggplot(svm_marked,  
                               aes(a, b, color = color))+
  geom_point(size = 1.25)+
  scale_color_manual(values=c(centroid_color(filter(colors_full_svm_lmd, color=='brown')),
                              centroid_color(filter(colors_full_svm_lmd, color=='green')),
                              centroid_color(filter(colors_full_svm_lmd, color=='red')),
                              'red',
                              centroid_color(filter(colors_full_svm_lmd, color=='yellow-brown')),
                              centroid_color(filter(colors_full_svm_lmd, color=='yellow-green'))))+
  geom_point(data=filter(svm_marked, color=='sample'), size = 1.25)+
  labs(x = 'Green-Red', y = 'Blue-Yellow', color='Color')+
  ylim(c(y_min, y_max)) +
  xlim(c(x_min, x_max)) +
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 

color_spaces_full <- ggarrange(color_space_full_thresholds, color_space_full_svm)

# save visualizations (already run) ----
# ggsave('plots/final/palettes.png', palettes, width = 16, height = 10)
# ggsave('plots/final/color_spaces_full.png', color_spaces_full, width = 10, height = 5)

# save color palette csv files ----
final_palette_svm <- colors_full_svm_lmd %>% select(-cluster)
final_palette_thresholds <- colors_full_thresholds_lmd %>% select(-cluster)

write_csv(final_palette_svm, 'data/Final_Palette-svm.csv')
write_csv(final_palette_thresholds, 'data/Final_Palette-thresholds.csv')