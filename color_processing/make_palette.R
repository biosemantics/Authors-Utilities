# Make color palettes
# Noah Giebink

# packages
library(tm)
library(ggpubr)
library(tidyverse)
library(cluster)
library(plot3D)
library(fpc)
library(factoextra)
library(ggpubr)
library(splitTools)
library(e1071)
library(caret)
library(forcats)

# Source functions ----
source('palette_functions.R')

# Data prep: call functions ----
# read in data 
colors_list <- read_excel_allsheets('data/colour_between_species_2021Mar7.xlsx')
colors_list <- colors_list[c('Leaf', 'Male Scale', 'Perigynium', 'Female Scale', 'Cataphyll')]

# remove scale measurements from colors_list
colors_list <- colors_list[-c(2,4)]

# get unique ids
colors <- unique_id(colors_list)

# add features ----
# add CIELab for lightness (L channel),
# distinguishing red vs. greenness (a channel: 'red-green)
# and blue vs. yellowness (b channel: 'blue-yellow')

Lab_features <- convertColor(colors[4:6], 'sRGB', 'Lab', scale.in = 255)
colors <- cbind(colors, Lab_features)

# choose synonyms ----

# explore color options with highest term frequency in set

terms <- colors$Colour %>% str_replace_all('-', ' ')
term_freq <- termFreq(terms)
tf <- as.data.frame(term_freq)
tf <- tf %>% arrange(desc(term_freq))

term_freq2 <- termFreq(colors$Colour)
tf2 <- as.data.frame(term_freq2)
tf2 <- tf2 %>% arrange(desc(term_freq2))

# fourth attempt (include yellow-green and yellow-brown)
green_synonyms <- c('green', 'olive')
brown_synonyms <- c('brown', 'castaneous', 'chestnut', 'copper', 'bronze', 'tan', 'tawny')
red_synonyms <- c('red', 'maroon') # includes purple
white_synonyms <- c('white', 'gray', 'glaucous')
yellow_green_synonyms <- c('yellow-green', 'green-yellow', 'yellow', 'golden', 'straw', 'gold')
yellow_brown_synonyms <- c('yellow-brown', 'yellow', 'golden', 'straw', 'gold')


# separate df's into colors based on 'Colour' descriptions ----

# get rough color data frames
rough_out <- rough_colors(colors, green_synonyms,
                          yellow_green_synonyms,
                          red_synonyms,
                          white_synonyms,
                          yellow_brown_synonyms,
                          brown_synonyms)
names(rough_out) <- c('green','yellow-green','red','white', 'yellow-brown','brown')


# get pure color data frames
pure <- pure_colors(rough_out) 
# yellow-green: only filter out brown
pure_yg <- pure_colors(rough_out[c(2,6)])
# yellow-brown: only filter out green
pure_yb <- pure_colors(rough_out[c(1,5)])


green <- pure[[1]]
red <- pure[[3]]
white <- pure[[4]]
brown <- pure[[6]]
yellow_brown <- pure_yb[[2]]
yellow_green <- pure_yg[[1]]


# get discarded rows for reference
get_discards <- function(rough, pure){
  discards <- filter(rough, !(rough$Colour %in% pure$Colour))
  return(discards)
}

green_discards <- get_discards(rough_out$green, green)
yellow_green_discards <- get_discards(rough_out$`yellow-green`, yellow_green)
red_discards <- get_discards(rough_out$red, red)
white_discards <- get_discards(rough_out$white, white)
yellow_brown_discards <- get_discards(rough_out$`yellow-brown`, yellow_brown)
brown_discards <- get_discards(rough_out$brown, brown)


# Manual thresholds on all labeled colors ---- 
white <- white %>% mutate(label = 'white')
brown <- brown %>% mutate(label = 'brown')
green <- green %>% mutate(label = 'green')
red <- red %>% mutate(label = 'red')
yellow_brown <- yellow_brown %>% mutate(label = 'yellow-brown')
yellow_green <- yellow_green %>% mutate(label = 'yellow-green')

# bind together 
colors_labeled <- bind_rows(white, brown, green, red, yellow_green, yellow_brown)

# Find thresholds 

# RED-GREEN AND YELLOW-BLUE THRESHOLDS 

# FULL DATA SET

rg_bins <- mutate(colors_labeled, bin = if_else(a < -2, 'more green', 'more red'))
yb_bins <- mutate(colors_labeled, bin = if_else(b < 32, 'more blue', 'more yellow'))

rg_split <- ggplot(rg_bins, aes(x=a, fill=bin))+
  geom_histogram()+
  scale_fill_manual(values=c(centroid_color(filter(rg_bins, bin=='more green')),
                             centroid_color(filter(rg_bins, bin=='more red'))))+
  labs(x = 'Green-Red', y = 'Count', fill = 'Bin')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 

yb_split <- ggplot(yb_bins, aes(x=b, fill=bin))+
  geom_histogram()+
  scale_fill_manual(values=c(centroid_color(filter(yb_bins, bin=='more blue')),
                             centroid_color(filter(yb_bins, bin=='more yellow'))))+
  labs(x = 'Blue-Yellow', y = 'Count', fill = 'Bin')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 


# 'MORE GREEN' SIDE

g_yg_bins <- filter(colors_labeled, a < -2) %>% mutate(bin = if_else(b < 32,
                                                                      'more green',
                                                                      'more yellow'))

g_yg_split <- ggplot(g_yg_bins, aes(x=b, fill=bin))+
  geom_histogram(binwidth = 2)+
  scale_fill_manual(values=c(centroid_color(filter(g_yg_bins, bin=='more green')),
                             centroid_color(filter(g_yg_bins, bin=='more yellow'))))+
  labs(x = 'Blue-Yellow', y = 'Count', fill = 'Bin')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 


# for the green-looking values to the "left" of yellow-green 
g_yg_bins_2 <- mutate(g_yg_bins, bin = if_else(a < - 12, 'more green', 'more yellow'))

g_yg_split_2 <- ggplot(g_yg_bins_2, aes(x=a, fill=bin))+
  geom_histogram(binwidth = 2)+
  scale_fill_manual(values=c(centroid_color(filter(g_yg_bins, bin=='more green')),
                             centroid_color(filter(g_yg_bins, bin=='more yellow'))))+
  labs(x = 'Green-Red', y = 'Count', fill = 'Bin')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 


# red vs. brown 


rb_bins <- filter(colors_labeled, a > -2) %>% mutate(bin = if_else(a > 10, 'more red', 
                                                                    'more brown'))

rb_split <- ggplot(rb_bins, aes(x=a, fill=bin))+
  geom_histogram()+
  scale_fill_manual(values=c(centroid_color(filter(rb_bins, bin=='more brown')),
                             centroid_color(filter(rb_bins, bin=='more red'))))+
  labs(x = 'Green-Red', y = 'Count', fill = 'Bin')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 


# red vs. yellow (when a > 10)

ry_bins <- filter(rb_bins, bin == 'more red') %>% mutate(bin = if_else(b > 25, 'more yellow',
                                                                       'more red'))

ry_split <- ggplot(ry_bins, aes(x=b, fill=bin))+
  geom_histogram(binwidth = 5)+
  scale_fill_manual(values=c(centroid_color(filter(ry_bins, bin=='more red')),
                             centroid_color(filter(ry_bins, bin=='more yellow'))))+
  labs(x = 'Blue-Yellow', y = 'Count', fill = 'Bin')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 


# brown vs yellow-brown
b_yb_bins <- filter(colors_labeled, a > -2 & a < 10) %>% mutate(bin = if_else(b < 32,
                                                                               'more brown',
                                                                               'more yellow'))

b_yb_split <- ggplot(b_yb_bins, aes(x=b, fill=bin))+
  geom_histogram(binwidth = 2)+
  scale_fill_manual(values=c(centroid_color(filter(b_yb_bins, bin=='more brown')),
                             centroid_color(filter(b_yb_bins, bin=='more yellow'))))+
  labs(x = 'Blue-Yellow', y = 'Count', fill = 'Bin')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 


# INDIVIDUAL COLOR BINS
# white
# to appear "white"(including gray and  black), set cutoff
# for b channel (lower values are more blue, less yellow)


white_bins_a <- mutate(white, bin = if_else(a< -5, 'green', 
                                            if_else(a > 0, 'brown',
                                                    'white')))
white_bins_b <- mutate(white, bin = if_else(b < 20, 'white', 'other'))

w_split <- ggplot(white_bins_a, aes(x=a, fill=bin))+
  geom_histogram(binwidth=2.5)+
  scale_x_continuous(breaks = seq(-15,10,5)) +
  scale_fill_manual(values = c(centroid_color(filter(white_bins_a, bin=='brown')),
                               centroid_color(filter(white_bins_a, bin=='green')),
                               centroid_color(filter(white_bins_a, bin=='white')))) +
  labs(x = 'Green-Red', y = 'Count', fill = 'Bin')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 

wb_split <- ggplot(white_bins_b, aes(x=b, fill=bin))+
  geom_histogram(binwidth=5)+
  scale_x_continuous(breaks = seq(10,45,5)) +
  scale_fill_manual(values = c(centroid_color(filter(white_bins_b, bin=='other')),
                               centroid_color(filter(white_bins_b, bin=='white')))) +
  labs(x = 'Blue-Yellow', y = 'Count', fill = 'Bin')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 

splits_main <- ggarrange(rg_split, yb_split, nrow=1)
splits_brownish <- ggarrange(b_yb_split, rb_split, ry_split, nrow=1)
splits_greenish <- ggarrange(g_yg_split, g_yg_split_2, nrow=1)
splits_whitish <- ggarrange(w_split, wb_split, nrow = 1)





# split using thresholds function
colors_labeled <- thresholds(colors_labeled)

colors_labeled$label <- as.factor(colors_labeled$label)
colors_labeled$color <- as.factor(colors_labeled$color)


# SVM ----

set.seed(42)

# remove outlier points
ggplot(colors_labeled, aes(x = label, y = a))+
  geom_boxplot()
ggplot(colors_labeled, aes(x = label, y = b))+
  geom_boxplot()


# remove one extreme (green?) value where b > 80
colors_labeled2 <- colors_labeled %>% filter(b < 80)
boxplot(colors_labeled$b)

# stratified train test val split
split <- partition(colors_labeled2$label, p = c(train = 0.8, test = 0.2))
train <- colors_labeled2[split$train,]
test <- colors_labeled2[split$test,]
# select green-red and blue-yellow features, label
train <- train %>% select(a, b, label)
test <- test %>% select(a, b, label)


# down sample to help with class imbalance
# train_down <- downSample(train, train$label)
# train_down <- select(train_down, a, b, label)
# down sample without white included
train_nowhite <- train %>% filter(label != 'white')
train_nowhite <- droplevels(train_nowhite)
train_down_nowhite <- downSample(train_nowhite, train_nowhite$label)
train_down_nowhite <- select(train_down_nowhite, a, b, label)

test_nowhite <- test %>% filter(label != 'white')
test_nowhite <- droplevels(test_nowhite)

# # regular set
# svm_radial <- svm(label~., data = train, type='C-classification', kernel='radial', cost=1)
# svm_linear <- svm(label~., data = train, type='C-classification', kernel='linear', cost=1)
# # down sampled set
# svm_radial_down <- svm(label~., data = train_down, type='C-classification', kernel='radial', cost=1)

# down sampled and no white
svm_radial_down_nowhite <- svm(label~., data = train_down_nowhite, type='C-classification', kernel='radial', cost=0.5)
svm_linear_down_nowhite <- svm(label~., data = train_down_nowhite, type='C-classification', kernel='linear', cost=0.5)

# plot(svm_radial, train, b~a)
# plot(svm_linear, train, b~a)
# plot(svm_radial_down, train_down, b~a)
svm_radial_plot <- plot(svm_radial_down_nowhite, train_down_nowhite, b~a)
svm_linear_plot <- plot(svm_linear_down_nowhite, train_down_nowhite, b~a)

 # performance on test

pred_rad <- predict(svm_radial_down_nowhite, test_nowhite)
pred_lin <- predict(svm_linear_down_nowhite, test_nowhite)

score_rad <- score(pred_rad, test_nowhite$label)
score_lin <- score(pred_lin, test_nowhite$label)

mean_f1_rad <- mean(score_rad$f1)
mean_f1_lin <- mean(score_lin$f1)

# also compare with thresholds predictions score
colors_labeled_test <- left_join(test, colors_labeled)

score_thresholds <- score(colors_labeled_test$color, colors_labeled_test$label)
mean_f1_thresholds <- mean(score_thresholds$f1)
# not counting white
score_thresholds_nowhite <- score_thresholds[c(1:3,5:6),]
mean_f1_thresholds_nowhite  <- mean(score_thresholds_nowhite$f1)


# train linear svm on full set (downsampled, no white, green outlier removed)
colors_train <- colors_labeled %>% filter(b < 80 & label != 'white')
colors_train <- droplevels(colors_train)
colors_train <- colors_train %>% select(a,b,label)
colors_train <- downSample(colors_train, colors_train$label)
colors_train <- colors_train %>% select(a,b,label)

# model
svm_linear <- svm(label~., data = colors_train, type='C-classification', kernel='linear', cost=0.5)

# model classification plot
plot(svm_linear, colors_train, b~a)

# classify colors using trained linear svm
pred <- predict(svm_linear, select(colors_labeled, a,b))
color_svm <- data.frame(pred)
colors_labeled_svm <- bind_cols(colors_labeled, color_svm)
colors_labeled_svm <- rename(colors_labeled_svm, color_svm = pred)

# # save models, performance, and complete prediction matrix
# saveRDS(svm_linear, "model/svm_linear.rds") # final model
# saveRDS(svm_radial_down_nowhite, "model/svm_radial_dev.rds")
# saveRDS(svm_linear_down_nowhite, "model/svm_linear_dev.rds")
# saveRDS(colors_labeled_svm, "data/colors_with_predictions.rds")
# saveRDS(score_lin, 'performance/score_lin.rds')
# saveRDS(score_thresholds, 'performance/score_thresholds.rds')

# # read model back in
# svm_linear <- readRDS("model/svm_linear.rds")


# visualizations ----

# mark point used for color sample
colors_svm <- colors_labeled_svm %>% select(-c(label, color))
colors_svm <- colors_svm %>% rename(color=color_svm)
colors_svm$color <- as.character(colors_svm$color)
colors_svm_marked <- mark_samples(colors_svm,
                                          centroid_color(filter(colors_svm, color == 'brown')),
                                          centroid_color(filter(colors_svm, color == 'green')),
                                          centroid_color(filter(colors_svm, color == 'red')),
                                          centroid_color(filter(colors_svm, color == 'yellow-brown')),
                                          centroid_color(filter(colors_svm, color == 'yellow-green')))
colors_labeled$color <- as.character(colors_labeled$color)
colors_marked <- mark_samples(colors_labeled,
                                  centroid_color(filter(colors_labeled, color == 'brown')),
                                  centroid_color(filter(colors_labeled, color == 'green')),
                                  centroid_color(filter(colors_labeled, color == 'red')),
                                  centroid_color(filter(colors_labeled, color == 'white')),
                                  centroid_color(filter(colors_labeled, color == 'yellow-brown')),
                                  centroid_color(filter(colors_labeled, color == 'yellow-green')))
colors_labels <- colors_labeled_svm %>% select(-c(color_svm, color))
colors_labels <- colors_labels %>% rename(color=label)
colors_labels$color <- as.character(colors_labels$color)
colors_labels_marked <- mark_samples(colors_labels,
                                     centroid_color(filter(colors_labels, color == 'brown')),
                                     centroid_color(filter(colors_labels, color == 'green')),
                                     centroid_color(filter(colors_labels, color == 'red')),
                                     centroid_color(filter(colors_labels, color == 'white')),
                                     centroid_color(filter(colors_labels, color == 'yellow-brown')),
                                     centroid_color(filter(colors_labels, color == 'yellow-green')))

# spatial distribution of author descriptions & color classification
# in RGB (x=R, y=G) vs. Lab space

# which two dimensions are most useful in RGB space?
# these will be the two with the widest variance
# which can be visually inspected via violin plots.
# answer: red and green

rgb_tidy <- pivot_longer(colors, 
                         cols=c(Red, Green, Blue), 
                         names_to = 'axis', values_to = 'value')
rgb_tidy <- factor(rgb_tidy$axis)

rgb_tidy <- rgb_tidy %>% mutate(axis = fct_relevel(axis, 'Red', 'Green', 'Blue'))

rgb_box <- ggplot(rgb_tidy, aes(x=axis, y=value, fill=axis))+
  geom_violin()+
  scale_fill_manual(values=c('red','green','blue'))+
  labs(x = 'Axis', y = 'Value', fill = 'Axis')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 

# rgb vs lab, colors vs. labels visualization
x_min <- -25
x_max <- 30
y_min <- -5
y_max <- 60


lab_svm <- ggplot(colors_svm,  
                  aes(a, b, color = color))+
  geom_point(size = 2)+
  scale_color_manual(values=c(centroid_color(filter(colors_svm, color_svm=='brown')),
                              centroid_color(filter(colors_svm, color_svm=='green')),
                              centroid_color(filter(colors_svm, color_svm=='red')),
                              'red',
                              # centroid_color(filter(colors_svm, color_svm=='white')),
                              centroid_color(filter(colors_svm, color_svm=='yellow-brown')),
                              centroid_color(filter(colors_svm, color_svm=='yellow-green'))))+
  geom_point(data=filter(colors_svm_marked, color=='sample'), size = 2)+
  ylab('Blue-Yellow') +
  xlab('Green-Red') +
  ylim(c(y_min, y_max)) +
  xlim(c(x_min, x_max)) +
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 


lab_thresholds <- ggplot(colors_labeled,  
                     aes(a, b, color = color))+
  geom_point(size = 2)+
  scale_color_manual(values=c(centroid_color(filter(colors_labeled, color=='brown')),
                              centroid_color(filter(colors_labeled, color=='green')),
                              centroid_color(filter(colors_labeled, color=='red')),
                              'red',
                              centroid_color(filter(colors_labeled, color=='white')),
                              centroid_color(filter(colors_labeled, color=='yellow-brown')),
                              centroid_color(filter(colors_labeled, color=='yellow-green'))))+
  geom_point(data = filter(colors_marked, color == 'sample'), size = 2)+
  ylab('Blue-Yellow') +
  xlab('Green-Red') +
  ylim(c(y_min, y_max)) +
  xlim(c(x_min, x_max)) +
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 

lab_labels <- ggplot(colors_labels_marked, 
                     aes(a, b, color = color))+
  geom_point(size = 2)+
  scale_color_manual(values=c(centroid_color(filter(colors_labels, color=='brown')),
                              centroid_color(filter(colors_labels, color=='green')),
                              centroid_color(filter(colors_labels, color=='red')),
                              'red',
                              centroid_color(filter(colors_labels, color=='white')),
                              centroid_color(filter(colors_labels, color=='yellow-brown')),
                              centroid_color(filter(colors_labels, color=='yellow-green'))))+
  geom_point(data = filter(colors_labels_marked, color == 'sample'), size = 2)+
  ylab('Blue-Yellow') +
  xlab('Green-Red') +
  ylim(c(y_min, y_max)) +
  xlim(c(x_min, x_max)) +
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 


# Lab space vs. rgb space example

rgb_thresholds <- ggplot(colors_labeled, 
                     aes(Red, Green, color = color))+
  geom_point(size = 2)+
  scale_color_manual(values=c(centroid_color(filter(colors_labeled, color=='brown')),
                              centroid_color(filter(colors_labeled, color=='green')),
                              centroid_color(filter(colors_labeled, color=='red')),
                              'red',
                              centroid_color(filter(colors_labeled, color=='white')),
                              centroid_color(filter(colors_labeled, color=='yellow-brown')),
                              centroid_color(filter(colors_labeled, color=='yellow-green'))))+
  geom_point(data = filter(colors_marked, color == 'sample'), size = 2)+
  ylab('Green Value') +
  xlab('Red Value') +
  # ylim(c(y_min, y_max)) +
  # xlim(c(x_min, x_max)) +
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10))  

rgb_labels <- ggplot(colors_labels_marked, 
                     aes(Red, Green, color = color))+
  geom_point(size = 2)+
  scale_color_manual(values=c(centroid_color(filter(colors_labels, color=='brown')),
                              centroid_color(filter(colors_labels, color=='green')),
                              centroid_color(filter(colors_labels, color=='red')),
                              'red',
                              centroid_color(filter(colors_labels, color=='white')),
                              centroid_color(filter(colors_labels, color=='yellow-brown')),
                              centroid_color(filter(colors_labels, color=='yellow-green'))))+
  geom_point(data = filter(colors_labels_marked, color == 'sample'), size = 2)+
  ylab('Green Value') +
  xlab('Red Value') +
  # ylim(c(y_min, y_max)) +
  # xlim(c(x_min, x_max)) +
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 

rgb_svm <- ggplot(colors_svm_marked,  
                  aes(Red, Green, color = color))+
  geom_point(size = 2)+
  scale_color_manual(values=c(centroid_color(filter(colors_svm, color_svm=='brown')),
                              centroid_color(filter(colors_svm, color_svm=='green')),
                              centroid_color(filter(colors_svm, color_svm=='red')),
                              'red',
                              # centroid_color(filter(colors_svm, color_svm=='white')),
                              centroid_color(filter(colors_svm, color_svm=='yellow-brown')),
                              centroid_color(filter(colors_svm, color_svm=='yellow-green'))))+
  geom_point(data=filter(colors_svm_marked, color=='sample'), size=2)+
  ylab('Green Value') +
  xlab('Red Value') +
  # ylim(c(y_min, y_max)) +
  # xlim(c(x_min, x_max)) +
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 
  

# arrange all of these plots into a big grid

spaces <- ggarrange(rgb_thresholds, lab_thresholds,
                    rgb_svm, lab_svm,
                    rgb_labels, lab_labels,
                    ncol=2, nrow=3)



# calculate proportion of each color label present in each actual color set
# label test 
label_test <- colors_labeled %>% group_by(color, label, .drop=FALSE) %>% tally()
label_totals <- label_test %>% group_by(color) %>% summarise(total = sum(n))
label_test <- right_join(label_test, label_totals, by='color')
label_test <- label_test %>% mutate(proportion = n/total)

label_test_plot <- ggplot(label_test, aes(x = color, y = proportion, fill = label))+
  geom_bar(position='dodge', stat='identity', color='black') +
  scale_fill_manual(values=c(centroid_color(filter(colors_labeled, color=='brown')),
                             centroid_color(filter(colors_labeled, color=='green')),
                             centroid_color(filter(colors_labeled, color=='red')),
                             centroid_color(filter(colors_labeled, color=='white')),
                             centroid_color(filter(colors_labeled, color=='yellow-brown')),
                             centroid_color(filter(colors_labeled, color=='yellow-green'))))+
  labs(x='Color Classification (Thresholds)', y='Frequency', fill='Author Label')+
  theme_pubr()+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10)) 

# color test
color_test <- colors_labeled %>% group_by(label, color, .drop=FALSE) %>% tally()
color_totals <- color_test %>% group_by(label) %>% summarise(total = sum(n))
color_test <- right_join(color_test, color_totals, by='label')
color_test <- color_test %>% mutate(proportion = n/total)

color_test_plot <- ggplot(color_test, aes(x = label, y = proportion, fill = color))+
  geom_bar(position='dodge', stat='identity', color='black') +
  scale_fill_manual(values=c(centroid_color(filter(colors_labeled, color=='brown')),
                             centroid_color(filter(colors_labeled, color=='green')),
                             centroid_color(filter(colors_labeled, color=='red')),
                             centroid_color(filter(colors_labeled, color=='white')),
                             centroid_color(filter(colors_labeled, color=='yellow-brown')),
                             centroid_color(filter(colors_labeled, color=='yellow-green'))))+
  labs(x='Author Label Category', y='Frequency', fill='Color Classification')+
  theme_pubr()

# save visualizations (already run) ----
# ggsave('plots/final/splits_main.png', splits_main, width = 10, height = 5)
# ggsave('plots/final/splits_brownish.png', splits_brownish, width = 15, height = 5)
# ggsave('plots/final/splits_greenish.png', splits_greenish, width = 10, height = 5)
# ggsave('plots/final/splits_whitish.png', splits_whitish, width = 10, height = 5)
# ggsave('plots/final/rgb_axis_distributions.png', rgb_box, width = 5, height = 5)
# ggsave('plots/final/spaces_rgb_vs_lab.png', spaces, width = 10, height = 12)
# ggsave('plots/final/label_vs_color.png', label_test_plot, width = 10, height = 5)