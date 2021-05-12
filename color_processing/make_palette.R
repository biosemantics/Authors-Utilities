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

# color synonyms
# first attempt (green brown purple only):
# green_synonyms <- c('green', 'olive')
# brown_synonyms <- c('brown', 'tan', 'copper', 'straw', 'chestnut', 'gold', 'bronze', 'tawny')
# purple_synonyms <- c('purple', 'maroon')

# second attempt:
# green_synonyms <- c('green', 'olive')
# brown_synonyms <- c('brown', 'castaneous', 'chestnut', 'copper', 'bronze', 'tan', 'tawny')
# red_synonyms <- c('red', 'maroon')
# yellow_synonyms <- c('yellow', 'golden', 'straw', 'gold')
# purple_synonyms <- c('purple')
# white_synonyms <- c('white', 'gray', 'glaucous')

# third attempt:
# green_synonyms <- c('green', 'olive')
# brown_synonyms <- c('brown', 'castaneous', 'chestnut', 'copper', 'bronze', 'tan', 'tawny',
#                     'yellow', 'golden', 'straw', 'gold') # includes yellow synonyms
# red_synonyms <- c('red', 'maroon') # includes purple
# white_synonyms <- c('white', 'gray', 'glaucous')

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


# # create new combined color data frame for purple brown
# purple_brown <- inner_join(rough_out[[2]], rough_out[[3]]) %>%
#   arrange(desc(luminance)) %>%
#   mutate(color_bin = 'purple-brown')

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


# Cluster each pure color into light, medium and dark (OLD) ----
#' Use kmeans clustering with k=3 to label light, medium, and dark 
#' for each pure color data frame (e.g. 'light brown', 'brown', or 'dark brown' value in new column)
# set.seed(42)


# km_white <- kmeans(white$L, 3) # 1 = dark, 2 = light, 3 = med
# km_green <- kmeans(green$L, 3) # 1 = dark, 2 = light, 3 = med
# km_brown <- kmeans(brown$L, 3) # 1 = dark, 2 = light, 3 = med
# km_red <- kmeans(red$L, 3) # 1 = dark, 2 = light, 3 = med
# km_yellow_green <- kmeans(yellow_green$L, 3) # 1 = dark, 2 = light, 3 = med
# km_yellow_brown <- kmeans(yellow_brown$L, 3) # 1 = dark, 2 = light, 3 = med
# 
#     # yellow and purple have to few rows to warrant clustering
# 
# # bind cluster value to respective color df
# white$cluster <- km_white$cluster 
# green$cluster <- km_green$cluster
# brown$cluster <- km_brown$cluster
# red$cluster <- km_red$cluster
# yellow_green$cluster <- km_yellow_green$cluster
# yellow_brown$cluster <- km_yellow_brown$cluster
# 
# 
# white <- white %>% arrange(desc(L))
# green <- green %>% arrange(desc(L))
# brown <- brown %>% arrange(desc(L))
# red <- red %>% arrange(desc(L))
# yellow_green <- yellow_green %>% arrange(desc(L))
# yellow_brown <- yellow_brown %>% arrange(desc(L))

# plot colors in tables using rgb data ----

# # separate into light, med, dark
# white_lmd <- get_lmd(white, km_white)
# white_l <- white_lmd[[1]]
# white_m <- white_lmd[[2]]
# white_d <- white_lmd[[3]]
# 
# green_lmd <- get_lmd(green, km_green)
# green_l <- green_lmd[[1]]
# green_m <- green_lmd[[2]]
# green_d <- green_lmd[[3]]
# 
# brown_lmd <- get_lmd(brown, km_brown)
# brown_l <- brown_lmd[[1]]
# brown_m <- brown_lmd[[2]]
# brown_d <- brown_lmd[[3]]
# 
# red_lmd <- get_lmd(red, km_red)
# red_l <- red_lmd[[1]]
# red_m <- red_lmd[[2]]
# red_d <- red_lmd[[3]]
# 
# yellow_green_lmd <- get_lmd(yellow_green, km_yellow_green)
# yellow_green_l <- yellow_green_lmd[[1]]
# yellow_green_m <- yellow_green_lmd[[2]]
# yellow_green_d <- yellow_green_lmd[[3]]
# 
# yellow_brown_lmd <- get_lmd(yellow_brown, km_yellow_brown)
# yellow_brown_l <- yellow_brown_lmd[[1]]
# yellow_brown_m <- yellow_brown_lmd[[2]]
# yellow_brown_d <- yellow_brown_lmd[[3]]


# # get tables for light, med, dark
# white_l_tab <- rgb_tab(white_l)
# white_m_tab <- rgb_tab(white_m)
# white_d_tab <- rgb_tab(white_d)

# green_l_tab <- rgb_tab(green_l)
# green_m_tab <- rgb_tab(green_m)
# green_d_tab <- rgb_tab(green_d)

# brown_l_tab <- rgb_tab(brown_l)
# brown_m_tab <- rgb_tab(brown_m)
# brown_d_tab <- rgb_tab(brown_d)

# red_l_tab <- rgb_tab(red_l)
# red_m_tab <- rgb_tab(red_m)
# red_d_tab <- rgb_tab(red_d)

# yellow_green_l_tab <- rgb_tab(yellow_green_l)
# yellow_green_m_tab <- rgb_tab(yellow_green_m)
# yellow_green_d_tab <- rgb_tab(yellow_green_d)
# 
# yellow_brown_l_tab <- rgb_tab(yellow_brown_l)
# yellow_brown_m_tab <- rgb_tab(yellow_brown_m)
# yellow_brown_d_tab <- rgb_tab(yellow_brown_d)


# purple_tab <- rgb_tab(purple)
# yellow_tab <- rgb_tab(yellow)

# # save table plots
# ggsave('plots/round_2/white_light.png',white_l_tab)
# ggsave('plots/round_2/white_med.png', white_m_tab)
# ggsave('plots/round_2/white_dark.png', white_d_tab)
# 
# ggsave('plots/round_2/green_light.png', green_l_tab)
# ggsave('plots/round_2/green_med.png', green_m_tab)
# ggsave('plots/round_2/green_dark.png', green_d_tab)
# 
# ggsave('plots/round_2/brown_light.png', brown_l_tab)
# ggsave('plots/round_2/brown_med.png', brown_m_tab)
# ggsave('plots/round_2/brown_dark.png', brown_d_tab)
# 
# ggsave('plots/round_2/red_light.png', red_l_tab)
# ggsave('plots/round_2/red_med.png', red_m_tab)
# ggsave('plots/round_2/red_dark.png', red_d_tab)
# 
# ggsave('plots/round_2/yellow_brown_light.png', yellow_brown_l_tab)
# ggsave('plots/round_2/yellow_brown_med.png', yellow_brown_m_tab)
# ggsave('plots/round_2/yellow_brown_dark.png', yellow_brown_d_tab)
# 
# ggsave('plots/round_2/yellow_green_light.png', yellow_green_l_tab)
# ggsave('plots/round_2/yellow_green_med.png', yellow_green_m_tab)
# ggsave('plots/round_2/yellow_green_dark.png', yellow_green_d_tab)
# 
# 
# 
# # remove tables from environment
# rm(white_l_tab, white_m_tab, white_d_tab,
#    green_l_tab, green_m_tab, green_d_tab,
#    brown_l_tab, brown_m_tab, brown_d_tab,
#    red_l_tab, red_m_tab, red_d_tab,
#    yellow_green_l_tab, yellow_green_m_tab, yellow_green_d_tab,
#    yellow_brown_l_tab, yellow_brown_m_tab, yellow_brown_d_tab)



# kmeans clustering for color bins ----
# silhouette method to choose k 

# 
# # plot color rgb space
# scatter3D(colors$Red, colors$Green, colors$Blue)
# 
# # green
# scatter3D(green_l$Red, green_l$Green, green_l$Blue)
# scatter3D(green_m$Red, green_m$Green, green_m$Blue)
# scatter3D(green_d$Red, green_d$Green, green_d$Blue)
# # brown
# scatter3D(brown_l$Red, brown_l$Green, brown_l$Blue)
# scatter3D(brown_m$Red, brown_m$Green, brown_m$Blue)
# scatter3D(brown_d$Red, brown_d$Green, brown_d$Blue)
# # red
# scatter3D(red_l$Red, red_l$Green, red_l$Blue)
# scatter3D(red_m$Red, red_m$Green, red_m$Blue)
# scatter3D(red_d$Red, red_d$Green, red_d$Blue)
# # white
# scatter3D(white_l$Red, white_l$Green, white_l$Blue)
# scatter3D(white_m$Red, white_m$Green, white_m$Blue)
# scatter3D(white_d$Red, white_d$Green, white_d$Blue)



# attempt at grouping based on RGB values alone: not using 'Colour' strings ----

# # split into luminance bins first
# km_lum <- kmeans(colors$luminance, 2, nstart = 25)
# 
# colors_lum <- colors %>% mutate(lum_bin = km_lum$cluster)
# colors_lum_1 <- colors_lum %>% filter(lum_bin == 1)
# colors_lum_2 <- colors_lum %>% filter(lum_bin == 2)
# 
# # plot color rgb space for different luminance clusters
# scatter3D(colors_lum_1$Red, colors_lum_1$Green, colors_lum_1$Blue)
# scatter3D(colors_lum_2$Red, colors_lum_2$Green, colors_lum_2$Blue)


# colors with new feature: red/green 
# (try to make non-brown colors more distinct for clustering)

# min_max <- function(x){
#   y <- (x - min(x))/(max(x) - min(x))
#   y
# }
# 
# colors_rg <- colors %>% mutate(r = Red/255, g = Green/255, b = Blue/255)
# colors_rg <- colors_rg %>% mutate(r_to_g = r/g)
# colors_rg <- colors_rg %>% mutate(r_norm = min_max(r), 
#                                   g_norm = min_max(g),
#                                   b_norm = min_max(b),
#                                   r_to_g_norm = min_max(r_to_g))
# 
# silhouette_score <- function(k, df){
#   km <- kmeans(df, centers = k, nstart=25)
#   ss <- silhouette(km$cluster, dist(df))
#   mean(ss[, 3])
# }
# k <- 2:6
# 
# # number of clusters for raw rgb in colors data set
# avg_sil <- sapply(k, silhouette_score, colors[,4:6])
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
# 
# # number of clusters for normalized rgb 
# avg_sil <- sapply(k, silhouette_score, select(colors_rg, r_norm:r_to_g_norm))
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
# 
# 
# km_colors <- kmeans(colors_rg[,15:18], centers = 3, nstart = 25)
# 
# colors_2 <- cbind(colors, km_colors$cluster)
# colors_2 <- colors_2 %>% rename(cluster = 'km_colors$cluster')
# 
# colors_a <- colors_2 %>% filter(cluster == 1) %>% arrange(desc(luminance))
# colors_b <- colors_2 %>% filter(cluster == 2) %>% arrange(desc(luminance))
# colors_c <- colors_2 %>% filter(cluster == 3) %>% arrange(desc(luminance))
# 
# rgb_a <- rgb_tab(colors_a)
# rgb_b <- rgb_tab(colors_b)
# rgb_c <- rgb_tab(colors_c)
# 
# ggsave('plots/rgb_a.png', rgb_a)
# ggsave('plots/rgb_b.png', rgb_b)
# ggsave('plots/rgb_c.png', rgb_c)



# # for lum cluster 1
# avg_sil <- sapply(k, silhouette_score, colors_lum_1[,4:6])
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
# # for lum cluster 2
# avg_sil <- sapply(k, silhouette_score, colors_lum_2[,4:6])
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
# 


# km_colors_lum_1 <- kmeans(colors_lum_1[,4:6], centers=2, nstart = 25)
# km_colors_lum_2 <- kmeans(colors_lum_2[,4:6], centers=2, nstart = 25)
# 
# colors_lum_1 <- cbind(colors_lum_1, km_colors_lum_1$cluster)
# colors_lum_1 <- colors_lum_1 %>% rename(cluster = 'km_colors_lum_1$cluster')
# colors_lum_2 <- cbind(colors_lum_2, km_colors_lum_2$cluster)
# colors_lum_2 <- colors_lum_2 %>% rename(cluster = 'km_colors_lum_2$cluster')
# 
# colors_lum_1_a <- colors_lum_1 %>% filter(cluster == 1)
# colors_lum_1_b <- colors_lum_1 %>% filter(cluster == 2)
# colors_lum_2_a <- colors_lum_2 %>% filter(cluster == 1)
# colors_lum_2_b <- colors_lum_2 %>% filter(cluster == 2)
# 
# rgb_lum_1_a <- rgb_tab(colors_lum_1_a)
# rgb_lum_1_b <- rgb_tab(colors_lum_1_b)
# rgb_lum_2_a <- rgb_tab(colors_lum_2_a)
# rgb_lum_2_b <- rgb_tab(colors_lum_2_b)
# 
# ggsave('plots/rgb_lum_1_a.png', rgb_lum_1_a)
# ggsave('plots/rgb_lum_1_b.png', rgb_lum_1_b)
# ggsave('plots/rgb_lum_2_a.png', rgb_lum_2_a)
# ggsave('plots/rgb_lum_2_b.png', rgb_lum_2_b)


# OLD THRESHOLDS ----

# g_yg_split b
# ry_split b
# b_yb_split b
# wb_split b

# white_l_filtered <- white_l %>% filter(b < 20)
# white_m_filtered <- white_m %>% filter(b < 20)
# white_d_filtered <- white_d %>% filter(b < 20)


# # light brown
# brown_l_km <- kmeans(brown_l[,4:6], 6)
# brown_l2 <- cbind(brown_l, brown_l_km$cluster)
# brown_l2 <- brown_l2 %>% rename(km = 'brown_l_km$cluster')

# brown
# filter greens out of brown examples by setting minimum threshold
# for 'a' channel, where positive values are more red, negative more green
# hist(brown_l$a) # quite different from distr. of m&d; most vals < 5
# hist(brown_m$a)
# hist(brown_d$a)

# brown_l_filtered <- brown_l %>% filter(a>0 & a<10 & b<30) 
# brown_m_filtered <- brown_m %>% filter(a>0 & a<10 & b<30)
# brown_d_filtered <- brown_d %>% filter(a>0 & a<10 & b<30)

# brown_l3 <- brown_l %>% filter(Green < 170 & Red > 170)
# 
# # new feature for light brown: Red/Green
# brown_l_rg <- brown_l %>% mutate(RG = Red/Green)
# hist(brown_l_rg$RG)
# brown_l_filtered <- brown_l_rg %>% filter(RG>1.1)

# green
# filter browns out of green examples by setting maximum threshold
# for 'a' channel, where positive values are more red, negative more green

# hist(green_l$a) 
# hist(green_m$a)
# hist(green_d$a)

# green_l_filtered <- green_l %>% filter(a< -5 & b <30)
# green_m_filtered <- green_m %>% filter(a< -5 & b <30)
# green_d_filtered <- green_d %>% filter(a< -5 & b <30)

# # # light green
# # green_l_db <- dbscan(green_l[,4:6], eps = 15, MinPts = 3)
# # green_l2 <- cbind(green_l, green_l_db$cluster)
# # green_l2 <- green_l2 %>% rename(db = 'green_l_db$cluster')
# 
# hist(green_l$Green)
# hist(green_l$Red)
# green_l3 <- green_l %>% filter(Green > 170 & Red < 170)
# 
# # new feature for light green: Red/Green
# green_l_rg <- green_l %>% mutate(RG = Red/Green)
# hist(green_l_rg$RG)
# 
# green_l_filtered <- green_l_rg %>% filter(RG<1)
# 
# # medium green
# green_m_km <- kmeans(green_m[,4:6], 3)
# green_m2 <- cbind(green_m, green_m_km$cluster)
# green_m2 <- green_m2 %>% rename(km = 'green_m_km$cluster')
# 
# # dark green
# green_d_db <- dbscan(green_d[,4:6], eps = 15, MinPts = 3)
# green_d2 <- cbind(green_d, green_d_db$cluster)
# green_d2 <- green_d2 %>% rename(db = 'green_d_db$cluster')


# yellow-green

# hist(yellow_green_l$b)
# hist(yellow_green_m$b)
# hist(yellow_green_d$b)
# hist(yellow_green$b)
# 
# hist(yellow_green_l$a)
# hist(yellow_green_m$a)
# hist(yellow_green_d$a)
# hist(yellow_green$a)


# minimum yellow threshold (b>=30) and not too red or too green (-15<a<-5)
# yellow_green_l_filtered <- yellow_green_l %>% filter(b>30 & a < -2 & a > -15)
# yellow_green_m_filtered <- yellow_green_m %>% filter(b>30 & a < -2 & a > -15)
# yellow_green_d_filtered <- yellow_green_d %>% filter(b>30 & a < -2)

# yellow-brown

# hist(yellow_brown$b)
# hist(yellow_brown_l$a)
# hist(yellow_brown_m$a)
# hist(yellow_brown_d$a)
# hist(yellow_brown$a)

# # minimum yellow threshold (b>=30) and not too red or too green (-15<a<-5)
# yellow_brown_l_filtered <- yellow_brown_l %>% filter(b>30 & a > -2 & a <10)
# yellow_brown_m_filtered <- yellow_brown_m %>% filter(b>30 & a > -2) # all a<10
# yellow_brown_d_filtered <- yellow_brown_d %>% filter(b>30 & a > -2) # all a<10


# red
# maximum threshold for green (a>=0)

# hist(red_l$a)
# hist(red_m$a)
# hist(red_d$a) 
# 
# red_l_filtered <- red_l %>% filter(a>10)
# red_m_filtered <- red_m %>% filter(a>10)
# red_d_filtered <- red_d %>% filter(a>10)


# visualize clusters
# fviz_cluster(brown_l_km, brown_l[,4:6]) # light brown
# fviz_cluster(green_l_db, green_l[,4:6]) # light green
# fviz_cluster(green_m_km, green_m[,4:6]) # medium green
# fviz_cluster(green_d_db, green_d[,4:6]) # dark green

# filter clusters that best maintain intended color
# brown_l_filtered <- brown_l2 %>% filter(km == 5 | km ==  2) # light brown
# green_l_filtered <- green_l2 %>% filter(db == 1) # light green
# green_m_filtered <- green_m2 %>% filter(km == 2) # medium green
# green_d_filtered <- green_d2 %>% filter(db == 0) # dark green

# # tabulate (visual)
# white_l_filtered_tab <- rgb_tab(white_l_filtered)
# white_m_filtered_tab <- rgb_tab(white_m_filtered)
# white_d_filtered_tab <- rgb_tab(white_d_filtered)
# 
# brown_l_filtered_tab <- rgb_tab(brown_l_filtered)
# brown_m_filtered_tab <- rgb_tab(brown_m_filtered)
# brown_d_filtered_tab <- rgb_tab(brown_d_filtered)
# 
# green_l_filtered_tab <- rgb_tab(green_l_filtered) # light green
# green_m_filtered_tab <- rgb_tab(green_m_filtered) # medium green
# green_d_filtered_tab <- rgb_tab(green_d_filtered) # dark green
# 
# yellow_green_l_filtered_tab <- rgb_tab(yellow_green_l_filtered)
# yellow_green_m_filtered_tab <- rgb_tab(yellow_green_m_filtered)
# yellow_green_d_filtered_tab <- rgb_tab(yellow_green_d_filtered) 
# 
# yellow_brown_l_filtered_tab <- rgb_tab(yellow_brown_l_filtered)
# yellow_brown_m_filtered_tab <- rgb_tab(yellow_brown_m_filtered)
# yellow_brown_d_filtered_tab <- rgb_tab(yellow_brown_d_filtered) 
# 
# red_l_filtered_tab <- rgb_tab(red_l_filtered)
# red_m_filtered_tab <- rgb_tab(red_m_filtered)
# red_d_filtered_tab <- rgb_tab(red_d_filtered) 
# 
# # save table
# ggsave('plots/round_2/filtered/white_light_filtered.png', white_l_filtered_tab)
# ggsave('plots/round_2/filtered/white_med_filtered.png', white_m_filtered_tab)
# ggsave('plots/round_2/filtered/white_dark_filtered.png', white_d_filtered_tab)
# 
# # brown
# ggsave('plots/round_2/filtered/brown_light_filtered.png', brown_l_filtered_tab)
# ggsave('plots/round_2/filtered/brown_med_filtered.png', brown_m_filtered_tab)
# ggsave('plots/round_2/filtered/brown_dark_filtered.png', brown_d_filtered_tab)
# 
# # green
# ggsave('plots/round_2/filtered/green_light_filtered.png', green_l_filtered_tab) 
# ggsave('plots/round_2/filtered/green_med_filtered.png', green_m_filtered_tab) 
# ggsave('plots/round_2/filtered/green_dark_filtered.png', green_d_filtered_tab) 
# 
# # yellow-green
# ggsave('plots/round_2/filtered/yellow-green_light_filtered.png', yellow_green_l_filtered_tab)
# ggsave('plots/round_2/filtered/yellow-green_med_filtered.png', yellow_green_m_filtered_tab)
# ggsave('plots/round_2/filtered/yellow-green_dark_filtered.png', yellow_green_d_filtered_tab)
# 
# # yellow-brown
# ggsave('plots/round_2/filtered/yellow-brown_light_filtered.png', yellow_brown_l_filtered_tab)
# ggsave('plots/round_2/filtered/yellow-brown_med_filtered.png', yellow_brown_m_filtered_tab)
# ggsave('plots/round_2/filtered/yellow-brown_dark_filtered.png', yellow_brown_d_filtered_tab)
# 
# # red
# ggsave('plots/round_2/filtered/red_light_filtered.png', red_l_filtered_tab)
# ggsave('plots/round_2/filtered/red_med_filtered.png', red_m_filtered_tab)
# ggsave('plots/round_2/filtered/red_dark_filtered.png', red_d_filtered_tab)


# # bind light, med, dark and select necessary columns
# 
# color_bins <- function(color_name, light, med, dark){
#   l <- light %>% mutate(color_bin = paste('light', color_name))
#   m <- med %>% mutate(color_bin = paste('medium', color_name))
#   d <- dark %>% mutate(color_bin = paste('dark', color_name))
#   df <- rbind(l,m,d)
#   df <- df %>% select(id, Species, Red, Green, Blue,
#                       L, a, b, Colour, color_bin)
#   return(df)
# }
# 
# # filtered 
# white2 <- color_bins('white', 
#                      white_l_filtered, white_m_filtered, white_d_filtered)
# brown2 <- color_bins('brown', 
#                      brown_l_filtered, brown_m_filtered, brown_d_filtered)
# green2 <- color_bins('green', 
#                      green_l_filtered, green_m_filtered, green_d_filtered)
# red2 <- color_bins('red', 
#                      red_l_filtered, red_m_filtered, red_d_filtered)
# yellow_green2 <- color_bins('yellow_green', 
#                      yellow_green_l_filtered, 
#                      yellow_green_m_filtered, 
#                      yellow_green_d_filtered)
# yellow_brown2 <- color_bins('yellow_brown', 
#                      yellow_brown_l_filtered, 
#                      yellow_brown_m_filtered, 
#                      yellow_brown_d_filtered)
# 
# # raw
# white_raw <- color_bins('white', 
#                      white_l, white_m, white_d)
# brown_raw <- color_bins('brown', 
#                      brown_l, brown_m, brown_d)
# green_raw <- color_bins('green', 
#                      green_l, green_m, green_d)
# red_raw <- color_bins('red', 
#                    red_l, red_m, red_d)
# yellow_green_raw <- color_bins('yellow_green', 
#                             yellow_green_l, 
#                             yellow_green_m, 
#                             yellow_green_d)
# yellow_brown_raw <- color_bins('yellow_brown', 
#                             yellow_brown_l, 
#                             yellow_brown_m, 
#                             yellow_brown_d)


# names vs. perception: discrepancy between color label and colors ----
# filter each color according to threshold splits determined above

# # white
# white_filtered <- white %>% filter(a> -5 & a < 0 & b < 20) %>% mutate(color = 'white')
# 
# # brown
# # don't include whites which overlap with these thresholds but belong as 'white'
# brown_filtered <- brown %>% filter(a > -2 & a < 10 & b < 32) %>% anti_join(white_filtered)
# brown_filtered_2 <- brown %>% filter(a > 10 & b > 25) 
# 
# # green
# # don't include whites which overlap with these thresholds but belong as 'white'
# green_filtered <- green %>% filter(a< -2 & b < 32) %>% anti_join(white_filtered)
# green_filtered_2 <- green %>% filter(a < -12 & b > 32)
# 
# # red
# red_filtered <- red %>% filter(a>10 & b < 25)
# 
# # yellow-brown
# yellow_brown_filtered <- yellow_brown %>% filter(a > -2 & a < 10 & b > 32)
# 
# # yellow-green
# yellow_green_d_filtered <- yellow_green %>% filter(a < -2 & b > 32 & a > -12)


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

# remove_outliers <- function(color){
#   outliers <- boxplot(color$a, plot=FALSE)$out
#   # it doesn't return anything at all if there are no outliers to remove
#   # to prevent this:
#   keep_a <- if(length(outliers>0)){
#     color[-which(color$a %in% outliers),]
#   } else {
#     color
#   }
#   outliers <- boxplot(color$b, plot=FALSE)$out
#   keep_b <- if(length(outliers>0)){
#     color[-which(color$b %in% outliers),]
#   } else {
#     color
#   }
#   keep <- inner_join(keep_a, keep_b)
#   return(keep)
# }

# 
# brown_keep <- remove_outliers(brown)
# green_keep <- remove_outliers(green)
# red_keep <- remove_outliers(red)
# white_keep <- remove_outliers(white)
# yellow_brown_keep <- remove_outliers(yellow_brown)
# yellow_green_keep <- remove_outliers(yellow_green)
# 
# colors_labeled_keep <- bind_rows(brown_keep, green_keep, red_keep,
#                                  white_keep, yellow_brown_keep, yellow_green_keep)
# 
# colors_labeled_keep$label <- as.factor(colors_labeled_keep$label)


# ggplot(colors_labeled_keep, aes(x = label, y = a))+
#   geom_boxplot()
# ggplot(colors_labeled_keep, aes(x = label, y = b))+
#   geom_boxplot()
# 

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



# # OLD
# # prep data
# colors_filtered <- rbind(white2, brown2, green2, red2, yellow_green2, yellow_brown2)
# colors_filtered <- colors_filtered %>%
#   mutate(color = word(color_bin, 2), L_bin = word(color_bin, 1))
# 
# colors_raw <- rbind(white_raw, brown_raw, green_raw, red_raw,
#                     yellow_green_raw, yellow_brown_raw)
# colors_raw <- colors_raw %>%
#   mutate(color = word(color_bin, 2), L_bin = word(color_bin, 1))
# 
# # # get sample colors for color scheme of visualization
# # sample_color <- function(x) {
# #   sample_rgb <- x[round(nrow(x)/2,0),]
# #   sample <- rgb(sample_rgb$Red/255, sample_rgb$Green/255, sample_rgb$Blue/255)
# #   return(sample)
# # }
# # 
# # sample_green <- sample_color(green2)
# # sample_brown <- sample_color(brown2)
# # sample_white <- sample_color(white2)
# # sample_red <- sample_color(red2)
# # sample_yellow_green <- sample_color(yellow_green2)
# # sample_yellow_brown <- sample_color(yellow_brown2)
# 
# sample_green <- centroid_color(green2)
# sample_brown <- centroid_color(brown2)
# sample_white <- centroid_color(white2)
# sample_red <- centroid_color(red2)
# sample_yellow_green <- centroid_color(yellow_green2)
# sample_yellow_brown <- centroid_color(yellow_brown2)
# 
# sample_green_raw <- centroid_color(green_raw)
# sample_brown_raw <- centroid_color(brown_raw)
# sample_white_raw <- centroid_color(white_raw)
# sample_red_raw <- centroid_color(red_raw)
# sample_yellow_green_raw <- centroid_color(yellow_green_raw)
# sample_yellow_brown_raw <- centroid_color(yellow_brown_raw)
# 
# # change name of sample color to "sample" to use as reference point
# colors_filtered_marked <- mark_samples(colors_filtered, sample_green, sample_brown, sample_red, 
#              sample_white, sample_yellow_green, sample_yellow_brown)
# 
# colors_raw_marked <- mark_samples(colors_raw, sample_green_raw, sample_brown_raw, sample_red_raw,
#                                   sample_white_raw, sample_yellow_green_raw, sample_yellow_brown_raw)
# 
# 
# # plot 
# x_min <- -25
# x_max <- 30
# y_min <- -5
# y_max <- 60
# 
# color_space_filtered <- ggplot(colors_filtered_marked, aes(x=a, y=b, color=color)) +
#   geom_point(alpha=3/4) +
#   # facet_grid(rows=vars(color), cols=vars(L_bin)) +
#   scale_color_manual(values=c(sample_brown, sample_green, sample_red, "red", sample_white,
#                               sample_yellow_brown, sample_yellow_green)) +
#   ylab('Blue-Yellow') +
#   xlab('Green-Red') +
#   ylim(c(y_min, y_max)) +
#   xlim(c(x_min, x_max)) +
#   geom_rect(xmin = 10, xmax = x_max, ymin = y_min, ymax = y_max, color = sample_red, alpha = 0) + 
#   geom_rect(xmin = 0, xmax = 10, ymin = y_min, ymax = 30, color = sample_brown, alpha = 0) +
#   geom_rect(xmin = x_min, xmax = -5, ymin = y_min, ymax = 30, color = sample_green, alpha = 0) +
#   geom_rect(xmin = -15, xmax = -2, ymin = 30, ymax = y_max, color = sample_yellow_green, alpha = 0) +
#   geom_rect(xmin  =  -2, xmax = 10, ymin = 30, ymax = y_max, color = sample_yellow_brown, alpha = 0) +
#   theme(legend.text = element_text(size=16),
#         legend.title = element_text(size=16),
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 12))
# 
# color_space_raw <- ggplot(colors_raw_marked, aes(x=a, y=b, color=color)) +
#   geom_point(alpha=3/4) + 
#   # facet_grid(rows=vars(color), cols=vars(L_bin)) +
#   scale_color_manual(values=c(sample_brown_raw, sample_green_raw, sample_red_raw,
#                               "red",
#                               sample_white_raw, sample_yellow_brown_raw,
#                               sample_yellow_green_raw)) +
#   ylab('Blue-Yellow') +
#   xlab('Green-Red') +
#   ylim(c(y_min, y_max)) +
#   xlim(c(x_min, x_max)) +
#   theme(legend.text = element_text(size=16),
#         legend.title = element_text(size=16),
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 12))
# 
# # Explore adding back discards
# discards <- rbind(brown_discards, green_discards, red_discards, white_discards,
#                   yellow_brown_discards, yellow_green_discards)
# discards$color <- 'discard'
#   
# 
# # plot discarded colors on top of color_space_filtered
# color_space_discards <- ggplot(colors_filtered_marked, aes(x=a, y=b, color=color)) +
#   geom_point(alpha=3/4) +
#   geom_point(data=discards, alpha=1/4) +
#   # facet_grid(rows=vars(color), cols=vars(L_bin)) +
#   scale_color_manual(values=c(sample_brown,'gray', sample_green, sample_red, "red", sample_white, 
#                               sample_yellow_brown, sample_yellow_green)) +
#   ylab('Blue-Yellow') +
#   xlab('Green-Red') +
#   ylim(c(y_min, y_max)) +
#   xlim(c(x_min, x_max)) +
#   geom_rect(xmin = 10, xmax = x_max, ymin = y_min, ymax = y_max, color = sample_red, alpha = 0) + 
#   geom_rect(xmin = 0, xmax = 10, ymin = y_min, ymax = 30, color = sample_brown, alpha = 0) +
#   geom_rect(xmin = x_min, xmax = -5, ymin = y_min, ymax = 30, color = sample_green, alpha = 0) +
#   geom_rect(xmin = -15, xmax = -2, ymin = 30, ymax = y_max, color = sample_yellow_green, alpha = 0) +
#   geom_rect(xmin  =  -2, xmax = 10, ymin = 30, ymax = y_max, color = sample_yellow_brown, alpha = 0) +
#   theme(legend.text = element_text(size=16),
#         legend.title = element_text(size=16),
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 12)) +
#   geom_rug(data=discards, inherit.aes=F, aes(x=a, y=b), alpha=1/10)
# 
# # export color spaces side by side 
# color_spaces <- ggarrange(color_space_raw, color_space_filtered, color_space_discards, 
#                           labels=c("Raw", "Filtered", "Discards Included"),
#                           nrow=1)
# 
# ggsave('./plots/color_spaces.png', color_spaces, width = 30, height = 8)
# 
# # export to excel ----
# 
# # finish prepping data to export
# # get colors removed when making colors_filtered
# green_removed <- anti_join(green_raw, green2)
# brown_removed <- anti_join(brown_raw, brown2)
# red_removed <- anti_join(red_raw, red2)
# white_removed <- anti_join(white_raw, white2)
# yellow_green_removed <- anti_join(yellow_green_raw, yellow_green2)
# yellow_brown_removed <- anti_join(yellow_brown_raw, yellow_brown2)
# 
# 
# # make a tab for the gigantic colors df to keep provenance for all colors using unique id
# write.xlsx(colors, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Master", 
#            append=TRUE)
# # tabs for green, brown, red, white
# write.xlsx(green2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Green", 
#            append=TRUE)
# write.xlsx(brown2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Brown", 
#            append=TRUE)
# write.xlsx(red2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Red", 
#            append=TRUE)
# write.xlsx(white2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="White", 
#            append=TRUE)
# write.xlsx(yellow_green2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Green", 
#            append=TRUE)
# write.xlsx(yellow_brown2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Brown", 
#            append=TRUE)
# 
# # tabs for each color: examples removed when filtering using L*a*b* space thresholds
# write.xlsx(green_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Green removed (Lab)", 
#            append=TRUE)
# write.xlsx(brown_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Brown removed (Lab)", 
#            append=TRUE)
# write.xlsx(red_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Red removed (Lab)", 
#            append=TRUE)
# write.xlsx(white_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="White removed (Lab)", 
#            append=TRUE)
# write.xlsx(yellow_green_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Green removed (Lab)", 
#            append=TRUE)
# write.xlsx(yellow_brown_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Brown removed (Lab)", 
#            append=TRUE)
# 
# # tabs for each color: examples discarded when making "pure" colors
# write.xlsx(green_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Green Discards", 
#            append=TRUE)
# write.xlsx(brown_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Brown Discards", 
#            append=TRUE)
# write.xlsx(red_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Red Discards", 
#            append=TRUE)
# write.xlsx(white_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="White Discards", 
#            append=TRUE)
# write.xlsx(yellow_green_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Green Discards", 
#            append=TRUE)
# write.xlsx(yellow_brown_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Brown Discards", 
#            append=TRUE)




