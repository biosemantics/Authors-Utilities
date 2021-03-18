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


# Cluster each pure color into light, medium and dark ----
#' Use kmeans clustering with k=3 to label light, medium, and dark 
#' for each pure color data frame (e.g. 'light brown', 'brown', or 'dark brown' value in new column)
set.seed(42)


km_white <- kmeans(white$L, 3) # 1 = dark, 2 = light, 3 = med
km_green <- kmeans(green$L, 3) # 1 = dark, 2 = light, 3 = med
km_brown <- kmeans(brown$L, 3) # 1 = dark, 2 = light, 3 = med
km_red <- kmeans(red$L, 3) # 1 = dark, 2 = light, 3 = med
km_yellow_green <- kmeans(yellow_green$L, 3) # 1 = dark, 2 = light, 3 = med
km_yellow_brown <- kmeans(yellow_brown$L, 3) # 1 = dark, 2 = light, 3 = med

    # yellow and purple have to few rows to warrant clustering

# bind cluster value to respective color df
white$cluster <- km_white$cluster 
green$cluster <- km_green$cluster
brown$cluster <- km_brown$cluster
red$cluster <- km_red$cluster
yellow_green$cluster <- km_yellow_green$cluster
yellow_brown$cluster <- km_yellow_brown$cluster


white <- white %>% arrange(desc(L))
green <- green %>% arrange(desc(L))
brown <- brown %>% arrange(desc(L))
red <- red %>% arrange(desc(L))
yellow_green <- yellow_green %>% arrange(desc(L))
yellow_brown <- yellow_brown %>% arrange(desc(L))

# plot colors in tables using rgb data ----

# separate into light, med, dark
white_lmd <- get_lmd(white, km_white)
white_l <- white_lmd[[1]]
white_m <- white_lmd[[2]]
white_d <- white_lmd[[3]]

green_lmd <- get_lmd(green, km_green)
green_l <- green_lmd[[1]]
green_m <- green_lmd[[2]]
green_d <- green_lmd[[3]]

brown_lmd <- get_lmd(brown, km_brown)
brown_l <- brown_lmd[[1]]
brown_m <- brown_lmd[[2]]
brown_d <- brown_lmd[[3]]

red_lmd <- get_lmd(red, km_red)
red_l <- red_lmd[[1]]
red_m <- red_lmd[[2]]
red_d <- red_lmd[[3]]

yellow_green_lmd <- get_lmd(yellow_green, km_yellow_green)
yellow_green_l <- yellow_green_lmd[[1]]
yellow_green_m <- yellow_green_lmd[[2]]
yellow_green_d <- yellow_green_lmd[[3]]

yellow_brown_lmd <- get_lmd(yellow_brown, km_yellow_brown)
yellow_brown_l <- yellow_brown_lmd[[1]]
yellow_brown_m <- yellow_brown_lmd[[2]]
yellow_brown_d <- yellow_brown_lmd[[3]]


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


# remove mismatched colors ----

# detect outlier colors to remove from each palette using:
# dbscan and kmeans clustering
# or RGB filtering where effective
set.seed(42)

# white
# to appear "white"(including gray and  black), set cutoff
# for b channel (lower values are more blue, less yellow)

hist(white_l$b) # <= 20 
hist(white_m$b) # <= 20
hist(white_d$b) # <= 20

white_l_filtered <- white_l %>% filter(b < 20)
white_m_filtered <- white_m %>% filter(b < 20)
white_d_filtered <- white_d %>% filter(b < 20)


# # light brown
# brown_l_km <- kmeans(brown_l[,4:6], 6)
# brown_l2 <- cbind(brown_l, brown_l_km$cluster)
# brown_l2 <- brown_l2 %>% rename(km = 'brown_l_km$cluster')

# brown
# filter greens out of brown examples by setting minimum threshold
# for 'a' channel, where positive values are more red, negative more green
hist(brown_l$a) # quite different from distr. of m&d; most vals < 5
hist(brown_m$a)
hist(brown_d$a)

brown_l_filtered <- brown_l %>% filter(a>0 & a<10 & b<30) 
brown_m_filtered <- brown_m %>% filter(a>0 & a<10 & b<30)
brown_d_filtered <- brown_d %>% filter(a>0 & a<10 & b<30)

# brown_l3 <- brown_l %>% filter(Green < 170 & Red > 170)
# 
# # new feature for light brown: Red/Green
# brown_l_rg <- brown_l %>% mutate(RG = Red/Green)
# hist(brown_l_rg$RG)
# brown_l_filtered <- brown_l_rg %>% filter(RG>1.1)

# green
# filter browns out of green examples by setting maximum threshold
# for 'a' channel, where positive values are more red, negative more green

hist(green_l$a) 
hist(green_m$a)
hist(green_d$a)

green_l_filtered <- green_l %>% filter(a< -5 & b <30)
green_m_filtered <- green_m %>% filter(a< -5 & b <30)
green_d_filtered <- green_d %>% filter(a< -5 & b <30)

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

hist(yellow_green_l$b)
hist(yellow_green_m$b)
hist(yellow_green_d$b)
hist(yellow_green$b)

hist(yellow_green_l$a)
hist(yellow_green_m$a)
hist(yellow_green_d$a)
hist(yellow_green$a)

# minimum yellow threshold (b>=30) and not too red or too green (-15<a<-5)
yellow_green_l_filtered <- yellow_green_l %>% filter(b>30 & a < -2 & a > -15)
yellow_green_m_filtered <- yellow_green_m %>% filter(b>30 & a < -2 & a > -15)
yellow_green_d_filtered <- yellow_green_d %>% filter(b>30 & a < -2)

# yellow-brown

hist(yellow_brown$b)
hist(yellow_brown_l$a)
hist(yellow_brown_m$a)
hist(yellow_brown_d$a)
hist(yellow_brown$a)


# minimum yellow threshold (b>=30) and not too red or too green (-15<a<-5)
yellow_brown_l_filtered <- yellow_brown_l %>% filter(b>30 & a > -2 & a <10)
yellow_brown_m_filtered <- yellow_brown_m %>% filter(b>30 & a > -2) # all a<10
yellow_brown_d_filtered <- yellow_brown_d %>% filter(b>30 & a > -2) # all a<10

hist(yellow_brown_m_filtered$b)


# red
# maximum threshold for green (a>=0)

hist(red_l$a)
hist(red_m$a)
hist(red_d$a) 

red_l_filtered <- red_l %>% filter(a>10)
red_m_filtered <- red_m %>% filter(a>10)
red_d_filtered <- red_d %>% filter(a>10)


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

# tabulate (visual)
white_l_filtered_tab <- rgb_tab(white_l_filtered)
white_m_filtered_tab <- rgb_tab(white_m_filtered)
white_d_filtered_tab <- rgb_tab(white_d_filtered)

brown_l_filtered_tab <- rgb_tab(brown_l_filtered)
brown_m_filtered_tab <- rgb_tab(brown_m_filtered)
brown_d_filtered_tab <- rgb_tab(brown_d_filtered)

green_l_filtered_tab <- rgb_tab(green_l_filtered) # light green
green_m_filtered_tab <- rgb_tab(green_m_filtered) # medium green
green_d_filtered_tab <- rgb_tab(green_d_filtered) # dark green

yellow_green_l_filtered_tab <- rgb_tab(yellow_green_l_filtered)
yellow_green_m_filtered_tab <- rgb_tab(yellow_green_m_filtered)
yellow_green_d_filtered_tab <- rgb_tab(yellow_green_d_filtered) 

yellow_brown_l_filtered_tab <- rgb_tab(yellow_brown_l_filtered)
yellow_brown_m_filtered_tab <- rgb_tab(yellow_brown_m_filtered)
yellow_brown_d_filtered_tab <- rgb_tab(yellow_brown_d_filtered) 

red_l_filtered_tab <- rgb_tab(red_l_filtered)
red_m_filtered_tab <- rgb_tab(red_m_filtered)
red_d_filtered_tab <- rgb_tab(red_d_filtered) 

# save table
ggsave('plots/round_2/filtered/white_light_filtered.png', white_l_filtered_tab)
ggsave('plots/round_2/filtered/white_med_filtered.png', white_m_filtered_tab)
ggsave('plots/round_2/filtered/white_dark_filtered.png', white_d_filtered_tab)

# brown
ggsave('plots/round_2/filtered/brown_light_filtered.png', brown_l_filtered_tab)
ggsave('plots/round_2/filtered/brown_med_filtered.png', brown_m_filtered_tab)
ggsave('plots/round_2/filtered/brown_dark_filtered.png', brown_d_filtered_tab)

# green
ggsave('plots/round_2/filtered/green_light_filtered.png', green_l_filtered_tab) 
ggsave('plots/round_2/filtered/green_med_filtered.png', green_m_filtered_tab) 
ggsave('plots/round_2/filtered/green_dark_filtered.png', green_d_filtered_tab) 

# yellow-green
ggsave('plots/round_2/filtered/yellow-green_light_filtered.png', yellow_green_l_filtered_tab)
ggsave('plots/round_2/filtered/yellow-green_med_filtered.png', yellow_green_m_filtered_tab)
ggsave('plots/round_2/filtered/yellow-green_dark_filtered.png', yellow_green_d_filtered_tab)

# yellow-brown
ggsave('plots/round_2/filtered/yellow-brown_light_filtered.png', yellow_brown_l_filtered_tab)
ggsave('plots/round_2/filtered/yellow-brown_med_filtered.png', yellow_brown_m_filtered_tab)
ggsave('plots/round_2/filtered/yellow-brown_dark_filtered.png', yellow_brown_d_filtered_tab)

# red
ggsave('plots/round_2/filtered/red_light_filtered.png', red_l_filtered_tab)
ggsave('plots/round_2/filtered/red_med_filtered.png', red_m_filtered_tab)
ggsave('plots/round_2/filtered/red_dark_filtered.png', red_d_filtered_tab)


# bind light, med, dark and select necessary columns

color_bins <- function(color_name, light, med ,dark){
  l <- light %>% mutate(color_bin = paste('light', color_name))
  m <- med %>% mutate(color_bin = paste('medium', color_name))
  d <- dark %>% mutate(color_bin = paste('dark', color_name))
  df <- rbind(l,m,d)
  df <- df %>% select(id, Species, Red, Green, Blue,
                      L, a, b, Colour, color_bin)
  return(df)
}

# filtered 
white2 <- color_bins('white', 
                     white_l_filtered, white_m_filtered, white_d_filtered)
brown2 <- color_bins('brown', 
                     brown_l_filtered, brown_m_filtered, brown_d_filtered)
green2 <- color_bins('green', 
                     green_l_filtered, green_m_filtered, green_d_filtered)
red2 <- color_bins('red', 
                     red_l_filtered, red_m_filtered, red_d_filtered)
yellow_green2 <- color_bins('yellow_green', 
                     yellow_green_l_filtered, 
                     yellow_green_m_filtered, 
                     yellow_green_d_filtered)
yellow_brown2 <- color_bins('yellow_brown', 
                     yellow_brown_l_filtered, 
                     yellow_brown_m_filtered, 
                     yellow_brown_d_filtered)

# raw
white_raw <- color_bins('white', 
                     white_l, white_m, white_d)
brown_raw <- color_bins('brown', 
                     brown_l, brown_m, brown_d)
green_raw <- color_bins('green', 
                     green_l, green_m, green_d)
red_raw <- color_bins('red', 
                   red_l, red_m, red_d)
yellow_green_raw <- color_bins('yellow_green', 
                            yellow_green_l, 
                            yellow_green_m, 
                            yellow_green_d)
yellow_brown_raw <- color_bins('yellow_brown', 
                            yellow_brown_l, 
                            yellow_brown_m, 
                            yellow_brown_d)




# color space visualization ----

# prep data
colors_filtered <- rbind(white2, brown2, green2, red2, yellow_green2, yellow_brown2)
colors_filtered <- colors_filtered %>%
  mutate(color = word(color_bin, 2), L_bin = word(color_bin, 1))

colors_raw <- rbind(white_raw, brown_raw, green_raw, red_raw,
                    yellow_green_raw, yellow_brown_raw)
colors_raw <- colors_raw %>%
  mutate(color = word(color_bin, 2), L_bin = word(color_bin, 1))

# get sample colors for color scheme of visualization
sample_color <- function(x) {
  sample_rgb <- x[round(nrow(x)/2,0),]
  sample <- rgb(sample_rgb$Red/255, sample_rgb$Green/255, sample_rgb$Blue/255)
  return(sample)
}

sample_green <- sample_color(green2)
sample_brown <- sample_color(brown2)
sample_white <- sample_color(white2)
sample_red <- sample_color(red2)
sample_yellow_green <- sample_color(yellow_green2)
sample_yellow_brown <- sample_color(yellow_brown2)

sample_green_raw <- sample_color(green_raw)
sample_brown_raw <- sample_color(brown_raw)
sample_white_raw <- sample_color(white_raw)
sample_red_raw <- sample_color(red_raw)
sample_yellow_green_raw <- sample_color(yellow_green_raw)
sample_yellow_brown_raw <- sample_color(yellow_brown_raw)

# change name of sample color to "sample" to use as reference point
mark_samples <- function(x, ...) {
  samples = list(...)
  marked <- mutate(x, color = if_else(rgb(x$Red/255, x$Green/255, x$Blue/255) %in% samples, "sample", x$color))
  return(marked)
}


colors_filtered_marked <- mark_samples(colors_filtered, sample_green, sample_brown, sample_red, 
             sample_white, sample_yellow_green, sample_yellow_brown)

colors_raw_marked <- mark_samples(colors_raw, sample_green_raw, sample_brown_raw, sample_red_raw,
                                  sample_white_raw, sample_yellow_green_raw, sample_yellow_brown_raw)


# plot 
x_min <- -25
x_max <- 30
y_min <- -5
y_max <- 60

color_space_filtered <- ggplot(colors_filtered_marked, aes(x=a, y=b, color=color)) +
  geom_point(alpha=3/4) +
  # facet_grid(rows=vars(color), cols=vars(L_bin)) +
  scale_color_manual(values=c(sample_brown, sample_green, sample_red, "red", sample_white, 
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
        axis.text = element_text(size = 12))
  
  
color_space_raw <- ggplot(colors_raw_marked, aes(x=a, y=b, color=color)) +
  geom_point(alpha=3/4) + 
  # facet_grid(rows=vars(color), cols=vars(L_bin)) +
  scale_color_manual(values=c(sample_brown_raw, sample_green_raw, sample_red_raw,
                              "red",
                              sample_white_raw, sample_yellow_brown_raw,
                              sample_yellow_green_raw)) +
  ylab('Blue-Yellow') +
  xlab('Green-Red') +
  ylim(c(y_min, y_max)) +
  xlim(c(x_min, x_max)) +
  theme(legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))

# export color spaces side by side 

color_spaces <- ggarrange(color_space_raw, color_space_filtered, labels=c("Raw", "Filtered"))

ggsave('./plots/color_spaces.png', color_spaces, width = 20, height = 8)


# export to excel ----

# finish prepping data to export
# get colors removed when making colors_filtered
green_removed <- anti_join(green_raw, green2)
brown_removed <- anti_join(brown_raw, brown2)
red_removed <- anti_join(red_raw, red2)
white_removed <- anti_join(white_raw, white2)
yellow_green_removed <- anti_join(yellow_green_raw, yellow_green2)
yellow_brown_removed <- anti_join(yellow_brown_raw, yellow_brown2)


# make a tab for the gigantic colors df to keep provenance for all colors using unique id
write.xlsx(colors, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Master", 
           append=TRUE)
# tabs for green, brown, red, white
write.xlsx(green2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Green", 
           append=TRUE)
write.xlsx(brown2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Brown", 
           append=TRUE)
write.xlsx(red2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Red", 
           append=TRUE)
write.xlsx(white2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="White", 
           append=TRUE)
write.xlsx(yellow_green2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Green", 
           append=TRUE)
write.xlsx(yellow_brown2, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Brown", 
           append=TRUE)

# tabs for each color: examples removed when filtering using L*a*b* space thresholds
write.xlsx(green_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Green removed (Lab)", 
           append=TRUE)
write.xlsx(brown_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Brown removed (Lab)", 
           append=TRUE)
write.xlsx(red_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Red removed (Lab)", 
           append=TRUE)
write.xlsx(white_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="White removed (Lab)", 
           append=TRUE)
write.xlsx(yellow_green_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Green removed (Lab)", 
           append=TRUE)
write.xlsx(yellow_brown_removed, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Brown removed (Lab)", 
           append=TRUE)

# tabs for each color: examples discarded when making "pure" colors
write.xlsx(green_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Green Discards", 
           append=TRUE)
write.xlsx(brown_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Brown Discards", 
           append=TRUE)
write.xlsx(red_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Red Discards", 
           append=TRUE)
write.xlsx(white_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="White Discards", 
           append=TRUE)
write.xlsx(yellow_green_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Green Discards", 
           append=TRUE)
write.xlsx(yellow_brown_discards, file="data/colour_between_species_2021Mar17.xlsx", sheetName="Yellow-Brown Discards", 
           append=TRUE)







