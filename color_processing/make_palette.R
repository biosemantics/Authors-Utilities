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

# Source functions ----
source('palette_functions.R')


# explore color options with highest term frequency in set ----

terms <- colors$Colour %>% str_replace_all('-', ' ')
term_freq <- termFreq(terms)
tf <- as.data.frame(term_freq)
tf <- tf %>% arrange(desc(term_freq))

term_freq2 <- termFreq(colors$Colour)
tf2 <- as.data.frame(term_freq2)
tf2 <- tf2 %>% arrange(desc(term_freq2))



# Data prep: call functions ----

# brown, green and purple:

# read in data

colors_list <- read_excel_allsheets('data/colour_between_species_2020Dec21(4).xlsm')
colors_list <- colors_list[c('Leaf', 'Male Scale', 'Perigynium', 'Female Scale', 'Cataphyll')]

# remove scale measurements from colors_list
colors_list <- colors_list[-c(2,4)]

# get unique ids
colors <- unique_id(colors_list)

# color synonyms

# green brown purple only (first round):
# green_synonyms <- c('green', 'olive')
# brown_synonyms <- c('brown', 'tan', 'copper', 'straw', 'chestnut', 'gold', 'bronze', 'tawny')
# purple_synonyms <- c('purple', 'maroon')

# green_synonyms <- c('green', 'olive')
# brown_synonyms <- c('brown', 'castaneous', 'chestnut', 'copper', 'bronze', 'tan', 'tawny')
# red_synonyms <- c('red', 'maroon')
# yellow_synonyms <- c('yellow', 'golden', 'straw', 'gold')
# purple_synonyms <- c('purple')
# white_synonyms <- c('white', 'gray', 'glaucous')


green_synonyms <- c('green', 'olive')
brown_synonyms <- c('brown', 'castaneous', 'chestnut', 'copper', 'bronze', 'tan', 'tawny',
                    'yellow', 'golden', 'straw', 'gold') # includes yellow synonyms
red_synonyms <- c('red', 'maroon') # includes purple
white_synonyms <- c('white', 'gray', 'glaucous')

# get rough color data frames
rough_out <- rough_colors(colors, green_synonyms,
                          brown_synonyms, 
                          red_synonyms,
                          # yellow_synonyms,
                          # purple_synonyms,
                          white_synonyms)

# # create new combined color data frame for purple brown
# purple_brown <- inner_join(rough_out[[2]], rough_out[[3]]) %>%
#   arrange(desc(luminance)) %>%
#   mutate(color_bin = 'purple-brown')

# get pure color data frames
pure_colors <- pure_colors(rough_out)


green <- pure_colors[[1]]
brown <- pure_colors[[2]]
red <- pure_colors[[3]]
# yellow <- pure_colors[[4]]
# purple <- pure_colors[[5]]
white <- pure_colors[[6]]


# Cluster each pure color into light, medium and dark ----
#' Use kmeans clustering with k=3 to label light, medium, and dark 
#' for each pure color data frame (e.g. 'light brown', 'brown', or 'dark brown' value in new column)
set.seed(42)


km_white <- kmeans(white$luminance, 3) # 1 = light, 2 = med, 3 = dark
km_green <- kmeans(green$luminance, 3) # 1 = med, 2 = dark, 3 = light
km_brown <- kmeans(brown$luminance, 3) # 1 = med, 2 = dark, 3 = light
km_red <- kmeans(red$luminance, 3) # 1 = dark, 2 = light, 3 = med

    # yellow and purple have to few rows to warrant clustering

# bind cluster value to respective color df
white$cluster <- km_white$cluster 
green$cluster <- km_green$cluster
brown$cluster <- km_brown$cluster
red$cluster <- km_red$cluster

# white <- white %>% mutate(color_bin = if_else(cluster == 1, 'light gray',
#                                               if_else(cluster == 2, 'gray', 'dark gray'))) %>%


# km_purple <- kmeans(purple$luminance, 3) # 1 = dark, 2 = med, 3 = light



# purple$cluster <- km_purple$cluster

# # rename cluster numbers e.g. 'light brown'
# green <- green %>% mutate(color_bin = if_else(cluster == 1, 'light green',
#                                               if_else(cluster == 2, 'green', 'dark green'))) %>%
#   arrange(desc(luminance))
# brown <- brown %>% mutate(color_bin = if_else(cluster == 1, 'light brown',
#                                               if_else(cluster == 2, 'brown', 'dark brown'))) %>%
#   arrange(desc(luminance))
# purple <- purple %>% mutate(color_bin = if_else(cluster == 3, 'light purple',
#                                                 if_else(cluster == 2, 'purple', 'dark purple'))) %>%
#   arrange(desc(luminance))


# # make sure color bins are sorted out correctly
# ggplot(green, aes(color_bin, luminance)) +
#   geom_boxplot()
# ggplot(brown, aes(color_bin, luminance)) +
#   geom_boxplot()
# ggplot(purple, aes(color_bin, luminance)) +
#   geom_boxplot()

# Function: arrange_luminance() ----
#' arrange by descending luminance
#' param data: data frame with luminance values 

white <- white %>% arrange(desc(luminance))
green <- green %>% arrange(desc(luminance))
brown <- brown %>% arrange(desc(luminance))
red <- red %>% arrange(desc(luminance))
# yellow <- yellow %>% arrange(desc(luminance))
# purple <- purple %>% arrange(desc(luminance))


# Function: rgb_tab() ----
#' plot colors in table using rgb data


rgb_tab <- function(rgb_data){
  red <- rgb_data$Red/255
  green <- rgb_data$Green/255
  blue <- rgb_data$Blue/255
  rgb_data$color_fill <- rgb(red, green, blue)
  rgb <- rgb_data %>% select(color_fill)
  rgb_tab <- rgb_data %>% select(id, Species, Red, Green, Blue, Colour)
  tab <- ggtexttable(rgb_tab)
  for (i in 1:nrow(rgb_tab)) {
    row = i+1
    tab <- table_cell_bg(tab, row = row, 
                         column = 7,
                         fill = rgb$color_fill[i])
  }
  tab
}

# separate into light, med, dark
white_l <- white %>% filter(cluster == 1)
white_m <- white %>% filter(cluster == 2)
white_d <- white %>% filter(cluster == 3)

green_l <- green %>% filter(cluster == 3)
green_m <- green %>% filter(cluster == 1)
green_d <- green %>% filter(cluster == 2)

brown_l <- brown %>% filter(cluster == 3)
brown_m <- brown %>% filter(cluster == 1)
brown_d <- brown %>% filter(cluster == 2)

red_l <- red %>% filter(cluster == 2)
red_m <- red %>% filter(cluster == 3)
red_d <- red %>% filter(cluster == 1)


# get tables for light, med, dark
white_l_tab <- rgb_tab(white_l)
white_m_tab <- rgb_tab(white_m)
white_d_tab <- rgb_tab(white_d)

green_l_tab <- rgb_tab(green_l)
green_m_tab <- rgb_tab(green_m)
green_d_tab <- rgb_tab(green_d)

brown_l_tab <- rgb_tab(brown_l)
brown_m_tab <- rgb_tab(brown_m)
brown_d_tab <- rgb_tab(brown_d)

red_l_tab <- rgb_tab(red_l)
red_m_tab <- rgb_tab(red_m)
red_d_tab <- rgb_tab(red_d)

# purple_tab <- rgb_tab(purple)
# yellow_tab <- rgb_tab(yellow)

# save table plots
ggsave('plots/white_light.png',white_l_tab)
ggsave('plots/white_med.png', white_m_tab)
ggsave('plots/white_dark.png', white_d_tab)

ggsave('plots/green_light.png', green_l_tab)
ggsave('plots/green_med.png', green_m_tab)
ggsave('plots/green_dark.png', green_d_tab)

ggsave('plots/brown_light.png', brown_l_tab)
ggsave('plots/brown_med.png', brown_m_tab)
ggsave('plots/brown_dark.png', brown_d_tab)

ggsave('plots/red_light.png', red_l_tab)
ggsave('plots/red_med.png', red_m_tab)
ggsave('plots/red_dark.png', red_d_tab)

# ggsave('plots/purple.png', purple_tab)
# ggsave('plots/yellow.png', yellow_tab)


# # heatmap style visualization with geom_tile
# white$y_pos <- c(0:length(white))
# white$color_fill <- rgb(white$Red/255, white$Green/255, white$Blue/255)
# 
# ggplot(white, aes(cluster, y_pos, fill = rgb(Red/255, Green/255, Blue/255))) +
#   geom_tile()
# 
# ggplot(white) +
#   facet_wrap(~cluster) +
#   geom



# kmeans clustering for color bins ----
# silhouette method to choose k 

# 
# # plot color rgb space
scatter3D(colors$Red, colors$Green, colors$Blue)

# green
scatter3D(green_l$Red, green_l$Green, green_l$Blue)
scatter3D(green_m$Red, green_m$Green, green_m$Blue)
scatter3D(green_d$Red, green_d$Green, green_d$Blue)
# brown
scatter3D(brown_l$Red, brown_l$Green, brown_l$Blue)
scatter3D(brown_m$Red, brown_m$Green, brown_m$Blue)
scatter3D(brown_d$Red, brown_d$Green, brown_d$Blue)
# red
scatter3D(red_l$Red, red_l$Green, red_l$Blue)
scatter3D(red_m$Red, red_m$Green, red_m$Blue)
scatter3D(red_d$Red, red_d$Green, red_d$Blue)
# white
scatter3D(white_l$Red, white_l$Green, white_l$Blue)
scatter3D(white_m$Red, white_m$Green, white_m$Blue)
scatter3D(white_d$Red, white_d$Green, white_d$Blue)


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

min_max <- function(x){
  y <- (x - min(x))/(max(x) - min(x))
  y
}

colors_rg <- colors %>% mutate(r = Red/255, g = Green/255, b = Blue/255)
colors_rg <- colors_rg %>% mutate(r_to_g = r/g)
colors_rg <- colors_rg %>% mutate(r_norm = min_max(r), 
                                  g_norm = min_max(g),
                                  b_norm = min_max(b),
                                  r_to_g_norm = min_max(r_to_g))

silhouette_score <- function(k, df){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:6

# number of clusters for raw rgb in colors data set
avg_sil <- sapply(k, silhouette_score, colors[,4:6])
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

# number of clusters for normalized rgb 
avg_sil <- sapply(k, silhouette_score, select(colors_rg, r_norm:r_to_g_norm))
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


km_colors <- kmeans(colors_rg[,15:18], centers = 3, nstart = 25)

colors_2 <- cbind(colors, km_colors$cluster)
colors_2 <- colors_2 %>% rename(cluster = 'km_colors$cluster')

colors_a <- colors_2 %>% filter(cluster == 1) %>% arrange(desc(luminance))
colors_b <- colors_2 %>% filter(cluster == 2) %>% arrange(desc(luminance))
colors_c <- colors_2 %>% filter(cluster == 3) %>% arrange(desc(luminance))

rgb_a <- rgb_tab(colors_a)
rgb_b <- rgb_tab(colors_b)
rgb_c <- rgb_tab(colors_c)

ggsave('plots/rgb_a.png', rgb_a)
ggsave('plots/rgb_b.png', rgb_b)
ggsave('plots/rgb_c.png', rgb_c)



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


# detect outlier colors to remove from each palette using:
# dbscan and kmeans clustering
# or RGB filtering where effective
set.seed(42)

# white
# to appear "white"(including gray and  black), set cutoff
# for blue channel at 75% of the mean red and green channel value

white_l_filtered <- white_l %>% mutate(mean_rg = ((Red + Green) / 2))
white_m_filtered <- white_m %>% mutate(mean_rg = ((Red + Green) / 2))
white_d_filtered <- white_d %>% mutate(mean_rg = ((Red + Green) / 2))

white_l_filtered <- white_l_filtered %>% filter(Blue > 0.75*mean_rg)
white_m_filtered <- white_m_filtered %>% filter(Blue > 0.75*mean_rg)
white_d_filtered <- white_d_filtered %>% filter(Blue > 0.75*mean_rg)

# # light brown
# brown_l_km <- kmeans(brown_l[,4:6], 6)
# brown_l2 <- cbind(brown_l, brown_l_km$cluster)
# brown_l2 <- brown_l2 %>% rename(km = 'brown_l_km$cluster')

# light brown by filtering down green channel, up Red
hist(brown_l$Green)
hist(brown_l$Red)

brown_l3 <- brown_l %>% filter(Green < 170 & Red > 170)


# # light green
# green_l_db <- dbscan(green_l[,4:6], eps = 15, MinPts = 3)
# green_l2 <- cbind(green_l, green_l_db$cluster)
# green_l2 <- green_l2 %>% rename(db = 'green_l_db$cluster')

# medium green
green_m_km <- kmeans(green_m[,4:6], 3)
green_m2 <- cbind(green_m, green_m_km$cluster)
green_m2 <- green_m2 %>% rename(km = 'green_m_km$cluster')

# dark green
green_d_db <- dbscan(green_d[,4:6], eps = 15, MinPts = 3)
green_d2 <- cbind(green_d, green_d_db$cluster)
green_d2 <- green_d2 %>% rename(db = 'green_d_db$cluster')

# visualize clusters
# fviz_cluster(brown_l_km, brown_l[,4:6]) # light brown
# fviz_cluster(green_l_db, green_l[,4:6]) # light green
fviz_cluster(green_m_km, green_m[,4:6]) # medium green
fviz_cluster(green_d_db, green_d[,4:6]) # dark green

# filter clusters that best maintain intended color
# brown_l_filtered <- brown_l2 %>% filter(km == 5 | km ==  2) # light brown
# green_l_filtered <- green_l2 %>% filter(db == 1) # light green
green_m_filtered <- green_m2 %>% filter(km == 1) # medium green
green_d_filtered <- green_d2 %>% filter(db == 0) # dark green

# tabulate (visual)
white_l_filtered_tab <- rgb_tab(white_l_filtered)
white_m_filtered_tab <- rgb_tab(white_m_filtered)
white_d_filtered_tab <- rgb_tab(white_d_filtered)
# brown_l_filtered_tab <- rgb_tab(brown_l_filtered) # light brown
# brown_l_filtered_tab2 <- rgb_tab(brown_l3) # light brown
# green_l_filtered_tab <- rgb_tab(green_l_filtered) # light green
green_m_filtered_tab <- rgb_tab(green_m_filtered) # medium green
green_d_filtered_tab <- rgb_tab(green_d_filtered) # dark green

# save table
ggsave('plots/white_light2.png', white_l_filtered_tab)
ggsave('plots/white_med2.png', white_m_filtered_tab)
ggsave('plots/white_dark2.png', white_d_filtered_tab)
# ggsave('plots/brown_light2.png', brown_l_filtered_tab)
# ggsave('plots/brown_light3.png', brown_l_filtered_tab2)
# ggsave('plots/green_light2.png', green_l_filtered_tab) # medium green
ggsave('plots/green_med2.png', green_m_filtered_tab) # medium green
ggsave('plots/green_dark2.png', green_d_filtered_tab) # dark green


# Export back to Excel ----

# bind light, med, dark and select necessary columns

white_l2 <- white_l_filtered %>% mutate(color_bin = 'light white') # after filtering blue channel
white_m2 <- white_m_filtered %>% mutate(color_bin = 'medium white') # after filtering blue channel
white_d2 <- white_d_filtered %>% mutate(color_bin = 'dark white') # after filtering blue channel
white2 <- rbind(white_l2, white_m2, white_d2)
white2 <- white2 %>% select(id, Species, Red, Green, Blue, color_bin)

brown_l4 <- brown_l3 %>% mutate(color_bin = 'light brown') # after filtering down green channel, up Red
brown_m2 <- brown_m %>% mutate(color_bin = 'brown') # original
brown_d2 <- brown_d %>% mutate(color_bin = 'dark brown') # original
brown2 <- rbind(brown_l4, brown_m2, brown_d2)
brown2 <- brown2 %>% select(id, Species, Red, Green, Blue, color_bin)

green_l2 <- green_l %>% mutate(color_bin = 'light green') # original
green_m2 <- green_m_filtered %>% mutate(color_bin = 'green') # after clustering
green_d2 <- green_d_filtered %>% mutate(color_bin = 'dark green') # after clustering
green2 <- rbind(green_l2, green_m2[-12], green_d2[-12])
green2 <- green2 %>% select(id, Species, Red, Green, Blue, color_bin)

red_l2 <- red_l %>% mutate(color_bin = 'light red') # original
red_m2 <- red_m %>% mutate(color_bin = 'red') # original
red_d2 <- red_d %>% mutate(color_bin = 'dark red') # original
red2 <- rbind(red_l2, red_m2, red_d2)
red2 <- red2 %>% select(id, Species, Red, Green, Blue, color_bin) 

# # clean up green, brown, purple to necessary columns
# green_clean <- green %>% select(Red, Green, Blue, color_bin, id)
# brown_clean <- brown %>% select(Red, Green, Blue, color_bin, id)
# purple_clean <- purple %>% select(Red, Green, Blue, color_bin, id)
# purple_brown_clean <- purple_brown %>% select(Red, Green, Blue, color_bin, id)


# export
# make a tab for the gigantic colors df to keep provenance for all colors using unique id
write.xlsx(colors, file="data/colour_between_species_2021Feb08.xlsm", sheetName="Master", 
           append=TRUE)
# tabs for green, brown, red, white
write.xlsx(green2, file="data/colour_between_species_2021Feb08.xlsm", sheetName="Green", 
           append=TRUE)
write.xlsx(brown2, file="data/colour_between_species_2021Feb08.xlsm", sheetName="Brown", 
           append=TRUE)
write.xlsx(red2, file="data/colour_between_species_2021Feb08.xlsm", sheetName="Red", 
           append=TRUE)
write.xlsx(white2, file="data/colour_between_species_2021Feb08.xlsm", sheetName="White", 
           append=TRUE)








