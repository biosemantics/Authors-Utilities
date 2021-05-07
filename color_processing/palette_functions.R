# Functions for making color palettes
# Noah Giebink

# libraries
library(tidyverse)
library(ggpubr)
library(readxl)
library(stringr)
library(fuzzyjoin)
library(xlsx)
library(RANN)

# Function read_excel_allsheets() ----
#' reads all sheets from an excel file into a list of data frames
#' https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Function color_strings() ----
#' param measurements: list of data frames with RGB measurements, missing color descriptions
#' param labels: list of data frames with color descriptions given to different species
#' returns measurements: input measurements updated with matching color descriptions

color_strings <- function(measurements, labels){
  
  # exclude unnecessary columns
  measurements <- lapply(measurements, function(x) select(x, Species, Red, Green, Blue))
  labs <- lapply(labels, function(x) select(x, Species, Colour...2))
  
  # align labels in labs to measurements
  # get just epithet from each species name for partial match with measurements$Species
  labs <- lapply(labs,
                 function(x) mutate(x, 
                                    'epithet' = str_remove(x$Species, 'Carex ')))
  
  # change order of labs list to match measurements
  labs <- labs[c('Leaf', 'Male Scale', 'Perigynium', 'Female Scale', 'Cataphyll')]
  
  # inner_join by partial string match, iterated over each df 
  for (df in 1:length(measurements)) {
    measurements[[df]] <- regex_inner_join(measurements[[df]], labs[[df]],
                                           by = c(Species = 'epithet'))
  }
  
  # clean up
  measurements <- lapply(measurements, function(x) select(x, Species = Species.x, 
                                                          Species.y, 
                                                          Red, Green, Blue, 
                                                          Colour))
  
  # sort colors by luminance, decreasing
  measurements <- lapply(measurements, 
                         function(x) mutate(x, 
                                            luminance = 0.299*Red+0.587*Green+0.114*Blue))
  measurements <- lapply(measurements, function(x) arrange(x, desc(luminance)))
  
  # remove faulty species matches (e.g. C. umbellata & C. bella falsely match)
  matches <- function(x){
    return(filter(x, x$Species == x$Species.y))
  }
  measurements <- lapply(measurements, function(x) matches(x))
  
  return(measurements)
}


# Function: unique_id() ----
#' param colors_list: list of excel sheets with rgb color measurements
#' gives each row in each sheet a unique id in a new column, 'id'
#' returns single data frame (binds rows of each sheet, differentiable by 'id')

unique_id <- function(colors_list){
  for (df in 1:length(colors_list)) {
    id = str_c(names(colors_list[df]), 1:nrow(colors_list[[df]]), sep = "_")
    colors_list[[df]] = cbind(colors_list[[df]], id)
  }
  colors <- bind_rows(colors_list)
  return(colors)
}

# Function rough_colors() ----
#' param x: data frame containing all color descriptions (e.g. 'colors')
#' param ...: s lists of synonyms for each pure color data frame to be returned
#' returns rough_out: list of data frames (equal to s lists of synonyms), 
#' each containing 'Colour' strings matching a set of synonyms
#' Intermediate step to creating pure color data frames to enable color combo creation

rough_colors <- function(x, ...){
  syn_all <- list(...)
  rough_out <- list()
  for (s in 1:length(syn_all)) {
    rough <- x %>%
      filter(str_detect(x$Colour, paste(syn_all[[s]], collapse = '|')))
    rough_out[[s]] <- rough
  }
  return(rough_out)
}

# Function: pure_colors() ----
#' param rough_out: ouput of rough_colors()
#' Returns pure_out: list of data frames containing ONLY
#' 'Coulour' strings matching their respective set of synonyms
#' with colour strings matching other synonyms (specified in rough_colors()) removed.
#' e.g. create 3 pure color data frames matching green, brown, and purple synonyms 


pure_colors <- function(rough_out){
  pure_out <- list()
  for (r in 1:length(rough_out)) {
    pure_list <- list()
    for (n in 1:length(rough_out)) {
      if (n != r) {
        pure <- rough_out[[r]] %>%
          filter(!(rough_out[[r]]$Colour %in% rough_out[[n]]$Colour))
        pure_list[[n]] <- pure
      }
    }
    # inner join pure list items (not in n, also not in n+1)
    # to get items unique to n = r
    # filter out empty items in pure_list (where n = r)
    pure_out[[r]] <- Reduce(function(...) inner_join(...),
                            Filter(Negate(is.null), pure_list))
  }
  return(pure_out)
}

# Function: get_lmd() ----
#' correctly match each k-means center to light, medium, and dark
#' and return list of data frames: light, medium, dark.
#' Use this function after doing k-means clustering on lightness values,
#' or L from L*a*b* space.
#' param color_df: color data frame (e.g. green)
#' param km_out: k-means output for color_df
get_lmd <- function(color_df, km_out){
  lmd_list <- list()
  lmd_list[[1]] <- color_df %>%
    filter(cluster == which(km_out$centers == max(km_out$centers)))
  lmd_list[[2]] <- color_df %>%
    filter(cluster != which(km_out$centers == min(km_out$centers)) 
           & cluster != which(km_out$centers == max(km_out$centers)))
  lmd_list[[3]] <- color_df %>%
    filter(cluster == which(km_out$centers == min(km_out$centers)))
  return(lmd_list)
}

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

# Function: nn_downsample() ----
nn_downsample <- function(data, k=3, scale=1/5){

  # get distance to k nearest neighbors (excluding nearest: self)
  nearest <- nn2(data,data, k = k+1)
  nn_dist <- as.data.frame(nearest$nn.dists[,2:(k+1)]) 
  mean_dist <- nn_dist %>% rowwise() %>% transmute(mean_dist = mean(c(V1, V2, V3)))

  # bind dist to data
  dist_data <- cbind(data, mean_dist)
  # normalize mean_dist
  min_max = function(x){
    y = (x - min(x))/(max(x) - min(x))
    return(y)
  }
  dist_data <- dist_data %>% mutate(nn_dist_norm = min_max(mean_dist))

  # quantile sampling approach (nothing too close or too far: more evenly spaced?)
  q25 <- quantile(dist_data$nn_dist_norm)[[2]]
  q75 <- quantile(dist_data$nn_dist_norm)[[4]]
  dist_sample <- dist_data %>% filter(nn_dist_norm > q25 & nn_dist_norm < q75)
  dist_sample <- dist_sample[sample(nrow(dist_sample), size = 20),]

return(dist_sample)
}

# # simulate points data
# x1 <- runif(100, 0, 2*pi)
# x2 <- runif(100, 0,3)
# data <- data.frame(x1, x2)
# 
# 
# dist_sample <- nn_downsample(data)
# 
# # # sample points proportionally to nearest neighbor
# # dist_sample <- dist_data[sample(nrow(dist_data), size = 20, prob = dist_data$nn_dist_norm),]
# # compare to random sample
# rand_sample <- data[sample(nrow(data), size = 1/5 * nrow(data)),]
# 
# # plot original points
# plot(data$x1, data$x2)
# 
# # plot down sampled points (proportional to distance)
# plot(dist_sample$x1, dist_sample$x2)
# 
# # plot down sampled points (random)
# plot(rand_sample$x1, rand_sample$x2)

# Function: centroid_color() ----

centroid_color <- function(data){
  # centroid = mean a, b in Lab space
  x <- mean(data$a)
  y <- mean(data$b)
  centroid <- data.frame(x, y)
  
  # actual point = nearest neighbor of centroid
  points <- select(data, a, b)
  nearest <- nn2(points, centroid, k=2)
  nn_idx <- nearest$nn.idx[,2]
  
  # get color sample from index
  sample_rgb <- data[nn_idx,]
  sample <- rgb(sample_rgb$Red/255, sample_rgb$Green/255, sample_rgb$Blue/255)
  return(sample)
}

# Function mark_samples() ----
mark_samples <- function(x, ...) {
  samples = list(...)
  marked <- mutate(x, color = if_else(rgb(x$Red/255, x$Green/255, x$Blue/255) %in% samples, "sample", x$color))
  return(marked)
}


# Function: species_name ----
#' keep just first two words of Species (genus, epithet)
species_name <- function(x){
  x <- mutate(x, Species = word(x$Species, 1, 2, sep = "_"))
  x <- mutate(x, Species = str_replace(x$Species, "_", " "))
  return(x)
}


# Function: thresholds_new ----
#' Separates colors in lab space into separate bins

thresholds <- function(data){
  out <- data %>% mutate(color = if_else(a> -5 & a < 0 & b < 20, 'white', 
                                         if_else(a< -2 & b < 32, 'green',
                                                 if_else(a < -2 & b > 32 & a > -12, 'yellow-green', 
                                                         if_else(a < -12 & b > 32, 'green',
                                                                 if_else(a > -2 & a < 10 & b < 32, 'brown',
                                                                         if_else(a > -2 & a < 10 & b > 32, 'yellow-brown',
                                                                                 if_else(a>10 & b < 25, 'red',
                                                                                         if_else(a > 10 & b > 25, 'brown',
                                                                                                 'discard')))))))))
  return(out)
}

# Function: update_measurements ----
#' replace rows in original measurements with matching resampled rows in resample

update_measurements <- function(original, new_samples){
  keep <- filter(original, !(original$Species %in% new_samples$Species))
  updated <- bind_rows(keep, new_samples)
  return(updated)
}


# Function: vis_breaks ----
#' visualize natural breaks in the distribution of color samples over a specified
#' CIELab color space axis
#' NOT WORKING - MAKES PLOT BUT ONLY IN GRAYSCALE

bin_values <- function(data, ...){
  bins <- list(...)
  vals <- list()
  for (b in bins) {
    val <- centroid_color(data[data$bin == b,])
    vals[[b]] <- val
  }
  return(vals)
}

vis_breaks <- function(data, dimension, bin_width, ...){
  color_bins <- list(...)
  out <- ggplot(data, aes(x=dimension))+
    geom_histogram(bin_width=bin_width)+
    scale_fill_manual(values = unlist(color_bins))+
    theme_pubr()
  return(out)
}
