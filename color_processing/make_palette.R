# Make color palettes
# Noah Giebink

# Source functions ----
source('palette_functions.R')

# Data prep: call functions ----

# brown, green and purple:

# read in data

colors_list <- read_excel_allsheets('data/colour_between_species_2020Dec21(4).xlsm')
colors_list <- colors_list[c('Leaf', 'Male Scale', 'Perigynium', 'Female Scale', 'Cataphyll')]

# remove scale measurements from colors_list
colors_list <- colors_list[-c(2,4)]

# get unique ids
colors <- unique_id(colors_list)

# get pure color data frames
green_synonyms <- c('green', 'olive')
brown_synonyms <- c('brown', 'tan', 'copper', 'straw', 'chestnut', 'gold', 'bronze', 'tawny')
purple_synonyms <- c('purple', 'maroon')

# get rough color data frames
rough_out <- rough_colors(colors, green_synonyms, brown_synonyms, purple_synonyms)

# create new combined color data frame for purple brown
purple_brown <- inner_join(rough_out[[2]], rough_out[[3]]) %>%
  arrange(desc(luminance)) %>%
  mutate(color_bin = 'purple-brown')

# get pure color data frames
pure_colors <- pure_colors(rough_out)
green <- pure_colors[[1]]
brown <- pure_colors[[2]]
purple <- pure_colors[[3]]

# Cluster each pure color into light, medium and dark ----
#' Use kmeans clustering with k=3 to label light, medium, and dark 
#' for each pure color data frame (e.g. 'light brown', 'brown', or 'dark brown' value in new column)
set.seed(42)

km_green <- kmeans(green$luminance, 3) # 1 = light, 2 = med, 3 = dark
km_brown <- kmeans(brown$luminance, 3) # 1 = light, 2 = dark, 3 = med
km_purple <- kmeans(purple$luminance, 3) # 1 = dark, 2 = med, 3 = light

# bind cluster value to respective color df
green$cluster <- km_green$cluster
brown$cluster <- km_brown$cluster
purple$cluster <- km_purple$cluster

# rename cluster numbers e.g. 'light brown'
green <- green %>% mutate(color_bin = if_else(cluster == 1, 'light green',
                                              if_else(cluster == 2, 'green', 'dark green'))) %>%
  arrange(desc(luminance))
brown <- brown %>% mutate(color_bin = if_else(cluster == 1, 'light brown',
                                              if_else(cluster == 2, 'brown', 'dark brown'))) %>%
  arrange(desc(luminance))
purple <- purple %>% mutate(color_bin = if_else(cluster == 3, 'light purple',
                                                if_else(cluster == 2, 'purple', 'dark purple'))) %>%
  arrange(desc(luminance))


# make sure color bins are sorted out correctly
ggplot(green, aes(color_bin, luminance)) +
  geom_boxplot()
ggplot(brown, aes(color_bin, luminance)) +
  geom_boxplot()
ggplot(purple, aes(color_bin, luminance)) +
  geom_boxplot()


# Export back to Excel ----

# clean up green, brown, purple to necessary columns
green_clean <- green %>% select(Red, Green, Blue, color_bin, id)
brown_clean <- brown %>% select(Red, Green, Blue, color_bin, id)
purple_clean <- purple %>% select(Red, Green, Blue, color_bin, id)
purple_brown_clean <- purple_brown %>% select(Red, Green, Blue, color_bin, id)


# export

# make a tab for the gigantic colors df to keep provenance for all colors using unique id
write.xlsx(colors, file="data/colour_between_species_2020Dec21(4).xlsm", sheetName="Master", 
           append=TRUE)

# tabs for green, brown, purple, green_brown, purple_brown
write.xlsx(green_clean, file="data/colour_between_species_2020Dec21(4).xlsm", sheetName="Green", 
           append=TRUE)
write.xlsx(brown_clean, file="data/colour_between_species_2020Dec21(4).xlsm", sheetName="Brown", 
           append=TRUE)
write.xlsx(purple_clean, file="data/colour_between_species_2020Dec21(4).xlsm", sheetName="Purple", 
           append=TRUE)
write.xlsx(purple_brown_clean, file="data/colour_between_species_2020Dec21(4).xlsm", sheetName="Purple-Brown", 
           append=TRUE)
