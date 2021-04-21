# color data processing
# 2020-12-21
# Noah Giebink

# libraries
library(tidyverse)
library(readxl)
library(stringr)
library(fuzzyjoin)
library(xlsx)

# source functions from palette functions
source('palette_functions.R')

# prep data to run in color_strings()

# for param: measurements
measurements <- read_excel_allsheets('data/colour_between_species_2020Dec04.xlsm')

# keep just first two words of Species (genus, epithet)
measurements <- lapply(measurements, function(x) species_name(x))

# new RGB samples for some species -- update measurements with these
measurements_resampled <- read_excel_allsheets('data/resampled_data_2021-02-13.xlsm')
measurements_resampled <- lapply(measurements_resampled, function(x) select(x, Species, 
                                                  Red = R,
                                                  Green = G,
                                                  Blue = B))

# list of label data frames (param: labels)
labs <- read_excel_allsheets('data/colour_between_species_labs.xlsm')

# run 
resample <- color_strings(measurements_resampled, labs)
original_measurements <- color_strings(measurements, labs)

# replace rows in original_measurements with matching resampled rows in resample
filter(original_measurements$Leaf, 
       !(original_measurements$Leaf$Species %in% resample$Leaf_2$Species))


update_measurements <- function(original, new_samples){
  keep <- filter(original, !(original$Species %in% new_samples$Species))
  updated <- bind_rows(keep, new_samples)
  return(updated)
}

# update original meaurements with resampled species
updated <- list()
updated$Leaf <- 
  update_measurements(original_measurements$Leaf, resample$Leaf_2)
updated$'Male Scale' <- 
  update_measurements(original_measurements$`Male Scale`, resample$`Male Scale_2`)
updated$Perigynium <- 
  update_measurements(original_measurements$Perigynium, resample$Perigynium_2)
updated$'Female Scale' <- 
  update_measurements(original_measurements$`Female Scale`, resample$`Female Scale_2`)
updated$Cataphyll <- 
  update_measurements(original_measurements$Cataphyll, resample$Cataphyll_2)



# export
write.xlsx(updated$Leaf, file="data/colour_between_species_2021Mar7.xlsx", sheetName="Leaf",
           append=TRUE)
write.xlsx(updated$`Male Scale`, file="data/colour_between_species_2021Mar7.xlsx", sheetName="Male Scale",
           append=TRUE)
write.xlsx(updated$Perigynium, file="data/colour_between_species_2021Mar7.xlsx", sheetName="Perigynium",
           append=TRUE)
write.xlsx(updated$`Female Scale`, file="data/colour_between_species_2021Mar7.xlsx", sheetName="Female Scale",
           append=TRUE)
write.xlsx(updated$Cataphyll, file="data/colour_between_species_2021Mar7.xlsx", sheetName="Cataphyll",
           append=TRUE)