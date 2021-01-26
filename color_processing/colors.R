# color data processing
# 2020-12-21
# Noah Giebink

# libraries
library(tidyverse)
library(readxl)
library(stringr)
library(fuzzyjoin)
library(xlsx)

# color data

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

measurements <- read_excel_allsheets('data/colour_between_species_2020Dec04.xlsm')
labs <- read_excel_allsheets('data/colour_between_species_labs.xlsm')


# exclude unnecessary columns
measurements <- lapply(measurements, function(x) select(x, Species, Red, Green, Blue))
labs <- lapply(labs, function(x) select(x, Species, Colour...2))

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

# export 
write.xlsx(measurements$Leaf, file="data/colour_between_species_2020Dec21.xlsx", sheetName="Leaf", 
           append=TRUE)
write.xlsx(measurements$`Male Scale`, file="data/colour_between_species_2020Dec21.xlsx", sheetName="Male Scale", 
           append=TRUE)
write.xlsx(measurements$Perigynium, file="data/colour_between_species_2020Dec21.xlsx", sheetName="Perigynium", 
           append=TRUE)
write.xlsx(measurements$`Female Scale`, file="data/colour_between_species_2020Dec21.xlsx", sheetName="Female Scale", 
           append=TRUE)
write.xlsx(measurements$Cataphyll, file="data/colour_between_species_2020Dec21.xlsx", sheetName="Cataphyll", 
           append=TRUE)