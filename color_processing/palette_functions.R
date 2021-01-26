# Functions for making color palettes
# Noah Giebink

# libraries
library(tidyverse)
library(readxl)
library(stringr)
library(fuzzyjoin)
library(xlsx)

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




