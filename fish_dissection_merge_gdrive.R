################################################################################
################################################################################
################  FORAGE FISH DISSECTION DATA    ######################
################################################################################
## by Alexandra Cabanelas
## 17-JUL-2025

# merging forage fish dissection data from google drive
# https://drive.google.com/drive/u/0/folders/1kANX7zQLnOsyFbZ-qAYhfIxJXe9zoQMH
# the published data doesnt contain fish weights etc. 

## ------------------------------------------ ##
#            Packages -----
## ------------------------------------------ ##
library(tidyverse)
library(lubridate)

## ------------------------------------------ ##
#            Data -----
## ------------------------------------------ ##
csv_files <- list.files("raw/raw_googledrive", 
                        pattern = "\\.csv$", full.names = TRUE)

# bind all csvs
all_data <- csv_files %>%
  lapply(function(file) {
    read_csv(file, col_types = cols(
      `Date of collection` = col_character(),
      `Indiv. #` = col_character()
    )) %>%
      mutate(
        indiv_has_asterisk = grepl("\\*", `Indiv. #`),
        `Indiv. #` = as.numeric(gsub("\\*", "", `Indiv. #`)),
        `Date of collection` = as.Date(`Date of collection`, format = "%y%m%d")
      )
  }) %>%
  bind_rows()

all_data <- all_data %>%
  mutate(Date_collected = as.Date("Date of collection", 
                                      format = "%y%m%d"))


