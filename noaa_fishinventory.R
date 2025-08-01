################################################################################
#############          FORAGE FISH INVENTORY       #############################
#############             JULY-2025                #############################

#### ------------------------------------------ #####
#            Packages -----
#### ------------------------------------------ #####
library(tidyverse)
library(purrr)
library(readr)

#### ------------------------------------------ #####
#            Data -----
#### ------------------------------------------ #####
noaafish <- list.files("raw/noaa_fishinventory", 
                       pattern = "\\.csv$", 
                       full.names = TRUE) %>%
  map_df(read_csv)
# inventory by NOAA (sent by Mike)

head(noaafish)

#write.csv(noaafish, "output/noaafishinventory.csv")
