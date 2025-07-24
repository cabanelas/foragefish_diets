################################################################################
#############          FORAGE FISH INVENTORY       #############################
#############             JULY-2025                #############################

#### ------------------------------------------ #####
#            Packages -----
#### ------------------------------------------ #####
library(tidyverse)

#### ------------------------------------------ #####
#            Data -----
#### ------------------------------------------ #####
fish <- read.csv("LTER_ForageFish_Inventory.csv")

head(fish)

#### ------------------------------------------ #####
#            Tidy -----
#### ------------------------------------------ #####

fish <- fish[, c(1:5, 8:9)]

fish1 <- fish %>%
  dplyr::filter(Cruise %in% c(202302, 202304, 202402, 202404) &
                  Species %in% c("P_tri", "S_sco", "C_har") &
                  Bongo == "Y")

fish1 %>%
  group_by(Cruise, Species) %>%
  summarise(total_fish = sum(Count), .groups = "drop")


fish2 <- fish %>%
  dplyr::filter(Cruise %in% c(202302, 202304, 202402, 202404) &
                  Species %in% c("P_tri", "S_sco", "C_har") &
                  Bongo == "N")
fish2 %>%
  group_by(Cruise, Species) %>%
  summarise(total_fish = sum(Count), .groups = "drop")



fish3 <- fish %>%
  dplyr::filter(Cruise %in% c(202302, 202304, 202402, 202404) &
                  Species %in% c("P_tri", "S_sco", "C_har"))
fish3 %>%
  group_by(Cruise, Species) %>%
  summarise(total_fish = sum(Count), .groups = "drop")
