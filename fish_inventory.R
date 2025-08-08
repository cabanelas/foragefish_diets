################################################################################
#############          FORAGE FISH INVENTORY       #############################
#############             JULY-2025                #############################

#### ------------------------------------------ #####
#            Packages -----
#### ------------------------------------------ #####
library(tidyverse)
library(sf)

#### ------------------------------------------ #####
#            Data -----
#### ------------------------------------------ #####
fish <- read.csv("raw/LTER_ForageFish_Inventory.csv")
# this is the fish inventory created by Lyndsey (googledrive)

head(fish)

fishbongodf <- read.csv(file.path("raw",
                                "trawlbongometa.csv"),
                      header = T) 
# created in the bottom_trawl_NEFSC project
# script trawl_with_bongo

# to add region info, this is from ecomon
ecomap <- st_read("raw/EcomonStrata_v4.shp")
ecomapb <- st_read("raw/EcomonStrata_v4b.shp")
#### ------------------------------------------ #####
#            Tidy -----
#### ------------------------------------------ #####
ecomap_all <- rbind(ecomap, ecomapb)

st_crs(ecomap_all) <- 4326
ecomap_all <- st_make_valid(ecomap_all)


fish <- fish[, c(1:5, 8:9)]

fish1 <- fish %>%
  dplyr::filter(Cruise %in% c(202302, 202304, 202402, 202404) &
                  Species %in% c("P_tri", "S_sco", "C_har") &
                  Bongo == "Y")

fish1 %>%
  group_by(Cruise, Species) %>%
  summarise(total_fish = sum(Count), .groups = "drop")


fishbongo <- fish1 %>%
  group_by(Cruise, Species, Station) %>%
  summarise(total_fish = sum(Count), 
            bongo = all(Bongo == "Y"),
            .groups = "drop")

#write.csv(fishbongo, "output/fishbongo20232024.csv")

## add region
# merge the coordinate info from fishbongo to the fish df
fish_coords <- fishbongodf %>%
  filter(CRUISE %in% unique(fishbongo$Cruise),
         STATION %in% unique(fishbongo$Station)) %>%
  select(CRUISE, STATION, DECDEG_BEGLAT, DECDEG_BEGLON) %>%
  distinct()

fish_w_coords <- fishbongo %>%
  left_join(fish_coords, by = c("Cruise" = "CRUISE", "Station" = "STATION"))

fish_sf <- fish_w_coords %>%
  filter(!is.na(DECDEG_BEGLAT), !is.na(DECDEG_BEGLON)) %>%
  st_as_sf(coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"), crs = 4326)

fish_region <- st_join(fish_sf, ecomap_all, join = st_intersects, 
                       left = TRUE)

fish_region_clean <- fish_region %>%
  distinct(Cruise, Station, Species, .keep_all = TRUE) %>%
  st_drop_geometry() %>%
  select(Cruise, Station, Species, total_fish, bongo, Region)

#write.csv(fish_region_clean, "output/fishbongoregion20232024.csv")

#################

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


fishall <- fish %>%
  dplyr::filter(Cruise %in% c(202302, 202304, 202402, 202404) &
                  Bongo == "Y")
