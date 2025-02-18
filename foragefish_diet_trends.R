################################################################################
#############          FORAGE FISH DIETS           #############################
#############             FEB-2025                 #############################
## by: Alexandra Cabanelas
# looking at fish diet trends of fish already processed by J Suca et al
# data is from https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nes.2.3 

#Glancy, S.G., J.J. Suca, and J.K. Llopiz. 2022. Diet composition for small 
##pelagic fishes across the Northeast U.S. Continental Shelf for NES-LTER, 
##ongoing since 2013 ver 3. Environmental Data Initiative. 
##https://doi.org/10.6073/pasta/e48fef01a8bb7ae3c443d57de83bed2e 

#Concat_Count_EDI = prey counts
#Concat_Meas_EDI = prey measurements 

#### ------------------------------------------ #####
#            Packages -----
#### ------------------------------------------ #####

library(tidyverse)
library(RColorBrewer)
#library(gridExtra) 
#library(cowplot)

#### ------------------------------------------ #####
#            Data -----
#### ------------------------------------------ #####

diets <- read.csv(file.path("raw","Concat_Count_EDI.csv"))


#### ------------------------------------------ #####
#            Tidy -----
#### ------------------------------------------ #####

# create season and year columns 
diets <- diets %>%
  mutate(
    year = substr(as.character(cruise), 1, 4),  # Extract first 4 digits as Year
    season = case_when(
      grepl("02$", cruise) ~ "Spring",  # If cruise ends in '02', assign "Spring"
      grepl("04$", cruise) ~ "Fall",    # If cruise ends in '04', assign "Fall"
      TRUE ~ NA_character_  # Assign NA if it doesn't match
    )
  )

# preyTaxon = Most general taxonomic category of prey
# preyTaxa = Specific taxonomic category of prey
# Llopiz_taxa = Llopiz lab category of prey item
# scientificName_preyTaxon = Scientific name of prey item

comparison_table <- diets %>%
  select(preyTaxon, preyTaxa, Llopiz_taxa, 
         scientificName_preyTaxon, scientificNameID_preyTaxon) %>%
  distinct()  

differences_in_taxa_cols <- diets %>%
  filter(preyTaxon != Llopiz_taxa | 
           preyTaxon != scientificName_preyTaxon | 
           preyTaxon != preyTaxa | 
           Llopiz_taxa != scientificName_preyTaxon | 
           Llopiz_taxa != preyTaxa | 
           scientificName_preyTaxon != preyTaxa) %>%
  select(preyTaxon, Llopiz_taxa, scientificName_preyTaxon, preyTaxa) %>%
  distinct() %>%
  arrange(preyTaxon)

# remove Empty stomachs?
diets <- diets %>%
  filter(preyTaxon != "-9999" & preyTaxon != "Empty")

# not sure what "preyNum" column is
# in EDI is says "Unique number for prey specimen within each gut"
diets <- diets %>%
  dplyr::select(-c("preyNum", "preySpp", "Ocularunits", "Mag","prey_mm", "Scope"))

# preyCount = Total number of prey items per taxanomic category in the gut of fish specimen
## not sure what the NAs in this col mean...

# gutCount = Total number of prey items in that individual fish’s gut.
## gut count is not always filled out - sometimes NA
## and sometimes it seems likes it's wrong (e.g. preyCount > gutCount)

diets <- diets %>%
  group_by(FishSpecies, cruise, station, FishNum) %>%
  mutate(gutCount = sum(preyCount, na.rm = TRUE)) %>%
  ungroup()

fish_counts <- diets %>%
  group_by(year, season, FishSpecies) %>%
  summarise(fish_processed = n_distinct(FishNum), .groups = "drop") %>%
  group_by(year) %>%
  mutate(total_per_year = sum(fish_processed)) %>% 
  ungroup()

# create common name column
#P_tri = Peprilus triacanthus = Butterfish
#A_aes = Alosa aestivalis = Blueback Herring
#A_pse =	Alosa pseudoharengus = Alewife
#C_har = Clupea harengus = Atlantic Herring
#A_dub =	Ammodytes dubius = Sand Lance
#S_sco =	Scomber scombrus = Atlantic Mackerel

# fix typo in data... 
diets <- diets %>%
  mutate(FishSpecies = ifelse(FishSpecies == "p_tri", "P_tri", FishSpecies))

diets <- diets %>%
  mutate(FishCommon = case_when(
    FishSpecies == "P_tri" ~ "Butterfish",
    FishSpecies == "A_aes" ~ "Blueback Herring",
    FishSpecies == "A_pse" ~ "Alewife",
    FishSpecies == "C_har" ~ "Atlantic Herring",
    FishSpecies == "A_dub" ~ "Sand Lance",
    FishSpecies == "S_sco" ~ "Atlantic Mackerel",
    TRUE ~ NA_character_  # Assign NA if no match
  ))


#### ------------------------------------------ #####
#            Plots -----
#### ------------------------------------------ #####

#im setting the NAs in preyCount == 0 for now bc i dont know what they are
diets <- diets %>%
  mutate(preyCount = ifelse(is.na(preyCount), 0, preyCount))


#            scientificName_preyTaxon -----
#### ------------------------------------------ #####

# Calculate diet composition (%) per fish species, year, and season
diets_percent <- diets %>%
  group_by(FishSpecies, year, season, scientificName_preyTaxon) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE), .groups = "drop") %>%
  group_by(FishSpecies, year, season) %>%
  mutate(percent = (total_prey / sum(total_prey, na.rm = TRUE)) * 100)


fish_species_list <- unique(diets_percent$FishSpecies)

# Loop through each FishSpecies and create a separate plot
for (fish in fish_species_list) {
  plot <- ggplot(filter(diets_percent, FishSpecies == fish), 
                 aes(x = interaction(year, season, sep = " "), 
                     y = percent, fill = scientificName_preyTaxon)) +
    geom_bar(stat = "identity", position = "fill") +  # Stacked bars normalized to 100%
    theme_classic() +
    labs(title = paste("Diet Composition of", fish),
         x = "Year & Season", 
         y = "Diet Composition (%)", 
         fill = "Prey Taxon") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  print(plot)  
}





#            preyTaxon -----
#### ------------------------------------------ #####

# Calculate diet composition (%) per fish species, year, and season
diets_percent_preyTaxon <- diets %>%
  group_by(FishSpecies, year, season, preyTaxon) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE), .groups = "drop") %>%
  group_by(FishSpecies, year, season) %>%
  mutate(percent = (total_prey / sum(total_prey, na.rm = TRUE)) * 100)

# Loop through each FishSpecies and create a separate plot
for (fish in fish_species_list) {
  plot <- ggplot(filter(diets_percent_preyTaxon, FishSpecies == fish), 
                 aes(x = interaction(year, season, sep = " "), 
                     y = percent, fill = preyTaxon)) +
    geom_bar(stat = "identity", position = "fill") +  
    theme_classic() +
    labs(title = paste("Diet Composition of", fish),
         x = "Year & Season", 
         y = "Diet Composition (%)", 
         fill = "Prey Taxon") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  print(plot)  
}



#            preyTaxon grouped top 10 -----
#### ------------------------------------------ #####

# rename PPC to Calanoida
diets_v1 <- diets %>%
  mutate(preyTaxon = ifelse(preyTaxon == "PPC", "Calanoida", 
                            preyTaxon))

diets_v1 <- diets_v1 %>%
  mutate(preyTaxon = case_when(
    preyTaxon %in% c("Unknown", "UnknownOther", "UnknownSoft") ~ "Unknown",  
    TRUE ~ preyTaxon  # keep other values unchanged
  ))

# top 10 most common preyTaxon across all data
top_10_prey <- diets_v1 %>%
  group_by(preyTaxon) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE)) %>%
  arrange(desc(total_prey)) %>%
  slice_head(n = 10) %>%
  pull(preyTaxon) 

diets_v1 <- diets_v1 %>%
  mutate(preyTaxon = case_when(
    preyTaxon %in% top_10_prey ~ preyTaxon,  # keep top 10 prey taxa
    TRUE ~ "Other"  # group all other prey into "Other"
  ))

diets_percent_preyTaxon_g <- diets_v1 %>%
  group_by(FishSpecies, year, season, preyTaxon) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE), .groups = "drop") %>%
  group_by(FishSpecies, year, season) %>%
  mutate(percent = (total_prey / sum(total_prey, na.rm = TRUE)) * 100)

unique_prey <- unique(diets_percent_preyTaxon_g$preyTaxon)
color_palette <- brewer.pal(n = 11, name = "Paired") #"Set3" or "Dark2"
prey_color_mapping <- setNames(color_palette, unique_prey)

# to make title the fish's common name
fish_common_map <- diets %>%
  select(FishSpecies, FishCommon) %>%
  distinct() %>%
  deframe() 

# Loop through each FishSpecies and create a separate plot

for (fish in fish_species_list) {
  plot <- ggplot(filter(diets_percent_preyTaxon_g, FishSpecies == fish), 
                 aes(x = interaction(season, year, sep = " "), 
                     y = percent, fill = preyTaxon)) +
    geom_bar(stat = "identity", position = "fill") +  
    scale_fill_manual(values = prey_color_mapping) +
    theme_classic() +
    labs(title = fish_common_map[fish],
         y = "Diet Composition (%)") +  
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 14),
          axis.text.y = element_text(angle = 0, color = "black", size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),  
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          legend.position = "right",
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          legend.spacing.y = unit(0.2, "cm")) +
    scale_y_continuous(expand = c(0, 0), 
                       labels = scales::percent_format(scale = 100))
  
  print(plot)  
}


#####HEREEEE NEED TO FIX
# do the same but coloring unknown in like a bright color
bright_colors <- c("#FF0000", "#00FF00", "#0000FF", "orange",  "#FF00FF",
                   "#00FFFF", "#800000", "#808000", "#008000", "#800080", 
                   "#000080", "#808080", "#C0C0C0", "#FF6347", "#FFD700", 
                   "#32CD32", "#FF1493", "#B22222", "#FF4500", "#ADFF2F")

custom_palette <- c(
  "Unknown" = "#E63946",  # Bright Red (stands out)
  "Calanoida" = "#A8DADC",  # Soft Cyan
  "Euphausiidae" = "#457B9D",  # Muted Blue
  "Hyperiidea" = "#1D3557",  # Dark Blue
  "Pteropoda" = "#F4A261",  # Warm Orange
  "Copepoda" = "#E9C46A",  # Yellow-Gold
  "Other" = "#8D99AE",  # Neutral Gray
  "Amphipoda" = "#2A9D8F",  # Green-Teal
  "Decapoda" = "#264653",  # Dark Teal
  "Chaetognatha" = "#6A0572",  # Deep Purple
  "Ostracoda" = "#D4A5A5"  # Soft Pink
)

for (fish in fish_species_list) {
  plot <- ggplot(filter(diets_percent_preyTaxon_g, FishSpecies == fish), 
                 aes(x = interaction(season, year, sep = " "), 
                     y = percent, fill = preyTaxon)) +
    geom_bar(stat = "identity", position = "fill") +  
    scale_fill_manual(values = custom_palette) +
    theme_classic() +
    labs(title = fish_common_map[fish],
         y = "Diet Composition (%)") +  
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 14),
          axis.text.y = element_text(angle = 0, color = "black", size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),  
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          legend.position = "right",
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          legend.spacing.y = unit(0.2, "cm")) +
    scale_y_continuous(expand = c(0, 0), 
                       labels = scales::percent_format(scale = 100))
  
  print(plot)  
}



#            Llopiz_taxa grouped top 10 -----
#### ------------------------------------------ #####


# then do same with llopiz taxa group bc calanoid small better described