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
library(patchwork)
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

# redo gutCount column 
diets <- diets %>%
  group_by(FishSpecies, cruise, station, FishNum) %>%
  mutate(gutCount = sum(preyCount, na.rm = TRUE)) %>%
  ungroup()

# number of guts processed per season, yearb by predator species
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

#im setting the NAs in preyCount == 0 for now bc i dont know what they are
diets <- diets %>%
  mutate(preyCount = ifelse(is.na(preyCount), 0, preyCount))


#### ------------------------------------------ #####
#            Plots -----
#### ------------------------------------------ #####

#            scientificName_preyTaxon -----
#### ------------------------------------------ #####

# calculate diet composition (%) per fish species, year, and season
diets_percent <- diets %>%
  group_by(FishSpecies, year, season, scientificName_preyTaxon) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE), .groups = "drop") %>%
  group_by(FishSpecies, year, season) %>%
  mutate(percent = (total_prey / sum(total_prey, na.rm = TRUE)) * 100)

# to make title the fish's common name
fish_common_map <- diets %>%
  select(FishSpecies, FishCommon) %>%
  distinct() %>%
  deframe() 

fish_species_list <- unique(diets_percent$FishSpecies)

# loop through each FishSpecies and create plots
for (fish in fish_species_list) {
  plot <- ggplot(filter(diets_percent, FishSpecies == fish), 
                 aes(x = interaction(year, season, sep = " "), 
                     y = percent, fill = scientificName_preyTaxon)) +
    geom_bar(stat = "identity", position = "fill") +  
    theme_classic() +
    labs(title = fish_common_map[fish],
         x = "Year & Season", 
         y = "Diet Composition (%)", 
         fill = "Prey Taxon") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  print(plot)  
}

#            preyTaxon -----
#### ------------------------------------------ #####

# calculate diet composition (%) per fish species, year, and season
diets_percent_preyTaxon <- diets %>%
  group_by(FishSpecies, year, season, preyTaxon) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE), .groups = "drop") %>%
  group_by(FishSpecies, year, season) %>%
  mutate(percent = (total_prey / sum(total_prey, na.rm = TRUE)) * 100)

# loop to plot
for (fish in fish_species_list) {
  plot <- ggplot(filter(diets_percent_preyTaxon, FishSpecies == fish), 
                 aes(x = interaction(year, season, sep = " "), 
                     y = percent, fill = preyTaxon)) +
    geom_bar(stat = "identity", position = "fill") +  
    theme_classic() +
    labs(title = fish_common_map[fish],
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
color_palette <- brewer.pal(n = 11, name = "RdGy") #"Paired", "Set3" or "Dark2"
prey_color_mapping <- setNames(color_palette, unique_prey)

# loop to plot

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

### SAME BUT OTHER PALETTE/COLOR OPTIONS
# do the same but coloring unknown in like a bright color
bright_colors <- c("#FF0000", "#00FF00", "#0000FF", "orange",  "#FF00FF",
                   "#00FFFF", "#800000", "#808000", "#008000", "#800080", 
                   "#000080", "#808080", "#C0C0C0", "#FF6347", "#FFD700", 
                   "#32CD32", "#FF1493", "#B22222", "#FF4500", "#ADFF2F")

custom_palette <-  c(
  "Unknown" = "#E63946",  # Bright Red (stands out)
  "Other" = "#8D99AE",  # Neutral Gray for "Other"
  "Calanoida" = "#A8DADC",  # Soft Cyan
  "Calanus spp." = "#457B9D",  # Muted Blue
  "Centropages spp." = "#1D3557",  # Dark Blue
  "Euphausia spp." = "#F4A261",  # Warm Orange
  "Neomysis spp." = "#E9C46A",  # Yellow-Gold
  "Oikopleura spp." = "#2A9D8F",  # Green-Teal
  "Temora longicornis" = "#264653",  # Dark Teal
  "Appendicularia" = "#6A0572",  # Deep Purple
  "CalanoidSmall" = "#D4A5A5"  # Soft Pink
)

custom_palette <- c(
  "Unknown" = "#D73027",  # Strong Red (stands out)
  "Other" = "#7E7E7E",  # Neutral Gray (background)
  "Calanoida" = "#1F78B4",  # Deep Blue
  "Calanus spp." = "#6BAED6",  # Soft Blue
  "Centropages spp." = "#A6CEE3",  # Light Blue
  "Euphausia spp." = "#33A02C",  # Deep Green
  "Neomysis spp." = "#B2DF8A",  # Light Green
  "Oikopleura spp." = "#FDAE61",  # Warm Orange
  "Temora longicornis" = "#F46D43",  # Orange-Red
  "Appendicularia" = "#FDBF6F",  # Soft Peach
  "CalanoidSmall" = "#CAB2D6"  # Soft Purple
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
# doing same with Llopiz_taxa bc calanoid small better described

diets_v2 <- diets %>%
  mutate(Llopiz_taxa = case_when( # Thaliacea ??
    Llopiz_taxa %in% c("Unknown", "UnknownOther", "UnknownSoft") ~ "Unknown",  
    TRUE ~ Llopiz_taxa  # keep other values unchanged
  ))

# fill in the NAs with some of the prey names in the scientificName_preyTaxon col
diets_v2 <- diets_v2 %>%
  mutate(Llopiz_taxa = ifelse(is.na(Llopiz_taxa), 
                              scientificName_preyTaxon, Llopiz_taxa))

# fill remaining NAs with prey names in the preyTaxon col
diets_v2 <- diets_v2 %>%
  mutate(Llopiz_taxa = ifelse(is.na(Llopiz_taxa), 
                              preyTaxon, Llopiz_taxa))

# group prey into broader categories:
# Calanoida = candacia, acartia, CalanoidDigested, Eucalanus, Eurytemora, 
##Paraeuchaeta, Pseudodiaptomas, Rhincalanus, Tortanus, Tortanus discaudatus, CalanoidSmall

# Copepoda = CopepodDigested, CopepodUnknown, Corycaeus (cyclopoid), Cyclopoida, 
##Monstrilloida, Microsetella, Sapphirina, Sapphirinidae

# Mysida = "Mysida", "Neomysis", "Mysidae", "Mysis", "Mysis mixta", "Americamysis"'
# FishItems = Ammodytes, Fish_Remains, FishEgg, FishLarvae, FishRemains, Larval_Fish
# Appendicularia
# Bivalvia
# Amphipoda = Ampithoidae, Aoridae, Caprellidae, Corophiidae, Liljeborgiidae
# Pteropoda = Clione, Clione limacina, Limacina, Limacina helicina, Thecosomata
# Euphausiidae = Euphausia, Euphausiacea, Meganyctiphanes norvegica, 
##Nematoscelis, Thysanoessa, Thysanopoda

# Gammaridea = Gammarus
# Hyperiidea = Hyperia, Hyperiella, Hyperiidae, Phronima, Phronimidae, Pronoidae, Themistella, Themisto
# Decapoda = Caridea, Penaeidae

# Temora spp. = Temora, Temora longicornis 
# Other = Unknown 
diets_v2 <- diets_v2 %>%
  mutate(Llopiz_taxa = case_when(
    Llopiz_taxa == "Oikopleura" ~ "Appendicularia",
    Llopiz_taxa == "Neomysis" ~ "Mysida",
    Llopiz_taxa == "Parathemisto" ~ "Hyperiidae",
    Llopiz_taxa == "Gammarus" ~ "Gammaridea",
    Llopiz_taxa == "Other" ~ "Unknown", 
    Llopiz_taxa %in% c("Candacia", "Acartia", "CalanoidDigested", "Eucalanus", 
                       "Eurytemora", "Paraeuchaeta", "Pseudodiaptomas", 
                       "Rhincalanus", "Tortanus", "Tortanus discaudatus", 
                       "CalanoidSmall") ~ "Calanoida",
    
    Llopiz_taxa %in% c("CopepodDigested", "CopepodUnknown", "Corycaeus", 
                       "Cyclopoida", "Monstrilloida", "Microsetella", 
                       "Sapphirina", "Sapphirinidae") ~ "Copepoda",
    
    Llopiz_taxa %in% c("Mysida", "Neomysis", "Mysidae", "Mysis", "Mysis mixta", 
                       "Americamysis") ~ "Mysida",
    
    Llopiz_taxa %in% c("Ammodytes", "Fish_Remains", "FishEgg", "FishLarvae", 
                       "FishRemains", "Larval_Fish") ~ "FishItems",
    
    Llopiz_taxa %in% c("Ampithoidae", "Aoridae", "Caprellidae", "Corophiidae", 
                       "Liljeborgiidae") ~ "Amphipoda",
    
    Llopiz_taxa %in% c("Clione", "Clione limacina", "Limacina", 
                       "Limacina helicina", "Thecosomata") ~ "Pteropoda",
    
    Llopiz_taxa %in% c("Euphausia", "Euphausiacea", "Meganyctiphanes norvegica", 
                       "Nematoscelis", "Thysanoessa", 
                       "Thysanopoda") ~ "Euphausiidae",
    
    Llopiz_taxa %in% c("Hyperia", "Hyperiella", "Hyperiidae", "Phronima", 
                       "Phronimidae", "Pronoidae", "Themistella", 
                       "Themisto") ~ "Hyperiidea",
    
    Llopiz_taxa %in% c("Caridea", "Penaeidae") ~ "Decapoda",
    
    Llopiz_taxa %in% c("Temora", "Temora longicornis") ~ "Temora spp.",
    
    TRUE ~ Llopiz_taxa  # everything else unchanged
  ))

# ??? left as is
# Calanus
# Caligidae = sea lice = could go into parasite or Copepoda = Caligus
# Cirripedia (barnacle) = Arthropoda thecostraca
# there is a Crustacea group
# Cumacea = Arthropoda malacostraca 
# there is a Gastropoda group
# there is a Isopoda group
# Hydrachnidae = water mite = Arthropoda = possibly into parasites
# Metridia = calanoid copepod 
# Oithona = copepoda
# Penilia = Artropoda ctenopoda = Penilia avirostris
# Pleuromamma = calanoida 
# Podon = Arthropoda onychopoda
# Tomopteris = annelid 
# Ostracoda
# Haustoriidae = Arthropoda crustacean malacostraca 
# Animalia
# Lycaenidae = butterfly ??
# Melittidae = beetles??

# top 10 most common Llopiz_taxa across all data
top_10_Llopiz <- diets_v2 %>%
  group_by(Llopiz_taxa) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE)) %>%
  arrange(desc(total_prey)) %>%
  slice_head(n = 10) %>%
  pull(Llopiz_taxa) 

diets_v3 <- diets_v2 %>%
  mutate(Llopiz_taxa = case_when(
    Llopiz_taxa %in% top_10_Llopiz ~ Llopiz_taxa,  # keep top 10 prey taxa
    TRUE ~ "Other"  # group all other prey into "Other"
  ))

diets_percent_Llopiz_taxa_g <- diets_v3 %>%
  group_by(FishSpecies, year, season, Llopiz_taxa) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE), .groups = "drop") %>%
  group_by(FishSpecies, year, season) %>%
  mutate(percent = (total_prey / sum(total_prey, na.rm = TRUE)) * 100)

unique_preyLlopiz <- unique(diets_percent_Llopiz_taxa_g$Llopiz_taxa)
prey_color_mappingLlopiz <- setNames(color_palette, unique_preyLlopiz)

# loop to plot

for (fish in fish_species_list) {
  plot <- ggplot(filter(diets_percent_Llopiz_taxa_g, FishSpecies == fish), 
                 aes(x = interaction(season, year, sep = " "), 
                     y = percent, fill = Llopiz_taxa)) +
    geom_bar(stat = "identity", position = "fill") +  
    scale_fill_manual(values = prey_color_mappingLlopiz) +
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


#            2018 and 2019 ONLY -----
#### ------------------------------------------ #####
#            Llopiz_taxa grouped top 10 -----
#### ------------------------------------------ #####

top_10_Llopiz_new <- diets_v2 %>%
  filter(year %in% c(2018, 2019) & 
           season %in% c("Fall", "Spring")) %>%
  group_by(Llopiz_taxa) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE)) %>%
  arrange(desc(total_prey)) %>%
  slice_head(n = 11) %>%
  pull(Llopiz_taxa) 

diets_v4 <- diets_v2 %>%
  mutate(Llopiz_taxa = case_when(
    Llopiz_taxa %in% top_10_Llopiz_new ~ Llopiz_taxa,  # keep top 10 prey taxa
    TRUE ~ "Other"  # group all other prey into "Other"
  ))

diets_percent_Llopiz_taxa_g_new <- diets_v4 %>%
  filter(year %in% c(2018, 2019) & season %in% c("Fall", "Spring")) %>%
  group_by(FishSpecies, year, season, Llopiz_taxa) %>%
  summarise(total_prey = sum(preyCount, na.rm = TRUE), .groups = "drop") %>%
  group_by(FishSpecies, year, season) %>%
  mutate(percent = (total_prey / sum(total_prey, na.rm = TRUE)) * 100,
         Llopiz_taxa = factor(Llopiz_taxa, 
                              levels = c(setdiff(top_10_Llopiz_new, 
                                                 c("Other", "Unknown")), 
                                         "Other", "Unknown")))  # Force order

unique_preyLlopiz_new <- unique(diets_percent_Llopiz_taxa_g_new$Llopiz_taxa)

#make custom color palette (to avoid white, and add more colors)
#print(color_palette)
prey_color_mappingLlopiznew <- c(
  "Appendicularia" = "#67001F",
  "Calanus" = "#664700",
  "Calanoida" = "#CD950C", #"#D6604D",
  "Centropages" = "#FFB90F", #E49B5D", #"#F4A582", 
  "Chaetognatha" = "#6E8B3D",#"#8C510A"
  "Copepoda" = "#c7522a", #"#A63D40"maybe it looks too similar to B218
  "Euphausiidae" = "#4D4D4D",
  "Hyperiidea" = "#9ACD32", #"#BABABA", 
  "Mysida" = "#878787",
  "Temora spp." = "#006647",
  "Other" = "#D8BFD8", #"#1A1A1A", 
  "Unknown" = "#FA8072" #"#B2182B", "#6959CD", "#4A708B", "#D8BFD8"
)

# loop to plot
for (fish in fish_species_list) {
  plot <- ggplot(filter(diets_percent_Llopiz_taxa_g_new, 
                        FishSpecies == fish), 
                 aes(x = interaction(season, year, sep = " "), 
                     y = percent, fill = Llopiz_taxa)) +
    geom_bar(stat = "identity", position = "fill") +  
    scale_fill_manual(values = prey_color_mappingLlopiznew) +
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


#            2018 and 2019 ONLY -----
#### ------------------------------------------ #####
#            Atl Mackerel and Butterfish ONLY -----
#### ------------------------------------------ #####
#            Llopiz_taxa grouped top 10 -----
#### ------------------------------------------ #####

#P_tri = Peprilus triacanthus = Butterfish
#S_sco =	Scomber scombrus = Atlantic Mackerel
selected_fish <- c("P_tri", "S_sco")

diets_filtered <- diets_percent_Llopiz_taxa_g_new %>%
  filter(FishSpecies %in% selected_fish)

diets_filtered <- diets_filtered %>%
  mutate(season = factor(season, levels = c("Spring", "Fall")),  # ensure Spring comes first
         x_order = interaction(season, year, sep = " "))

(plot_mackerel <- ggplot(filter(diets_filtered, FishSpecies == "S_sco"), 
                        aes(x = x_order, 
                            y = percent, fill = Llopiz_taxa)) +
  geom_bar(stat = "identity", position = "fill") +  
  scale_fill_manual(values = prey_color_mappingLlopiznew) +
  theme_classic() +
  labs(title = "Atlantic Mackerel",
       y = "Diet Composition (%)") +  
  theme(axis.text.x = element_text(hjust = 0.5, color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17, face = "bold"),  
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position = "none")  +
    scale_y_continuous(expand = c(0, 0), 
                       labels = scales::percent_format(scale = 100)))  

(plot_butterfish <- ggplot(filter(diets_filtered, FishSpecies == "P_tri"), 
                          aes(x = x_order, 
                              y = percent, fill = Llopiz_taxa)) +
  geom_bar(stat = "identity", position = "fill") +  
  scale_fill_manual(values = prey_color_mappingLlopiznew) +
  theme_classic() +
  labs(title = "Butterfish",
       y = "Diet Composition (%)") +  
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 14),
        axis.text.y = element_text(angle = 0, color = "black", size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),  
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position = "right",  # Keep legend in one plot
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.y = unit(0.2, "cm"))  +
    scale_y_continuous(expand = c(0, 0), 
                       labels = scales::percent_format(scale = 100)))

(final_plot <- plot_mackerel + 
               plot_butterfish + 
               plot_layout(guides = "collect"))

ggsave("figures/AtlanticMackerel_Butterfish_Diet1.png", 
      plot = final_plot, 
      width = 10, height = 4, 
      dpi = 300, 
      units = "in")
