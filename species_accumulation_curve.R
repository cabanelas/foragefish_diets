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
library(vegan)
library(taxize)

#### ------------------------------------------ #####
#            Data -----
#### ------------------------------------------ #####
diets <- read.csv(file.path("raw","Concat_Count_EDI.csv"))

#### ------------------------------------------ #####
#            Tidy -----
#### ------------------------------------------ #####
# P_tri typo
diets <- diets %>%
  mutate(FishSpecies = ifelse(FishSpecies == "p_tri", "P_tri", FishSpecies))

# create unique stomach ID
diets <- diets %>%
  mutate(stomach_id = paste(cruise, station, FishSpecies, FishNum, sep = "_"))

# remove missing, "Other", "-9999", and other bad values
clean_diets <- diets %>%
  filter(!is.na(scientificName_preyTaxon)) %>%
  filter(!scientificName_preyTaxon %in% c("-9999", "Other", "Unknown", "Empty", "UnknownOther", "Animalia"))

# scientificName_preyTaxon 
presence_matrix <- clean_diets %>%
  distinct(stomach_id, scientificName_preyTaxon) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = scientificName_preyTaxon, 
              values_from = present, 
              values_fill = 0) %>%
  column_to_rownames("stomach_id")

#### ------------------------------------------ #####
#            Species accumulation curve -----
#### ------------------------------------------ #####
accum_curve <- specaccum(presence_matrix, method = "random")

# Plot all fish species together
par(mfrow = c(1, 1)) 
plot(accum_curve, ci.type = "polygon", col = "blue", lwd = 2,
     ci.lty = 0, ci.col = rgb(0, 0, 1, 0.3),
     xlab = "Number of stomachs", ylab = "Cumulative prey taxa",
     main = "Species Accumulation Curve – All Fish")

# Plot all fish separately
unique_species <- unique(clean_diets$FishSpecies)
accum_list <- list()

par(mfrow = c(2, 3)) 
for (sp in unique_species) {
  sub_df <- clean_diets %>%
    filter(FishSpecies == sp) %>%
    distinct(stomach_id, scientificName_preyTaxon) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = scientificName_preyTaxon, 
                values_from = present, 
                values_fill = 0) %>%
    column_to_rownames("stomach_id")
  
  if (nrow(sub_df) > 1) {
    acc <- specaccum(sub_df, method = "random")
    accum_list[[sp]] <- acc
    plot(acc, ci.type = "polygon", col = "black", lwd = 2,
         ci.lty = 0, ci.col = rgb(0, 0, 0, 0.2),
         main = sp, xlab = "Number of stomachs", ylab = "Prey taxa")
  }
}

# --- Function to find when curve reaches 95% of max ---
estimate_plateau <- function(accum_obj, threshold = 0.95) {
  max_rich <- max(accum_obj$richness)
  plateau_level <- max_rich * threshold
  idx <- which(accum_obj$richness >= plateau_level)[1]
  
  tibble(
    stomachs_needed = accum_obj$sites[idx],
    richness_reached = accum_obj$richness[idx],
    max_richness = max_rich
  )
}

# --- Function to check if curve has likely plateaued ---
check_plateau <- function(acc) {
  last_vals <- tail(acc$richness, 5)
  diffs <- diff(last_vals)
  mean_slope <- mean(diffs)
  mean_slope < 0.5  # adjust threshold if needed
}

# --- Function to fit asymptotic model ---
fit_model_safe <- safely(function(acc) {
  fitspecaccum(acc, model = "arrhenius") # or "gompertz", "michaelis"
})

# --- Apply to all species ---
plateau_df <- map_dfr(accum_list, estimate_plateau, .id = "FishSpecies")
plateau_df$plateau_reached <- map_lgl(accum_list, check_plateau)

fit_models <- map(accum_list, fit_model_safe)

plateau_df$asymptotic_richness <- map_dbl(fit_models, ~{
  if (!is.null(.x$result)) {
    coef(.x$result)["Asym"]
  } else {
    NA_real_
  }
})

print(plateau_df)


#### ------------------------------------------ #####
#            Final plot -----
#### ------------------------------------------ #####

# filter out A_dub 
plot_species <- plateau_df %>%
  filter(FishSpecies != "A_dub")

# long format with CIs
accum_df <- map2_dfr(
  .x = accum_list[plot_species$FishSpecies],
  .y = names(accum_list[plot_species$FishSpecies]),
  ~ tibble(
    FishSpecies = .y,
    stomachs = .x$sites,
    richness = .x$richness,
    sd = .x$sd,
    lower = .x$richness - .x$sd,
    upper = .x$richness + .x$sd
  )
)

accum_df <- left_join(accum_df, plot_species, by = "FishSpecies")

common_names <- c(
  C_har = "Atlantic Herring",
  P_tri = "Atlantic Mackerel",
  S_sco = "Butterfish",
  A_aes = "Blueback Herring",
  A_pse = "Alewife"
)

accum_df <- accum_df %>%
  filter(FishSpecies %in% names(common_names)) %>%
  mutate(FishCommon = common_names[FishSpecies])

text_labels <- distinct(accum_df, FishSpecies, FishCommon, stomachs_needed)

# Plot
(sp_curve_all <- ggplot(accum_df, aes(x = stomachs, y = richness)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "gray80", alpha = 0.5) +
  geom_line(color = "black", linewidth = 1) +
  facet_wrap(~FishCommon, scales = "free_x") + #
  geom_vline(aes(xintercept = stomachs_needed), 
             linetype = "dashed", color = "blue") +
  geom_text(data = text_labels,
            aes(x = stomachs_needed, y = Inf, 
                label = paste0(stomachs_needed, " stomachs")),
           # number of stomachs required to reach 95% of the max observed prey richness
             vjust = 1.5, hjust = 1.3, color = "black", size = 3.9) +
  labs(x = "Number of stomachs", y = "Cumulative prey taxa",
       title = "Species Accumulation Curves") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", size = 14),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(size = 14))
)

#ggsave("sp_accum_curves_allspecies.png", plot = sp_curve_all, width = 10, height = 6, dpi = 300, bg = "white", units = "in")

# Plot v2
accum_df <- accum_df %>%
  mutate(FishLabel = paste0(FishCommon, " (", stomachs_needed, ")"))

text_labels <- text_labels %>%
  mutate(FishLabel = paste0(FishCommon, " (", stomachs_needed, ")"))

(sp_curve_all_onepanel <- ggplot(accum_df, aes(x = stomachs, y = richness, color = FishLabel)) +
  geom_line(linewidth = 1.3) +
  labs(x = "Number of stomachs", y = "Cumulative prey taxa",
       title = "Species Accumulation Curves") +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = c(0.7, 0.3),
        legend.text = element_text(size = 14),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(size = 14))
)

#ggsave("sp_accum_curves_allspecies_onepanel.png", plot = sp_curve_all_onepanel, width = 10, height = 6, dpi = 300, bg = "white", units = "in")

# due to uneven sampling sizes
# can compare prey richness across species at the same sample size by...
# rarefaction

# Create list of prey matrices per fish species
prey_matrices <- list()

for (sp in unique(clean_diets$FishSpecies)) {
  mat <- clean_diets %>%
    filter(FishSpecies == sp) %>%
    distinct(stomach_id, scientificName_preyTaxon) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = scientificName_preyTaxon, 
                values_from = present, values_fill = 0) %>%
    column_to_rownames("stomach_id")
  
  if (nrow(mat) >= 50) {  # Only keep if enough stomachs to rarefy
    prey_matrices[[sp]] <- mat
  }
}

# rarefy to 50 stomachs per species
rarefied <- map_dbl(prey_matrices, ~ {
  row_sums <- rowSums(.x)
  rarefy(row_sums, sample = 50)
})

rarefied
# these values are the expected number of unique prey taxa (i.e., prey richness)
# you’d observe if you randomly sampled exactly 50 stomachs from each species


# Number of stomachs processed 
sample_table <- diets %>%
  group_by(FishSpecies) %>%
  summarise(
    total_stomachs = n_distinct(stomach_id),
    non_empty_stomachs = n_distinct(stomach_id[!is.na(scientificName_preyTaxon)]),
    .groups = "drop"
  ) %>%
  mutate(
    FishCommon = common_names[FishSpecies],
    pct_empty = 100 * (1 - non_empty_stomachs / total_stomachs)
  ) %>%
  select(FishSpecies, FishCommon, total_stomachs, non_empty_stomachs, pct_empty)

library(knitr)

kable(sample_table, format = "markdown")

#### ------------------------------------------ #####
#      Grouping prey items to broader level -----
#### ------------------------------------------ #####
taxa <- colnames(presence_matrix)

# get GBIF IDs; suppress messages for clarity
gbif_ids <- get_gbifid(taxa, messages = FALSE, ask = FALSE)
gbif_class <- classification(gbif_ids, db = "gbif")

order_df <- lapply(gbif_class, function(x) {
  if (!is.data.frame(x)) return(NA)
  order_row <- x[x$rank == "order", ]
  if (nrow(order_row) > 0) return(order_row$name) else return(NA)
}) %>% unlist()

taxon_to_order <- data.frame(
  original_name = taxa,
  order = order_df,
  stringsAsFactors = FALSE
)

extract_rank <- function(class_list, rank_name) {
  sapply(class_list, function(x) {
    if (!is.data.frame(x)) return(NA)
    row <- x[x$rank == rank_name, ]
    if (nrow(row) > 0) return(row$name) else return(NA)
  })
}

# get each rank
genus_df  <- extract_rank(gbif_class, "genus")
family_df <- extract_rank(gbif_class, "family")
order_df  <- extract_rank(gbif_class, "order")
class_df  <- extract_rank(gbif_class, "class")

# combine into one data frame
taxon_lookup <- data.frame(
  original_name = taxa,
  genus = genus_df,
  family = family_df,
  order = order_df,
  class = class_df,
  stringsAsFactors = FALSE
)

# patch known missing values
taxon_lookup$order[which(taxon_lookup$original_name == "Calanus")] <- "Calanoida"
taxon_lookup$order[which(taxon_lookup$original_name == "Temora longicornis")] <- "Calanoida"
taxon_lookup$order[which(taxon_lookup$original_name == "Hyperia")] <- "Amphipoda"
taxon_lookup$order[which(taxon_lookup$original_name == "Thysanopoda")] <- "Euphausiacea"
taxon_lookup$order[which(taxon_lookup$original_name == "Gammarus")] <- "Amphipoda"
taxon_lookup$order[taxon_lookup$original_name == "Chaetognatha"] <- "Chaetognatha" #phylum
taxon_lookup$order[taxon_lookup$original_name == "Ostracoda"] <- "Ostracoda" #class
taxon_lookup$order[taxon_lookup$original_name == "Cladocera"] <- "Cladocera" #Diplostraca is the accepted name
taxon_lookup$order[taxon_lookup$original_name == "Ammodytes"] <- "Ammodytes" # order Perciformes
taxon_lookup$order[taxon_lookup$original_name == "Polychaeta"] <- "Polychaeta"
taxon_lookup$order[taxon_lookup$original_name == "Isopoda"] <- "Isopoda"
taxon_lookup$order[taxon_lookup$original_name == "Nematoscelis"] <- "Euphausiacea"
taxon_lookup$order[taxon_lookup$original_name == "Appendicularia"] <- "Appendicularia"
taxon_lookup$order[taxon_lookup$original_name == "Copepoda"] <- "Copepoda"
taxon_lookup$order[taxon_lookup$original_name == "Cirripedia"] <- "Cirripedia"
taxon_lookup$order[taxon_lookup$original_name == "Decopoda"] <- "Decapoda"
taxon_lookup$order[taxon_lookup$original_name == "Gnathostomata"] <- "Gnathostomata"
taxon_lookup$order[taxon_lookup$original_name == "Bivalvia"] <- "Bivalvia"  
taxon_lookup$order[taxon_lookup$original_name == "Themisto"] <- "Amphipoda"
taxon_lookup$order[taxon_lookup$original_name == "Temora"] <- "Calanoida"
taxon_lookup$order[which(taxon_lookup$original_name == "Clione")] <- "Pteropoda"
taxon_lookup$order[which(taxon_lookup$original_name == "Oithona")] <- "Cyclopoida"
taxon_lookup$order[which(taxon_lookup$original_name == "Caridea")] <- "Decapoda"
taxon_lookup$order[taxon_lookup$original_name == "Gastropoda"] <- "Gastropoda"
taxon_lookup$order[taxon_lookup$original_name == "Limacina"] <- "Pteropoda"
taxon_lookup$order[taxon_lookup$original_name == "Thaliacea"] <- "Thaliacea"
taxon_lookup$order[taxon_lookup$original_name == "Tortanus discaudatus"] <- "Calanoida"
taxon_lookup$order[taxon_lookup$original_name == "Foraminiferida"] <- "Foraminiferida" 
taxon_lookup$order[taxon_lookup$original_name == "Eucalanus"] <- "Calanoida"
taxon_lookup$order[taxon_lookup$original_name == "Coleoidea"] <- "Coleoidea"


diets_with_order <- clean_diets %>%
  left_join(taxon_lookup, 
            by = c("scientificName_preyTaxon" = "original_name")) %>%
  filter(!is.na(order)) 

presence_by_order <- diets_with_order %>%
  distinct(stomach_id, order) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = order, values_from = present, values_fill = 0) %>%
  column_to_rownames("stomach_id")

accum_curve <- specaccum(presence_by_order, method = "random")

# Plot all fish separately
accum_list <- list()
par(mfrow = c(2, 3)) 
for (sp in unique_species) {
  # Subset and join order-level taxonomy
  sub_df <- clean_diets %>%
    filter(FishSpecies == sp) %>%
    left_join(taxon_lookup, by = c("scientificName_preyTaxon" = "original_name")) %>%
    filter(!is.na(order)) %>%
    distinct(stomach_id, order) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = order, values_from = present, values_fill = 0) %>%
    column_to_rownames("stomach_id")
  
  # Only plot if >1 stomach
  if (nrow(sub_df) > 1) {
    acc <- specaccum(sub_df, method = "random")
    accum_list[[sp]] <- acc
    plot(acc, ci.type = "polygon", col = "black", lwd = 2,
         ci.lty = 0, ci.col = rgb(0, 0, 0, 0.2),
         main = sp, xlab = "Number of stomachs", ylab = "Prey orders")
  }
}

## adding vertical line showing 95%
estimate_plateau <- function(accum_obj, threshold = 0.95) {
  max_rich <- max(accum_obj$richness)
  plateau_level <- max_rich * threshold
  idx <- which(accum_obj$richness >= plateau_level)[1]
  
  tibble(
    stomachs_needed = accum_obj$sites[idx],
    richness_reached = accum_obj$richness[idx],
    max_richness = max_rich
  )
}

# Get info for each species (based on prey order)
plateau_order_df <- map_dfr(accum_list, estimate_plateau, .id = "FishSpecies")

# Build long-format df with CI
accum_order_df <- map2_dfr(
  .x = accum_list[plateau_order_df$FishSpecies],
  .y = names(accum_list[plateau_order_df$FishSpecies]),
  ~ tibble(
    FishSpecies = .y,
    stomachs = .x$sites,
    richness = .x$richness,
    sd = .x$sd,
    lower = .x$richness - .x$sd,
    upper = .x$richness + .x$sd
  )
)

# Join to add stomach threshold
accum_order_df <- left_join(accum_order_df, plateau_order_df, by = "FishSpecies")

common_names <- c(
  C_har = "Atlantic Herring",
  P_tri = "Atlantic Mackerel",
  S_sco = "Butterfish",
  A_aes = "Blueback Herring",
  A_pse = "Alewife"
)

accum_order_df <- accum_order_df %>%
  filter(FishSpecies %in% names(common_names)) %>%
  mutate(FishCommon = common_names[FishSpecies])

text_labels <- distinct(accum_order_df, FishSpecies, FishCommon, stomachs_needed)


ggplot(accum_order_df, aes(x = stomachs, y = richness)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "gray80", alpha = 0.5) +
  geom_line(color = "black", linewidth = 1) +
  facet_wrap(~FishCommon, scales = "free_x") +
  geom_vline(aes(xintercept = stomachs_needed), 
             linetype = "dashed", color = "blue") +
  geom_text(data = text_labels,
            aes(x = stomachs_needed, y = Inf, 
                label = paste0(stomachs_needed, " stomachs")),
            vjust = 1.5, hjust = 1.3, color = "black", size = 3.9) +
  labs(x = "Number of stomachs", y = "Cumulative prey orders",
       title = "Species Accumulation Curves (Prey Orders)") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", size = 14),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(size = 14))
