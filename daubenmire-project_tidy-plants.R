##  ------------------------------------------------------------  ##
          # Daubenmire Project - Plant Quadrat Tidying
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Wrangle data on plant functional groups in southern Iowa/northern Missouri

##  ------------------------------------------  ##      
                # Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Clear environment & collect garbage
rm(list = ls()); gc()

##  ------------------------------------------  ##      
                  # Wrangling ----
##  ------------------------------------------  ##      

# Read in data
daub_v1 <- read.csv(file = file.path("data", "raw-data", "daubenmire-project_raw-data.csv"))

# Check structure
dplyr::glimpse(daub_v1)

# Reshape data to get all ostensibly numeric columns into one column
daub_v2 <- daub_v1 %>% 
  dplyr::mutate(row_id = 1:nrow(.), .before = dplyr::everything()) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = as.character)) %>% 
  tidyr::pivot_longer(cols = Robel.N:Angle_of_O,
                      names_to = "vars", values_to = "values")

# Check numeric data
supportR::num_check(data = daub_v2, col = "values")

# Replace problem values and re-reshape back to original format
daub_v3 <- daub_v2 %>% 
  dplyr::mutate(values = dplyr::case_when(
    values %in% c("", " ", ".") ~ NA,
    T ~ values)) %>% 
  dplyr::mutate(values = as.numeric(values)) %>% 
  tidyr::pivot_wider(names_from = vars, values_from = values)

# Re-check structure
dplyr::glimpse(daub_v3)

# Re-calculate average/std. dev of Robel
robel_v1 <- daub_v3 %>% 
  dplyr::select(-Avg_Robel, -SD_Robel) %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("Robel.")) %>% 
  dplyr::group_by(Pasture_Patch_Year_Transect) %>% 
  dplyr::summarize(mean.robel_dm = mean(value, na.rm = T),
                   std.dev.robel_dm = sd(value, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()
  
# Check structure
dplyr::glimpse(robel_v1)

# Update Robel data in original dataset
daub_v4 <- daub_v3 %>%
  dplyr::select(-Avg_Robel, -SD_Robel) %>% 
  dplyr::rename(robel.north_dm = Robel.N,
                robel.east_dm = Robel.E,
                robel.south_dm = Robel.S,
                robel.west_dm = Robel.W) %>% 
  dplyr::left_join(y = robel_v1, by = "Pasture_Patch_Year_Transect") %>% 
  dplyr::relocate(dplyr::contains("robel"), .before = WSG)

# Re-check structure
dplyr::glimpse(daub_v4)

# Remaining vegetation categories were quantified as pseudo-categorical percents
## Only allowed values are: 0, 1, 3, 16, 38, 63, 86, 98
veg_v1 <- daub_v4 %>% 
  dplyr::select(row_id, Pasture_Patch_Year_Transect, WSG:Litter, Seed_mix) %>% 
  tidyr::pivot_longer(cols = -row_id:-Pasture_Patch_Year_Transect)

# Check values
sort(unique(veg_v1$value))

# Repair back into allowed bins & make better column names
veg_v2 <- veg_v1 %>% 
  dplyr::mutate(value = dplyr::case_when(
    value == 2 ~ 3,
    value %in% c(6, 15, 165) ~ 16,
    value %in% c(28, 36, 58) ~ 38,
    value == 80 ~ 86,
    value == 96 ~ 98,
    T ~ value)) %>% 
  dplyr::mutate(name = dplyr::case_when(
    name == "Bare" ~ "bare.ground_binned.perc",
    name == "CSG" ~ "cool.season.grass_binned.perc",
    name %in% c("Fescue", "Forbs", "Legumes", 
                "Sedges") ~ paste0(tolower(name), "_binned.perc"),
    name == "Seed_mix" ~ "seedmix.forbs_binned.perc.of.all.forbs",
    name == "Litter" ~ "plant.litter_binned.perc",
    name == "Violets" ~ "prairie.violets_binned.perc",
    name == "Woody" ~ "woody.plants_binned.perc",
    name == "WSG" ~ "warm.season.grass_binned.perc",
    T ~ name)) %>% 
  tidyr::pivot_wider(names_from = name, values_from = value)

# Check structure
dplyr::glimpse(veg_v2)

# Re-attach to broader daubenmire data
daub_v5 <- daub_v4 %>% 
  dplyr::select(-WSG:-Litter, -Seed_mix) %>% 
  dplyr::left_join(y = veg_v2, by = c("row_id", "Pasture_Patch_Year_Transect"))

# Re-check structure
dplyr::glimpse(daub_v5)

##  ------------------------------------------  ##      
# Summarizing ----
##  ------------------------------------------  ##      

# Summarize within patch (i.e., across quadrats from 2 transects / patch)
daub_v6 <- daub_v5 %>% 
  dplyr::select(-row_id, -Patch, -Pasture_Patch_Year,
                -Pasture_Patch_Year_Transect, -Angle_of_O) %>% 
  dplyr::rename(year = Year,
                pasture = Pasture,
                patch = Pasture_Patch,
                litter.depth_cm = Litter_dep) %>% 
  tidyr::pivot_longer(cols = robel.north_dm:seedmix.forbs_binned.perc.of.all.forbs) %>% 
  dplyr::group_by(year, pasture, patch, name) %>% 
  dplyr::summarize(value = mean(value, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider()

# Re-check structure
dplyr::glimpse(daub_v6)

# Clarify the panic grass column & reorder remaining columns
daub_v7 <- daub_v6 %>% 
  dplyr::mutate(panic.grass_pres.abs = dplyr::case_when(
    Panic > 0 ~ 1,
    is.na(Panic) ~ NA, 
    T ~ Panic)) %>% 
  dplyr::select(-Panic) %>% 
  dplyr::relocate(dplyr::starts_with("robel."), .after = patch) %>% 
  dplyr::relocate(dplyr::contains(".robel"), .after = robel.west_dm) %>% 
  dplyr::relocate(panic.grass_pres.abs, .after = std.dev.robel_dm) %>% 
  dplyr::relocate(dplyr::starts_with("bare.ground"), dplyr::starts_with("plant.litter"),
                  dplyr::contains("season.grass"), dplyr::starts_with("fescue"),
                  dplyr::starts_with("sedges"), dplyr::starts_with("woody.plants"),
                  dplyr::starts_with("forbs"), dplyr::starts_with("seedmix"),
                  dplyr::starts_with("prairie.violets"), dplyr::starts_with("legumes"),
                  .after = panic.grass_pres.abs) %>% 
  dplyr::relocate(litter.depth_cm, .after = std.dev.robel_dm)
  
# Re-check structure
dplyr::glimpse(daub_v7)

# Export tidy data
write.csv(x = daub_v7, row.names = F, na = '',
          file = file.path("data", "tidy-data", "daubenmire-project_tidy-plants.csv"))

# End ----
