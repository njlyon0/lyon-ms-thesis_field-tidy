##  ------------------------------------------------------------  ##
                  # Bee Project - Bee Tidy 2017-2018
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Wrangle 2017 and 2018 data on native bees in southern Iowa/northern Missouri

##  ------------------------------------------  ##      
                # Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, vegan)

# Clear environment & collect garbage
rm(list = ls()); gc()

##  ------------------------------------------  ##      
# 2017 Standardization ----
##  ------------------------------------------  ##      

# Read in data
bz.17_v0 <- read.csv(file = file.path("data", "raw-data", "bee-project_raw-bees_2017.csv"))

# Check structure
dplyr::glimpse(bz.17_v0)

# Simplify/standardize column names
bz.17_v1 <- bz.17_v0 %>% 
  # Drop unwanted rows
  dplyr::filter(is.na(Capture.Year) != T) %>% 
  # Generate new needed columns
  ## Quantitative 'height'
  dplyr::mutate(height_cm = ifelse(test = (Height == "Low"),
                                   yes = 2.5, no = 100),
                .after = Height) %>% 
  ## Better 'species' column
  dplyr::mutate(species = paste0(Genus, ".", Species),
                .after = Species) %>% 
  ## Quantitative 'bowls recovered'
  dplyr::mutate(bowls.recovered_percent = (Bowls.Recovered / 6) * 100,
                .after = Bowls.Recovered) %>% 
  # Drop (some) old columns
  dplyr::select(-Height, -Species, -Bowls.Recovered, -ID.Checked., -Pinned.) %>% 
  # Rename columns
  dplyr::rename(sampling.event.id = Sampling.Event.ID,
                sampling.round = Round,
                capture.year = Capture.Year,
                capture.date = Capture.Date,
                pasture = Site,
                patch = SiteCode,
                specimen.id = Specimen.ID,
                family = Family,
                genus = Genus,
                sex = Sex,
                number = Number)

# Re-check structure
dplyr::glimpse(bz.17_v1)

# Summarize within bee species
bz.17_v2 <- bz.17_v1 %>% 
  # Count total bees / species within bowls
  dplyr::group_by(capture.year, capture.date, pasture, patch, height_cm,
                  bowls.recovered_percent, family, genus, species) %>% 
  dplyr::summarize(bee.total = sum(number, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()
  
# Re-check structure
dplyr::glimpse(bz.17_v2)

# Do some QC on the remaining rows
bz.17_v3 <- bz.17_v2 %>% 
  # Remove instances where bees were not found or otherwise not recorded
  dplyr::filter(!species %in% c("X.X", ".")) %>% 
  # Remove all bowls if less than 80% of the bowls were recovered
  dplyr::filter(bowls.recovered_percent >= 80) %>% 
  # Then drop that column
  dplyr::select(-bowls.recovered_percent)

# Export this tidied data!
write.csv(x = bz.17_v3, row.names = F, na = '',
          file = file.path("data", "tidy-data", "bee-project_tidy-bees_2017.csv"))

##  ------------------------------------------  ##      
# 2018 Standardization ----
##  ------------------------------------------  ##      





# End ----
