##  ------------------------------------------------------------  ##
          # Bee Project - Floral Resource Tidy 2017-2018
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Wrangle 2017 and 2018 data on nectar-producing flowers in southern Iowa/northern Missouri

##  ------------------------------------------  ##      
               # Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Clear environment & collect garbage
rm(list = ls()); gc()

##  ------------------------------------------  ##      
            # 2017 Standardization ----
##  ------------------------------------------  ##      

# Read in data
flr.17_v0 <- read.csv(file = file.path("data", "raw-data", "bee-project_raw-flowers_2017.csv"))

# Check structure
dplyr::glimpse(flr.17_v0)

# Do big-picture standardizing
flr.17_v1 <- flr.17_v0 %>% 
  # Rename columns
  dplyr::rename(sampling.event.id = Sampling.Event.ID,
                sampling.round = Round,
                capture.year = Capture.Year,
                capture.date = Capture.Date,
                pasture = Site,
                patch = SiteCode,
                nectar.common = Nectar.Common.Name) %>% 
  # Drop unwanted columns
  dplyr::select(-dplyr::starts_with("sampling."), -Collector, 
                -Enterer, -Checker) %>% 
  # Make floral common names lowercase
  dplyr::mutate(nectar.common = tolower(nectar.common)) %>% 
  # Reshape to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("Section."),
                      names_to = "section.id", values_to = "flower_ct")

# Re-check structure
dplyr::glimpse(flr.17_v1)

##  ------------------------------------------  ##      
          # 2018 Standardization ----
##  ------------------------------------------  ##      

# Read in data
flr.18_v0 <- read.csv(file = file.path("data", "raw-data", "bee-project_raw-flowers_2018.csv"))

# Check structure
dplyr::glimpse(flr.18_v0)

# Do big-picture standardizing
flr.18_v1 <- flr.18_v0 %>% 
  # Rename columns
  dplyr::rename(sampling.event.id = Sampling.Event.ID,
                sampling.round = Round,
                capture.year = Capture.Year,
                capture.date = Capture.Date,
                pasture = Site,
                patch = Patch,
                nectar.common = Flower.Common.Name) %>% 
  # Drop unwanted columns
  dplyr::select(-dplyr::starts_with("sampling."), -Collector, -Enterer, 
                -Entry.Date, -Checker, -Check.Date) %>% 
  # Make floral common names lowercase
  dplyr::mutate(nectar.common = tolower(nectar.common)) %>% 
  # Reshape to long format
  tidyr::pivot_longer(cols = P1:P6, names_to = "point.id", values_to = "flower_ct")

# Re-check structure
dplyr::glimpse(flr.18_v1)

##  ------------------------------------------  ##      
# Combine Years ----
##  ------------------------------------------  ##      

# Combine into a single data object
flr.both_v1 <- dplyr::bind_rows(flr.17_v1, flr.18_v1) %>% 
  # Put capture date in a less ambiguous format
  tidyr::separate_wider_delim(cols = capture.date, delim = ".",
                              names = c("capture.month", "capture.day")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("capture."),
                              .fns = as.numeric))

# Check structure
dplyr::glimpse(flr.both_v1)

# Standardize floral names where needed
flr.both_v2 <- flr.both_v1 %>% 
  dplyr::mutate(nectar.common = dplyr::case_when(
    nectar.common %in% c("bee balm") ~ "bergamot",
    nectar.common %in% c("plantago lanceolata") ~ "ribwort plantain",
    # nectar.common %in% c() ~ "",
    T ~ nectar.common))

# Check remaining names
sort(unique(flr.both_v2$nectar.common))

# Remove section/point identification and summarize
flr.both_v3 <- flr.both_v2 %>% 
  dplyr::group_by(capture.year, capture.month, capture.day, 
                  pasture, patch, nectar.common) %>% 
  dplyr::summarize(flower_total = sum(flower_ct, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Remove any rows with 0 flowers
  dplyr::filter(flower_total > 0)

# Re-check structure
dplyr::glimpse(flr.both_v3)

# Export this tidied data!
write.csv(x = flr.both_v3, row.names = F, na = '',
          file = file.path("data", "tidy-data", "bee-project_tidy-flowers_2017-18.csv"))

# End ----
