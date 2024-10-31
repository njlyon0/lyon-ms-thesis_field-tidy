##  ------------------------------------------------------------  ##
            # Butterfly Project - Butterfly Tidying
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Wrangle data on butterflies in southern Iowa/northern Missouri

##  ------------------------------------------  ##      
                # Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("data", "tidy-data"), showWarnings = F)

##  ------------------------------------------  ##      
          # Prepare Site-Level Info ----
##  ------------------------------------------  ##      

# Read in 'sites' info
## Data were originally entered into MS Access pseudo-databases so some info stored in a separate file
site_v0 <- read.csv(file = file.path("data", "raw-data", "butterfly-project_raw-sites.csv")) %>%
  # Resolve/some not truly unique rows
  dplyr::mutate(Nectar.Observer = ifelse(test = any(is.na(Nectar.Observer) == T),
                                         yes = !is.na(Nectar.Observer),
                                         no = Nectar.Observer)) %>% 
  dplyr::distinct()

# Check structure
dplyr::glimpse(site_v0)

# Fill in missing data (where findable from scanned data files)
site_v1 <- site_v0 %>% 
  dplyr::mutate(Year = dplyr::case_when(
    any(is.na(Year) | nchar(Year)) & Transect.ID >= 854 & Transect.ID <= 936 ~ 11,
    any(is.na(Year) | nchar(Year)) & Transect.ID >= 1081 & Transect.ID <= 1130 ~ 12,
    T ~ Year)) %>% 
  dplyr::mutate(Patch = dplyr::case_when(
    Transect.ID %in% c(854, 912) ~ "PAW PBG 1-1",
    Transect.ID %in% c(861, 917) ~ "PAW PBG 1-2",
    Transect.ID %in% c(855, 931) ~ "PAW PBG 2-1",
    Transect.ID %in% c(864, 934) ~ "PAW PBG 2-2",
    Transect.ID %in% c(857) ~ "PAW PBG 3-1",
    Transect.ID %in% c(863, 916) ~ "PAW PBG 3-2",
    Transect.ID %in% c(856, 935) ~ "PAW PBG 4-1",
    Transect.ID %in% c(865, 936) ~ "PAW PBG 4-2",
    Transect.ID %in% c(858) ~ "PAW PBG 5-1",
    Transect.ID %in% c(866) ~ "PAW PBG 5-2",
    Transect.ID %in% c(932) ~ "PAW PBG 2S to 2N",
    Transect.ID %in% c(933) ~ "PAW PBG 5-1 1N to 1S",
    Transect.ID %in% c(1130) ~ "KLT-S",
    T ~ Patch))

# Identify gained/lost columns
supportR::diff_check(old = names(site_v0), new = names(site_v1))

# Rename most columns so provenance is clear
site_v2 <- supportR::safe_rename(data = site_v1, bad_names = names(site_v1),
                                 good_names = paste0("site_", names(site_v1))) %>% 
  dplyr::rename(Transect.ID = site_Transect.ID)

# Check structure
dplyr::glimpse(site_v2)

##  ------------------------------------------  ##      
# Integrate Site-Level Info ----
##  ------------------------------------------  ##      

# Read in data
bf_v0 <- read.csv(file = file.path("data", "raw-data", "butterfly-project_raw-butterflies.csv"))

# Check structure
dplyr::glimpse(bf_v0)

# Attach site-level info & do macro tidying
bf_v1 <- bf_v0 %>% 
  dplyr::left_join(y = site_v2, by = "Transect.ID") %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(test = nchar(.) == 0 | . %in% c("NA", "NA-NA"),
                                              yes = NA, no = .)))

# Check structure
dplyr::glimpse(bf_v1)

##  ------------------------------------------  ##      
                  # Wrangling ----
##  ------------------------------------------  ##      

# Do column-level wrangling / streamlining
bf_v2 <- bf_v1 %>% 
  # Coalesce synonymous columns
  dplyr::mutate(
    year = dplyr::coalesce(Year, site_Year),
    pasture = dplyr::coalesce(Site, site_Site),
    patch = dplyr::coalesce(Patch, site_Patch),
    transect = dplyr::coalesce(Whittaker, site_Whittaker),
    round = dplyr::coalesce(Round, site_Round),
    date = dplyr::coalesce(Date, site_Date)
    # = dplyr::coalesce(, site_)
    ) %>% 
  # Fill in common names if only species is recorded
  dplyr::mutate(butterfly.common = dplyr::case_when(
    is.na(Butterfly.Common.Name) == T ~ Butterfly.Species,
    nchar(Butterfly.Common.Name) == 0 ~ Butterfly.Species,
    T ~ Butterfly.Common.Name)) %>% 
  # Rename some other columns
  dplyr::rename(number = Number,
                wind.speed_kph = site_Wind_kph,
                temp_deg.celsius = site_Temp_C,
                cloud.cover_perc = site_Cloud.Cover_Percent) %>% 
  # Remove unwanted columns
  dplyr::select(-Year, -Site, -Patch, -Whittaker, -Round, -Month, -Date, 
                -Butterfly.ID, -Butterfly.Species, -Butterfly.Common.Name,
                -ID.Confidence, -Activity, -Transect.Section, -Distance_m,
                -Outside_Transect, -Sex, -dplyr::starts_with("Nectar."),
                -Comments, -dplyr::starts_with("site_"), -Transect.ID) %>% 
  # Reorder
  dplyr::relocate(year:date, .before = dplyr::everything()) %>% 
  dplyr::relocate(wind.speed_kph:cloud.cover_perc, .after = date) %>% 
  dplyr::relocate(butterfly.common, .before = number) %>% 
  # Split up date
  tidyr::separate_wider_delim(cols = date, delim = ".",
                              names = c("month", "day")) %>% 
  dplyr::mutate(month = as.numeric(month),
                day = as.numeric(day)) %>% 
  # Make year more explicit
  dplyr::mutate(year = year + 2000)

# Check structure
dplyr::glimpse(bf_v2)










# Export tidy data
# write.csv(x = bf_vxx, row.names = F, na = '',
#           file = file.path("data", "tidy-data", "butterfly-project_tidy-butterflies.csv"))

# End ----

