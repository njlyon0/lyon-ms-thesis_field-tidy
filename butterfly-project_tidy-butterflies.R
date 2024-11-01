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

# Re-check structure
dplyr::glimpse(bf_v2)

# Drop unknown butterflies & placeholder rows
bf_v3 <- bf_v2 %>% 
  dplyr::filter(stringr::str_detect(string = butterfly.common, pattern = "unknown") != T) %>% 
  dplyr::filter(butterfly.common != "accidental row") %>% 
  dplyr::filter(butterfly.common != "none")
  
# Check remaining 'species'
sort(unique(bf_v3$butterfly.common))

# Re-check structure
dplyr::glimpse(bf_v3)

# Summarize within species
bf_v4 <- bf_v3 %>% 
  dplyr::group_by(year, pasture, patch, transect, month, day, butterfly.common) %>%
  dplyr::summarize(wind.speed_kph = mean(wind.speed_kph, na.rm = T),
                   temp_deg.celsius = mean(temp_deg.celsius, na.rm = T),
                   cloud.cover_perc = mean(cloud.cover_perc, na.rm = T),
                   butterfly.count = sum(number, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Re-check structure
dplyr::glimpse(bf_v4)

##  ------------------------------------------  ##      
              # Prepare Indices ----
##  ------------------------------------------  ##      

# Read in management information index
mgmt_v0 <- read.csv(file = file.path("supporting-materials", "management_history.csv"))

# Check structure
dplyr::glimpse(mgmt_v0)

# Make this more interoperable with the other data
mgmt_v1 <- mgmt_v0 %>% 
  dplyr::mutate(patch = paste0(pasture, "-", patch)) %>% 
  dplyr::mutate(transect = ifelse(test = nchar(transect) == 0,
                                  yes = NA, no = transect)) %>% 
  dplyr::mutate(transect = ifelse(test = is.na(transect) != T,
                                  yes = paste0(patch, gsub(pattern = "T", replacement = "",
                                                           x = transect)),
                                  no = transect)) %>% 
  dplyr::distinct() %>% 
  supportR::safe_rename(data = ., bad_names = names(.), 
                        good_names = gsub(pattern = "_", replacement = ".",
                                          x = names(.)))

# Re-check structure
dplyr::glimpse(mgmt_v1)

# Separate sites where patch-level is fine from those where more detail is needed
mgmt.patch <- mgmt_v1 %>%
  dplyr::filter(is.na(transect) == T) %>% 
  dplyr::select(-transect)
mgmt.trans <- mgmt_v1 %>% 
  dplyr::filter(is.na(transect) != T)

# Read in species information index
spp.faq_v0 <- read.csv(file = file.path("supporting-materials", "spp-info_butterflies.csv"))

# Check structure
dplyr::glimpse(spp.faq_v0)

# Pare down columns
spp.faq_v1 <- spp.faq_v0 %>% 
  dplyr::mutate(butterfly.common = tolower(butterfly_common),
                butterfly.family = tolower(family)) %>% 
  dplyr::select(butterfly.common, butterfly.family) %>%
  dplyr::distinct()
  
# Re-check structure
dplyr::glimpse(spp.faq_v1)

# Check for--and resolve--mismatch in butterfly common names
## Index contains entries not necessarily in data
supportR::diff_check(old = unique(bf_v4$butterfly.common),
                     new = unique(spp.faq_v1$butterfly.common))

##  ------------------------------------------  ##      
            # Integrate Indices ----
##  ------------------------------------------  ##      

# Handle site identification issues in the data
bf_v5 <- bf_v4 %>% 
  # Fill in missing sites where patch is known
  dplyr::mutate(pasture = ifelse(test = is.na(pasture),
                                 yes = stringr::str_sub(string = patch,
                                                        start = 1, end = 3),
                                 no = pasture)) %>% 
  # Do some repair for the "RCH" site
  ## Site was re-drawn between 2013 & 14 seasons
  dplyr::mutate(
    transect = dplyr::case_when(
      pasture == "RCH" & year <= 2013 ~ gsub("RCH", "RCH2007", transect), 
      pasture == "RCH" & year >= 2014 ~ gsub("RCH", "RCH2014", transect),
      transect == "RIN-C2" & year == 2015 ~ "RIN-S2",
      T ~ transect),
    patch = dplyr::case_when(
      pasture == "RCH" & year <= 2013 ~ gsub("RCH", "RCH2007", patch), 
      pasture == "RCH" & year >= 2014 ~ gsub("RCH", "RCH2014", patch),
      patch == "RIN-C" & year == 2015 ~ "RIN-S",
      T ~ patch),
    pasture = dplyr::case_when(
      pasture == "RCH" & year <= 2013 ~ "RCH2007", 
      pasture == "RCH" & year >= 2014 ~ "RCH2014",
      T ~ pasture) ) %>% 
  dplyr::mutate(
    patch = gsub(pattern = "07-N", replacement = "07-E", x = patch),
    patch = gsub(pattern = "07-S", replacement = "07-C", x = patch),
    patch = gsub(pattern = "14-Y", replacement = "14-W", x = patch),
    transect = gsub(pattern = "14-Y", replacement = "14-W", x = transect)
  )

# Check structure
dplyr::glimpse(bf_v5)

# Actually integrate indices
bf_v6 <- bf_v5 %>%
  dplyr::left_join(y = mgmt.patch, by = c("year", "pasture", "patch")) %>%
  dplyr::left_join(y = mgmt.trans, by = c("year", "pasture", "patch", "transect")) %>%
  dplyr::left_join(y = spp.faq_v1, by = "butterfly.common") %>% 
  # Coalesce duplicate columns
  dplyr::mutate(
    fire.treatment = dplyr::coalesce(fire.treatment.x, fire.treatment.y),
    years.since.fire = dplyr::coalesce(years.since.fire.x, years.since.fire.y),
    adaptive.mgmt = dplyr::coalesce(adaptive.mgmt.x, adaptive.mgmt.y),
    stocking.treatment = dplyr::coalesce(stocking.treatment.x, stocking.treatment.y),
    herbicide.treatment = dplyr::coalesce(herbicide.treatment.x, herbicide.treatment.y),
    years.since.herbicide = dplyr::coalesce(years.since.herbicide.x, years.since.herbicide.y)
  ) %>% 
  # Drop unwanted columns / re-order remaining ones
  dplyr::select(-dplyr::ends_with(c(".x", ".y"))) %>% 
  dplyr::relocate(fire.treatment:years.since.herbicide, .after = transect) %>% 
  dplyr::relocate(wind.speed_kph:cloud.cover_perc, .after = day) %>% 
  dplyr::relocate(butterfly.family, butterfly.common, butterfly.count, 
                  .after = cloud.cover_perc)

# Check structure
dplyr::glimpse(bf_v6)

##  ------------------------------------------  ##      
                  # Finalize ----
##  ------------------------------------------  ##      

# Remove unwanted / 'bad' data
bf_v7 <- bf_v6 %>% 
  dplyr::filter(stringr::str_detect(string = patch, pattern = "PAW PBG") != TRUE) %>% 
  dplyr::filter(stringr::str_detect(string = patch, pattern = "SS\\.") != TRUE) %>% 
  dplyr::filter(patch != "RCH2014-S") %>% 
  dplyr::filter(!pasture %in% c("FRN", "JER"))

# Count lost rows
nrow(bf_v6) - nrow(bf_v7)

# Re-check structure
dplyr::glimpse(bf_v7)

# Export tidy data
write.csv(x = bf_v7, row.names = F, na = '',
          file = file.path("data", "tidy-data", "butterfly-project_tidy-butterflies.csv"))

# End ----

