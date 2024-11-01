##  ------------------------------------------------------------  ##
                  # All Projects - Download Data
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Download raw data from Google Drive folder

##  ------------------------------------------  ##      
                # Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "raw-data"), showWarnings = F)

# Identify relevant Drive folder
raw_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1bo5Hye4FjIu-yOiIjr7lp8B0z4v-b1cK")

##  ------------------------------------------  ##      
                # Download Data ----
##  ------------------------------------------  ##      

# Identify files that are already downloaded
already_done <- dir(file.path("data", "raw-data"))

# List files in Drive
all_data <- googledrive::drive_ls(path = raw_url)

# Identify only the needed files
needed_data <- all_data  %>% 
  ## And remove already downloaded files
  dplyr::filter(!name %in% already_done)

# Download the needed files
purrr::walk2(.x = needed_data$id, .y = needed_data$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw-data", .y)))

# End ----
