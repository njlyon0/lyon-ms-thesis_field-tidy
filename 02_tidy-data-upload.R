##  ------------------------------------------------------------  ##
                  # All Projects - Upload Data
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Upload raw data from Google Drive folder

##  ------------------------------------------  ##      
                # Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Clear environment & collect garbage
rm(list = ls()); gc()

# Create needed folder(s)
dir.create(path = file.path("data", "tidy-data"), showWarnings = F)

# Identify relevant Drive folder
tidy_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1L9GPPA3M6LcbWeGF-aVTLlom9TxszwY7")

##  ------------------------------------------  ##      
                # Upload Data ----
##  ------------------------------------------  ##      

# List local files
tidy_data <- dir(file.path("data", "tidy-data"))

# Upload them
purrr::walk(.x = tidy_data,
            .f = ~ googledrive::drive_upload(media = file.path("data", "tidy-data", .x),
                                             overwrite = T, path = tidy_url))

# End ----
