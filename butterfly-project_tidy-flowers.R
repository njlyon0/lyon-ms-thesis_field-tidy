##  ------------------------------------------------------------  ##
              # Butterfly Project - Flowers Tidying
##  ------------------------------------------------------------  ##
# Written by Nick J Lyon

# Script purpose:
## Wrangle data on floral resources (i.e., nectar-producing flowers) in southern Iowa/northern Missouri

##  ------------------------------------------  ##      
# Housekeeping ----
##  ------------------------------------------  ##      

# Set required libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Clear environment & collect garbage
rm(list = ls()); gc()

##  ------------------------------------------  ##      
# Wrangling ----
##  ------------------------------------------  ##      

# Read in data
flr_v0 <- read.csv(file = file.path("data", "raw-data", "butterfly-project_raw-flowers.csv"))

# Check structure
dplyr::glimpse(flr_v0)










# Export tidy data
# write.csv(x = flr_vxx, row.names = F, na = '',
#           file = file.path("data", "tidy-data", "butterfly-project_tidy-flowers.csv"))

# End ----
