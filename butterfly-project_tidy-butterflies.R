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
librarian::shelf(tidyverse)

# Clear environment & collect garbage
rm(list = ls()); gc()

##  ------------------------------------------  ##      
# Wrangling ----
##  ------------------------------------------  ##      

# Read in data
bf_v0 <- read.csv(file = file.path("data", "raw-data", "butterfly-project_raw-butterflies.csv"))

# Check structure
dplyr::glimpse(bf_v0)










# Export tidy data
# write.csv(x = bf_vxx, row.names = F, na = '',
#           file = file.path("data", "tidy-data", "butterfly-project_tidy-butterflies.csv"))

# End ----

