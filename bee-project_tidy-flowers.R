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

##  ------------------------------------------  ##      
# 2018 Standardization ----
##  ------------------------------------------  ##      

# Read in data
flr.18_v0 <- read.csv(file = file.path("data", "raw-data", "bee-project_raw-flowers_2018.csv"))

# Check structure
dplyr::glimpse(flr.18_v0)


##  ------------------------------------------  ##      
# Combine Years ----
##  ------------------------------------------  ##      


# End ----
