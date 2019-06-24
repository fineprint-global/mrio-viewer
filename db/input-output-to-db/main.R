### main.R
# This file is used to transform an input-output table to fit the
# database format defined in the database model.
#
# Here, *FABIO* is used, so you will have to adjust your code to 
# fit your own input-output table.

## STRUCTURE
# 1. Load packages
# 2. Setup environment variables
# 3. Create the database connection
# 4. E.rds - add general info about product, region
# 5. E.rds - add environmental use (landuse and biomass)
# 6. Y.rds - add final demand
# 7. Load IO-Leontief, modify it and save it to the database

setwd("db/input-output-to-db")

##########################################################################
### 01-03_setup.r
##########################################################################

# 1. Load packages
# 2. Setup environment variables
# 3. Create the database connection
source("01-03_setup.R")

##########################################################################
### 4. E.rds - add general info about product, region, landuse and biomass
##########################################################################

source("04_general-info.R")

##########################################################################
### 5. E.rds - add environmental use (landuse and biomass)
##########################################################################

source("05_env-use.R")

##################################################################
### 6. Y.rds - add final demand
##################################################################

source("06_final-demand.R")

##################################################################
### 7. Load IO-Leontief, modify it and save it to the database
##################################################################

# for(year in year_range){
year <- 2013 # temporarily just 2013

# TODO: second loop for different allocation types
data <- read_file_function(sprintf(file_format, year, file_names$io_leontief[1]))

# }


