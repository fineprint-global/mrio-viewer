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
# 7. L.rds - add input-output leontief

# setwd("db/input-output-to-db")

################################################################################
### 01-03_setup.r
################################################################################

# 1. Load packages
# 2. Setup environment variables
# 3. Create the database connection
source("01-03_setup.R")

################################################################################
### 4. E.rds - add general info about product, region, landuse and biomass
################################################################################

source("04_general-info.R")

################################################################################
### 5. Add EXIOBASE info
################################################################################

source("05_exio-info.R")

################################################################################
### 6. E.rds - add environmental use (landuse and biomass)
################################################################################

source("06_env-use.R")

################################################################################
### 7. Y.rds - add final demand
################################################################################

source("07_final-demand.R")

################################################################################
### 8. L.rds - add input-output leontief
################################################################################

source("08_input-output.R")

# finally, disconnect the DB
DBI::dbDisconnect(db)