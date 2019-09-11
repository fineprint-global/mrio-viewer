### main.R
# This file is used to transform an input-output table to fit the
# database format defined in the database model.
#
# Here, *FABIO* is used, so you will have to adjust your code to 
# fit your own input-output table.

# Note: per year 01-09 take around 8 hours to be saved into the db.

## STRUCTURE
# 1. Load packages
# 2. Setup environment variables
# 3. Create the database connection
# 4. E.rds - add general info about product, region
# 5. E.rds - add environmental use (landuse and biomass)
# 6. Y.rds - add final demand
# 7. L.rds - add input-output leontief
# 8. L.rds - add input-output leontief
# 9. B_inv.rds - add input-output leontief for hybrid
# 10. Final demand from EXIOBASE

# setwd("db/input-output-to-db")

# getting the starting time
script_start <- Sys.time()
print(paste("Starting main.R at", script_start))

################################################################################
### 01-03_setup.r
################################################################################

# 1. Load packages
# 2. Setup environment variables
# 3. Create the database connection
source("01-03_setup.R")

################################################################################
### 4. E.rds - add product info
################################################################################

source("04_product-info.R")

################################################################################
### 5. Add region info
################################################################################

source("05_region-info.R")

################################################################################
### 6. E.rds - add environmental use (landuse and biomass)
################################################################################

source("06_env-use.R")

################################################################################
### 7. Y.rds - add final demand
################################################################################

source("07_final-demand_fabio.R")

################################################################################
### 8. Final demand from EXIOBASE
################################################################################

source("08_final-demand_exio.R")

################################################################################
### 9. L.rds - add input-output leontief
################################################################################

source("09_input-output_fabio.R")

################################################################################
### 10. B_inv.rds - add input-output leontief for hybrid
################################################################################

source("10_input-output_exio.R")


# ------------------------------------------------------------------------------
# run vacuum analyze -----------------------------------------------------------
# ------------------------------------------------------------------------------
# we save the sendQuery into a variable to be able to clear the result

start <- Sys.time()

# we don't run the VACUUM on region because geometry (indexed) is empty for now
# DBI::dbSendQuery(db, statement = 'VACUUM ANALYZE "region";')
v <- RPostgres::dbSendQuery(db, statement = 'VACUUM ANALYZE "env_intensity";')
RPostgres::dbClearResult(v)
v <- RPostgres::dbSendQuery(db, statement = 'VACUUM ANALYZE "input-output_leontief";')
RPostgres::dbClearResult(v)
v <- RPostgres::dbSendQuery(db, statement = 'VACUUM ANALYZE "final_demand";')
RPostgres::dbClearResult(v)

print("VACUUM ANALYSE took")
print(Sys.time()-start)

# finally, disconnect the DB
DBI::dbDisconnect(db)

# print current date and time
script_end <- Sys.time()
print(paste("Finished main.R at", script_end))

# print time difference between script start and end
print(script_end-script_start)