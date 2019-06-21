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
# 6. Load IO-Leontief, modify it and save it to the database

##################################################################
### 1. Load packages
##################################################################

# required packages
# install.packages(c("DBI", "RPostgreSQL", "countrycode", "tidyverse"))

library(tidyverse)

##################################################################
### 2. Setup environment variables
##################################################################

# save the folder path
# folder_path <- "/mnt/nfs_fineprint/tmp/fabio/"
folder_path <- "db/data/" # temporarily using the data folder

## FILE TYPE
file_type <- "rds"
# for convenience, the function used to read your file_type is set up here
read_file_function <- readRDS

# check in case the file_type is changed and the read_file_function is not
if(file_type != "rds" &&
   identical(read_file_function, readRDS)){
  warning("Your file type is NOT .rds yet the read_file_function() is set to readRDS(), this will lead to unexpected errors when trying to read files.")
}

## FILE FORMAT
# %i ... 4 digit year
# %s ... character that is either ...
#
#        - E           (info about items, country, landuse and biomass numbers)
#         Country.Code, Country, Item.Code, Item, Com.Code, Group, Landuse, Biomass, ID
#     Country.Code Country Item.Code                            Item Com.Code              Group Landuse Biomass     ID
# 1              1 Armenia      2805        Rice (Milled Equivalent)     c001      Primary crops       0       0 1_2805
# 2              1 Armenia      2511              Wheat and products     c002      Primary crops   99626  311558 1_2511
# 
#        - L_mass      (leontief inverse of IO table, mass-based allocation)
#        - L_price     (leontief inverse of IO table, price-based allocation)
#        - X           ()
#        - Y           (final demand)
#        - Z_mass      ()
#        - Z_price     ()
#        - hybrid/...B ()

file_format <- paste0(folder_path, "%i_%s.", file_type) # e.g. /mnt/nfs_fineprint/tmp/fabio/2013_X.rds
# adds an additional argument to the format to allow to state a subfolder first
file_format_subfolder <- paste0(folder_path, "%s/%i_%s.", file_type) 

## DIFFERENT FILE NAMES
allocation_type <- c("mass", "price") # this is needed to read files in our case

file_names <- data.frame(
  io_leontief = c(paste0("L_", allocation_type)),
  final_demand = "Y" #,
)

## Other crucial information
year_range <- c(1986:2013)

##################################################################
### 3. Create the database connection
##################################################################

# connect to the database
db <-DBI::dbConnect(drv = RPostgres::Postgres(),
                    host = Sys.getenv("db_host"),
                    port = Sys.getenv("db_port"),
                    dbname = Sys.getenv("db_name"),
                    user = Sys.getenv("db_user"),
                    password = Sys.getenv("db_password"))

# disconnect AFTER all database operations are done
# DBI::dbDisconnect(db)

##########################################################################
### 4. E.rds - add general info about product, region, landuse and biomass
##########################################################################

year <- 2013
data <- read_file_function(sprintf(file_format, year, "E"))

# region table --------------------------------------------------
insert_data <- data.frame(
  name = levels(data$Country),
  iso3 = countrycode::countrycode(sourcevar = levels(data$Country),
                                  origin = "country.name",
                                  destination = "iso3c") # get the iso3 code for the country
)

DBI::dbAppendTable(db, name = "region", value = insert_data)

rm(insert_data)

region <- RPostgres::dbReadTable(db, "region")

# NOTE: this would also be one potential place to populate
#       region_aggregate or region_cluster

# product_group table --------------------------------------------------
insert_data <- data.frame(
  name = levels(data$Group)
)

DBI::dbAppendTable(db, name = "product_group", value = insert_data)

rm(insert_data)

product_group <- RPostgres::dbReadTable(db, "product_group")

# product_unit table --------------------------------------------------
insert_data <- data.frame(
  name = c("tonnes", "heads") # head for lifestock, tonnes for the rest
)

number_product_units <- length(insert_data$name)

DBI::dbAppendTable(db, name = "product_unit", value = insert_data)

rm(insert_data)

product_unit <- RPostgres::dbReadTable(db, "product_unit")

# product table --------------------------------------------------
insert_data <- data[!duplicated(data$Item),] %>% # get all products that are not duplicated
  dplyr::rename("name" = "Item", "product_group" = "Group", "other_id" = "Com.Code") %>% # or Item.Code instead of Com.Code
  dplyr::select(name, product_group, other_id) %>% 
  # if product_group contains "Lifestock", we use unit-id 2 for "heads", otherwise "tonnes"
  dplyr::mutate(product_unit = ifelse(grepl("Livestock", as.character(product_group)), 2, 1)) %>% # TODO: check with Martin, if only Livestock or also Livestock products are counted in head
  # we can access the group id via as.numeric as we passed on the levels of the factor to the database
  dplyr::mutate(product_group = as.numeric(product_group)) %>% 
  dplyr::arrange(name) # arrange by factor "name"

DBI::dbAppendTable(db, name = "product", value = insert_data)

rm(insert_data)

product <- RPostgres::dbReadTable(db, "product")

# env_factor_unit table --------------------------------------------------
insert_data <- data.frame(
  name = c("ha", "tonnes") # ha for landuse, tonnes for biomass
)

DBI::dbAppendTable(db, name = "env_factor_unit", value = insert_data)

rm(insert_data)

env_factor_unit <- RPostgres::dbReadTable(db, "env_factor_unit")

# env_factor table --------------------------------------------------
insert_data <- data.frame(
  name = c("landuse", "biomass"),
  env_factor_unit = env_factor_unit$id
)

DBI::dbAppendTable(db, name = "env_factor", value = insert_data)

rm(insert_data)

env_factor <- RPostgres::dbReadTable(db, "env_factor")

##########################################################################
### 5. E.rds - add environmental use (landuse and biomass)
##########################################################################

# for(year in year_range){
year <- 2013 # temporarily just 2013
data <- read_file_function(sprintf(file_format, year, "E"))

# get environmental use variables - make sure they match the format used in your data
env_factor$name_data <- paste0(toupper(substr(env_factor$name, 1, 1)), 
                               substr(env_factor$name, 2, nchar(env_factor$name)))

# prepare environmental data -> unselect unnecessary cols, add ids for region and product
env_data <- data %>%
  dplyr::select(-Country.Code, -Item.Code, -Com.Code, -Group, -ID) %>% 
  # REGION: join table, add ID, remove unnecessary cols
  dplyr::left_join(region, by = c("Country" = "name"), suffix = c("", ".region")) %>% 
  dplyr::rename("from_region" = "id") %>%  
  dplyr::select(-Country, -iso3, -geometry) %>% 
  # PRODUCT: join table, add ID, remove unnecessary cols
  dplyr::left_join(product, by = c("Item" = "name"), suffix = c("", ".product")) %>% 
  dplyr::rename("from_product" = "id") %>% 
  dplyr::select(-Item, -product_unit, -product_group, -other_id) %>% 
  # add the year
  dplyr::mutate(year = year)

# loop through env_use_vars
for(i in length(row.names(env_factor))){
  insert_data <- env_data %>%  
    dplyr::mutate(env_factor = env_factor[i,]$id) %>% 
    dplyr::rename("amount" = env_factor[i,]$name_data) %>%
    dplyr::select(from_region, from_product, env_factor, year, amount)
  
  # append env_use with the amount for the environmental factor currently in loop
  DBI::dbAppendTable(db, name = "env_use", value = insert_data)
}

rm(env_data)
rm(insert_data)

env_use <- RPostgres::dbReadTable(db, "env_use")

# }

##################################################################
### 6. Load IO-Leontief, modify it and save it to the database
##################################################################

# for(year in year_range){
year <- 2013 # temporarily just 2013

# TODO: second loop for different allocation types
data <- read_file_function(sprintf(file_format, year, file_names$io_leontief[1]))

# }


