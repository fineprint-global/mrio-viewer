##################################################################
### 1. Load packages
##################################################################

# # install required packages if you do not have them already
# install.packages(c("DBI", "RPostgreSQL", "countrycode", "tidyverse", "Matrix"))

library(tidyverse)

##################################################################
### 2. Setup environment variables
##################################################################

# save the folder path
folder_path <- "/mnt/nfs_fineprint/tmp/fabio/v2/"

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
# here we define the file format to be used in the sprintf()-function to generate the actual file name
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
allocation_type <- c("mass", "value") # this is needed to read files in our case
# there is a different allocation type for the output because the term "value" is
# preferred over "price", but the files are still saved with "price"
# v2 already uses value
allocation_type_output <- c("mass", "value")

file_names <- data.frame(
  io_leontief = c(paste0("L_", allocation_type)),
  final_demand = "Y",
  E = "E",
  X = "X",
  B = c(paste0("B_inv_", allocation_type)),
  Z = c(paste0("Z_", allocation_type)) # similar structure to L.rds
)

## other crucial information

# define the year range of data to insert into the database
year_range_orig <- c(2013:2010) # 1986

# names used for FABIO and EXIOBASE
name_fabio <- "FABIO"
name_exio <- "EXIOBASE"
# number of products FABIO and EXIOBASE
n_product_fabio <- 125
n_product_exiobase <- 200
# number of regions FABIO and EXIOBASE
n_region_fabio <- 192
n_region_exio <- 49

##################################################################
### 3. Create the database connection
##################################################################

# check if there is a valid connection already
# to close it before creating a new one
if(exists("db") && RPostgres::dbIsValid(db)){
  DBI::dbDisconnect(db)
}

# connect to the database
# Sys.getenv() gets the environment variables stored in .Renviron
db <-DBI::dbConnect(drv = RPostgres::Postgres(),
                    host = Sys.getenv("db_host"),
                    port = Sys.getenv("db_port"),
                    dbname = Sys.getenv("db_name"),
                    user = Sys.getenv("db_user"),
                    password = Sys.getenv("db_password"))

# disconnect AFTER all database operations are done
# DBI::dbDisconnect(db)
