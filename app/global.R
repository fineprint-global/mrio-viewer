### global.R
# This file is loaded on startup of the app.
# Here, code that only needs to be performed once is executed.
# Typically, this includes defining libraries,
# setting up the database connection, and
# defining variables connected to database operation.

## STRUCTURE
# 1. Load packages
# 2. Data setup

print("global.R")
start <- Sys.time()

##################################################################
### 1. Load packages
##################################################################

library(shiny)
library(tidyverse)
library(pool)
library(RColorBrewer)

##################################################################
### 2. Data setup
##################################################################

# connect PostGIS database via a pool object for better Shiny behaviour --------
pool <- pool::dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  host = Sys.getenv("db_host"), # for usage inside docker: ioviz_db; outside the docker-environment: use proper server ip and port,
  port = Sys.getenv("db_port"),
  dbname = Sys.getenv("db_name"),
  user = Sys.getenv("db_user"),
  password = Sys.getenv("db_password"),
  minSize = 1,
  maxSize = Inf,
  idleTimeout = 300, # 5 minutes,
  validationInterval = 120
)
onStop(function() { # this is required to close the pool so we have no leaking connections
  pool::poolClose(pool)
  message("Pool closed.")
})

# define all tables
name_fabio <- "FABIO"
name_exio <- "EXIOBASE"
modes <- c("origin", "destination")

# Region -----------------------------------------------------------------------
# here, we create region_fabio and _exio to be able to 
# display the FABIO regions to the user in the select and use the 
# EXIOBASE regions afterwards in the visualization
region_cluster_tbl <- dplyr::tbl(pool, "region_cluster")
region_cluster_region_tbl <- dplyr::tbl(pool, "region_cluster_region")
region_aggregate <- dplyr::tbl(pool, "region_aggregate")

# make region distiction between FABIO and EXIOBASE
region_tbl <- dplyr::tbl(pool, "region") %>% 
  dplyr::full_join(region_cluster_region_tbl, by = c("id" = "id_region")) %>% 
  dplyr::left_join(region_cluster_tbl, 
                   by = c("id_region_cluster" = "id"), 
                   suffix = c("",".cluster")) %>% 
  dplyr::filter(name.cluster %in% c(name_fabio, name_exio)) %>%
  dplyr::select(-geometry, -id_region_cluster)

region_continent <- dplyr::tbl(pool, "region") %>% 
  dplyr::full_join(region_cluster_region_tbl, by = c("id" = "id_region")) %>% 
  dplyr::full_join(region_cluster_tbl, 
                   by = c("id_region_cluster" = "id"), 
                   suffix = c("",".cluster")) %>% 
  dplyr::filter(!(name.cluster %in% c(name_fabio, name_exio))) %>%
  dplyr::select(-geometry) %>% 
  dplyr::collect()

continents <- region_continent %>% 
  dplyr::select(name.cluster) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(name.cluster) %>% 
  dplyr::collect()

region_conc <- region_tbl %>% dplyr::collect()

region_fabio <- region_tbl %>% 
  dplyr::filter(name.cluster == name_fabio) %>% 
  dplyr::select(-name.cluster) %>% 
  dplyr::left_join(region_aggregate, by = c("id" = "region_in_aggregate")) %>% 
  dplyr::rename(exio_id = region_aggregate) %>% 
  dplyr::collect()

region_exio <- region_tbl %>% 
  dplyr::filter(name.cluster == name_exio) %>% 
  dplyr::select(-name.cluster) %>% 
  dplyr::collect()

region_aggregated <- region_fabio %>% 
  dplyr::full_join(region_exio, by = c("exio_id" = "id"), suffix = c("", "_exio")) %>% 
  dplyr::rename("id_exio" = "exio_id") %>% 
  dplyr::select(-iso3_exio)

# Product ----------------------------------------------------------------------
# here, we create product_fabio and _exio to be able to 
# display the FABIO products to the user in the select and use the 
# EXIOBASE products afterwards in the visualization

product_conc <- dplyr::tbl(pool, "product") %>% dplyr::collect()

# create two new product IDs representing food and nonfood
# this will be used for aggregating very small product flows
product_other <- list(
  food = max(product_conc$id)+1L,
  nonfood = max(product_conc$id)+2L
)

# get first 130 products (FABIO products) and add "Food (aggregate)"
product_fabio <- product_conc %>% 
  dplyr::slice(1:130) %>% 
  dplyr::add_row(id = product_other$food, name = "Food (aggregate)")
# get the rest of the products (EXIO products) and add "Nonfood (aggregate)"
product_exio <- product_conc %>% 
  dplyr::slice(131:nrow(product_conc)) %>% 
  dplyr::add_row(id = product_other$nonfood, name = "Nonfood (aggregate)")
# now combine the new product_fabio and _exio to get the updated product_conc
product_conc <- rbind(product_fabio, product_exio)

product_group_conc <- dplyr::tbl(pool, "product_group") %>% dplyr::collect()
product_unit_conc <- dplyr::tbl(pool, "product_unit") %>% dplyr::collect()

## create list for input-dropdown
product_dropdown <- list()
# group this list by product group
for(product_group in product_group_conc$id){
  product_group_name <- product_group_conc$name[product_group_conc$id == product_group]
  
  product_dropdown[product_group_name] <- list(
    # we use [1:130,] to slice away the "Food (aggregate)"-product type
    # that is only used for aggregating, not filtering
    product_fabio[1:130,]$name[product_fabio[1:130,]$product_group == product_group] %>% 
      base::sort() # sort alphabetically within groups
  )
}

# separate dropdown for second mode (destination)
product_destination_dropdown <- product_dropdown
product_destination_dropdown["Non-food"] <- list(product_exio$name %>% base::sort())

# env_intensity ----------------------------------------------------------------
env_intensity_tbl <- dplyr::tbl(pool, "env_intensity")
env_intensity_calculated_tbl <- dplyr::tbl(pool, "env_intensity_calculated")
env_factor_conc <- dplyr::tbl(pool, "env_factor") %>% 
  dplyr::collect() %>% 
  # change the name of landuse and biomass
  dplyr::mutate(name = if_else(name == "landuse", 
                               "cropland", 
                               if_else(name == "biomass", 
                                       "primary biomass", 
                                       name)))
env_factor_unit_conc <- dplyr::tbl(pool, "env_factor_unit") %>% dplyr::collect()

# IO-leontief ------------------------------------------------------------------
io_leontief_tbl <- dplyr::tbl(pool, "input-output_leontief")
allocation_conc <- dplyr::tbl(pool, "allocation") %>% dplyr::collect()

# get max and min year from the IO-leontief to display to the user
query <- 'SELECT MAX(year), MIN(year) FROM "input-output_leontief";'
year_max_min <- pool::dbGetQuery(pool, query)

# final demand -----------------------------------------------------------------
final_demand_tbl <- dplyr::tbl(pool, "final_demand")
element_conc <- dplyr::tbl(pool, "element") %>% dplyr::collect()
type_conc <- dplyr::tbl(pool, "type") %>% dplyr::collect()

element_type <- element_conc %>% dplyr::left_join(type_conc, by = c("type" = "id"), suffix = c("", ".type"))

print("global.R took ...")
print(Sys.time()-start)

# allow bookmarking (necessary to allow saving the state of the app via URL)
enableBookmarking(store = "url")
