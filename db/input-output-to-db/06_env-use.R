##########################################################################
### 6. E.rds - add environmental use (landuse and biomass)
##########################################################################

print("06_env-use.R")

# ----------------------------------------------------------------
# preparation ----------------------------------------------------
# ----------------------------------------------------------------

# env_factor_unit table --------------------------------------------------
env_factor_unit <- RPostgres::dbReadTable(db, "env_factor_unit")

if(nrow(env_factor_unit) == 0){
  
  # X.rds durch total prod of this year
  insert_data <- data.frame(
    # ha for landuse, tonnes for biomass
    # we divide by product unit because this is how we store the information
    # we divide the total landuse or biomass by the total production
    name = c("ha/product unit", "tonnes/product unit")
  )
  
  DBI::dbAppendTable(db, name = "env_factor_unit", value = insert_data)
  
  rm(insert_data)
  
  env_factor_unit <- RPostgres::dbReadTable(db, "env_factor_unit")
}

# env_factor table --------------------------------------------------
env_factor <- RPostgres::dbReadTable(db, "env_factor")

if(nrow(env_factor) == 0){
  
  insert_data <- data.frame(
    name = c("landuse", "biomass"),
    env_factor_unit = env_factor_unit$id
  )
  
  DBI::dbAppendTable(db, name = "env_factor", value = insert_data)
  
  rm(insert_data)
  
  env_factor <- RPostgres::dbReadTable(db, "env_factor")
}

# get tables required for these operations
product <- product_fabio
region <- region_fabio

# ------------------------------------------------------------------------------
# get years that are not yet done ----------------------------------------------
# ------------------------------------------------------------------------------

# Check for which years we already have data for
# care: in case you stopped an operation to the db or changed the original data
# you should remove the data from the db before any other operations.
year_range_landuse <- year_range_orig
# get all years from the db
query <- sprintf('SELECT DISTINCT year FROM "%s" WHERE env_factor = %.0f;', 
                 "env_intensity", env_factor$id[env_factor$name == "landuse"])
result <- RPostgres::dbGetQuery(db, query)$year
# get all years that are NOT in the db
year_range_landuse <- year_range_landuse[!(year_range_landuse %in% result)]

rm(query, result)

year_range_biomass <- year_range_orig
# get all years from the db
query <- sprintf('SELECT DISTINCT year FROM "%s" WHERE env_factor = %.0f;', 
                 "env_intensity", env_factor$id[env_factor$name == "biomass"])
result <- RPostgres::dbGetQuery(db, query)$year
# get all years that are NOT in the db
year_range_biomass <- year_range_biomass[!(year_range_biomass %in% result)]

rm(query, result)

# combine both to only collect data once
year_range <- c(year_range_biomass, year_range_landuse[!(year_range_landuse %in% year_range_biomass)])

# ------------------------------------------------------------------------------
# get data and insert ----------------------------------------------------------
# ------------------------------------------------------------------------------

start <- Sys.time()

data_E <- read_file_function(sprintf(file_format_noyear, file_names$E[1]))
data_X <- read_file_function(sprintf(file_format_noyear, file_names$X[1]))

for(year in year_range){
  print(paste("Year", year, "/", year_range[length(year_range)]))
  
  # environmental impact data --------------------------------------------------
  
  env_data <- data_E[[as.character(year)]] %>% 
    # join product
    dplyr::left_join(product[,c("other_id","id")], by = c("comm_code" = "other_id")) %>% 
    dplyr::select(-comm_code, -item, -item_code, -comm_group, -group) %>% 
    dplyr::rename(from_product = id) %>% 
    # join region
    dplyr::left_join(region[,c("id", "name")], by = c("area" = "name")) %>% 
    dplyr::select(-area, -area_code) %>% 
    dplyr::rename(from_region = id) %>% 
    # filter so we only keep variables 
    dplyr::filter(abs(round(landuse, digits = 2)) >= 0.01 |
                  abs(round(biomass, digits = 2)) >= 0.01) %>%
    # add the year
    dplyr::mutate(year = year)
  
  # total production data ------------------------------------------------------
  
  # get total production data
  tp_data <- data_X[,as.character(year)]
  # the order of products and countries is here:
  # country 1 with total production for products from 1:125 (1 to 125)
  # country 2 with total production for products from 1:125 (1.1 to 125.1)
  # ...
  # country 192 with total production for products from 1:125 (1.191 to 125.191)
  
  total_production <- tp_data %>% 
    enframe(name = NULL) %>% # "product.country"
    #  as_tibble() %>% 
    # dplyr::mutate(year = year) %>%
    dplyr::mutate(from_region = rep(region$id, each = n_product_fabio)) %>% # each for 125 products, 4 elem * 192 countries
    dplyr::mutate(from_product = rep(product$id, times = n_region_fabio)) # 4 elements * 192 countries * 192 countries
  
  rm(tp_data)
  
  # combine them ---------------------------------------------------------------
  data <- env_data %>% 
    dplyr::left_join(total_production, by = c("from_region" = "from_region", 
                                              "from_product" = "from_product")) %>% 
    dplyr::rename(total_production = value)
  
  rm(total_production, env_data)
  
  if(year %in% year_range_biomass){
    insert_data <- data %>%
      dplyr::mutate(env_factor = env_factor$id[env_factor$name == "biomass"]) %>%
      dplyr::rename("amount" = "biomass") %>%
      dplyr::filter(abs(round(amount, digits = 2)) >= 0.01) %>% 
      # now we create amount as an environmental pressure by dividing it by total_production
      dplyr::mutate(amount = ifelse(total_production == 0, 0, amount/total_production)) %>% 
      dplyr::select(from_region, from_product, env_factor, year, amount) %>% 
      as.data.frame()
    
    # append env_intensity with the amount for the environmental factor currently in loop
    # RPostgres::dbAppendTable(db, name = "env_intensity", value = insert_data)
    RPostgres::dbWriteTable(db, name = "env_intensity", value = insert_data, append = TRUE)
  }
  if(year %in% year_range_landuse){
    insert_data <- data %>%
      dplyr::mutate(env_factor = env_factor$id[env_factor$name == "landuse"]) %>%
      dplyr::rename("amount" = "landuse") %>%
      dplyr::filter(abs(round(amount, digits = 2)) >= 0.01) %>% 
      # now we create amount as an environmental pressure by dividing it by total_production
      dplyr::mutate(amount = ifelse(total_production == 0, 0, amount/total_production)) %>% 
      dplyr::select(from_region, from_product, env_factor, year, amount) %>% 
      as.data.frame()
    
    # append env_intensity with the amount for the environmental factor currently in loop
    # RPostgres::dbAppendTable(db, name = "env_intensity", value = insert_data)
    RPostgres::dbWriteTable(db, name = "env_intensity", value = insert_data, append = TRUE)
  }

  rm(data, insert_data)
}

print("6. Populating env_intensity took")
print(Sys.time()-start)
# Time difference of 1.938291 mins

# env_intensity <- RPostgres::dbReadTable(db, "env_intensity")
