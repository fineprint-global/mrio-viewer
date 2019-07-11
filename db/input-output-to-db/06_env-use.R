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
  
  # TODO: ha/tonnes, tonnes/tonnes --> X.rds durch total prod of this year
  insert_data <- data.frame(
    name = c("ha", "tonnes") # ha for landuse, tonnes for biomass
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

# ----------------------------------------------------------------
# get years ------------------------------------------------------
# ----------------------------------------------------------------

# Check for which years we already have data for
# care: in case you stopped an operation to the db or changed the original data
# you should remove the data from the db before any other operations.
year_range <- year_range_orig
# get all years from the db
query <- sprintf('SELECT DISTINCT year FROM "%s";', 
                 "env_intensity")
result <- RPostgres::dbGetQuery(db, query)$year
# get all years that are NOT in the db
year_range <- year_range[!(year_range %in% result)]

rm(query, result)

# ----------------------------------------------------------------
# get data and insert --------------------------------------------
# ----------------------------------------------------------------

start <- Sys.time()

for(year in year_range){
  # year <- 2013 # temporarily just 2013
  
  print(paste("Year", year, "/", year_range[length(year_range)]))
  
  data <- read_file_function(sprintf(file_format, year, "E"))

  # get environmental use variables - make sure they match the format used in your data
  env_factor$name_data <- paste0(toupper(substr(env_factor$name, 1, 1)),
                                 substr(env_factor$name, 2, nchar(env_factor$name)))

  # prepare environmental data -> unselect unnecessary cols, add ids for region and product
  env_data <- data %>%
    # filter so we only keep variables 
    dplyr::filter(abs(round(Landuse, digits = 2)) >= 0.01 |
                  abs(round(Biomass, digits = 2)) >= 0.01) %>%
    dplyr::select(-Country.Code, -Item.Code, -Com.Code, -Group, -ID) %>%
    # REGION: join table, add ID, remove unnecessary cols
    dplyr::left_join(region, by = c("Country" = "name"), suffix = c("", ".region")) %>%
    dplyr::rename("from_region" = "id") %>%
    dplyr::select(-Country, -iso3) %>%
    # PRODUCT: join table, add ID, remove unnecessary cols
    dplyr::left_join(product, by = c("Item" = "name"), suffix = c("", ".product")) %>%
    dplyr::rename("from_product" = "id") %>%
    dplyr::select(-Item, -product_unit, -product_group, -other_id) %>%
    # add the year
    dplyr::mutate(year = year)

  rm(data)
  
  # get total production data
  tp_data <- read_file_function(sprintf(file_format, year, file_names$X[1]))
  # the order of products and countries is here:
  # country 1 with total production for products from 1:130 (1 to 130)
  # country 2 with total production for products from 1:130 (1.1 to 130.1)
  # ...
  # country 192 with total production for products from 1:130 (1.191 to 130.191)
  
  total_production <- tp_data %>% 
    enframe(name = NULL) %>% # "product.country"
    #  as_tibble() %>% 
    # dplyr::mutate(year = year) %>%
    dplyr::mutate(from_region = rep(region$id, each = 130)) %>% # each for 130 products, 4 elem * 192 countries
    dplyr::mutate(from_product = rep(product$id, times = 192)) # 4 elements * 192 countries * 192 countries
  
  rm(tp_data)
  
  data <- env_data %>% 
    dplyr::left_join(total_production, by = c("from_region" = "from_region", 
                                              "from_product" = "from_product")) %>% 
    dplyr::mutate(total_production = value)
  
  rm(total_production)
  
  # loop through env_intensity_vars, for us this is landuse and biomass
  for(i in length(row.names(env_factor))){
    insert_data <- data %>%
      dplyr::mutate(env_factor = env_factor[i,]$id) %>%
      dplyr::rename("amount" = env_factor[i,]$name_data) %>%
      # now we create amount as an environmental pressure by dividing it by total_production
      dplyr::mutate(amount = ifelse(total_production == 0, 0, amount/total_production)) %>% 
      dplyr::select(from_region, from_product, env_factor, year, amount)

    # append env_intensity with the amount for the environmental factor currently in loop
    RPostgres::dbAppendTable(db, name = "env_intensity", value = insert_data)
  }

  rm(data, env_data, insert_data)
}

print("5. Populating env_intensity took")
print(Sys.time()-start)
# Time difference of 28.80357 mins

# env_intensity <- RPostgres::dbReadTable(db, "env_intensity")
