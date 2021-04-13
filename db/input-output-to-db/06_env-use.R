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
    name = c("ha/product unit", "tonnes/product unit", "m3/product unit")
  )
  
  DBI::dbAppendTable(db, name = "env_factor_unit", value = insert_data)
  
  rm(insert_data)
  
  env_factor_unit <- RPostgres::dbReadTable(db, "env_factor_unit")
  
} else if(nrow(env_factor_unit) == 2){ # we don't have green and blue 
    
    # X.rds durch total prod of this year
    insert_data <- data.frame(
      # we divide by product unit because this is how we store the information
      name = c("m3/product unit") # for water
    )
    
    DBI::dbAppendTable(db, name = "env_factor_unit", value = insert_data)
    
    rm(insert_data)
    
    env_factor_unit <- RPostgres::dbReadTable(db, "env_factor_unit")
}

# env_factor table --------------------------------------------------
env_factor <- RPostgres::dbReadTable(db, "env_factor")

if(nrow(env_factor) == 0){
  
  insert_data <- data.frame(
    name = c("landuse", "biomass", "green", "blue"),
    env_factor_unit = c(env_factor_unit$id[1:2],env_factor_unit$id[3],env_factor_unit$id[3])
  )
  
  DBI::dbAppendTable(db, name = "env_factor", value = insert_data)
  
  rm(insert_data)
  
  env_factor <- RPostgres::dbReadTable(db, "env_factor")
} else if (nrow(env_factor) == 2){ # this indicates the "old setting" of just land and bm
  insert_data <- data.frame(
    name = c("green", "blue"),
    env_factor_unit = c(env_factor_unit$id[3],env_factor_unit$id[3])
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

year_range <- list()
year_range_all <- c()

for(env_factor_name in env_factor$name){
  # Check for which years we already have data for
  # care: in case you stopped an operation to the db or changed the original data
  # you should remove the data from the db before any other operations.
  year_range[[env_factor_name]] <- year_range_orig
  # get all years from the db
  query <- sprintf('SELECT DISTINCT year FROM "%s" WHERE env_factor = %.0f;', 
                   "env_intensity", env_factor$id[env_factor$name == env_factor_name])
  result <- RPostgres::dbGetQuery(db, query)$year
  # get all years that are NOT in the db
  year_range[[env_factor_name]] <- year_range[[env_factor_name]][!(year_range[[env_factor_name]] %in% result)]
  
  # merge them together to one list which includes each year to be iterated over
  # where at least one env_factor is not yet in the db
  year_range_all <- c(year_range_all, year_range[[env_factor_name]][!(year_range[[env_factor_name]] %in% year_range_all)])
  
  rm(query, result)
}

# ------------------------------------------------------------------------------
# get data and insert ----------------------------------------------------------
# ------------------------------------------------------------------------------

start <- Sys.time()

data_E <- read_file_function(sprintf(file_format_noyear, file_names$E[1]))
data_X <- read_file_function(sprintf(file_format_noyear, file_names$X[1]))

for(year in year_range_all){
  print(paste("Year", year, "/", year_range_all[length(year_range_all)]))
  
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
  
  for(env_factor_name in env_factor$name){
    if(year %in% year_range[[env_factor_name]]){
      insert_data <- data %>%
        dplyr::mutate(env_factor = env_factor$id[env_factor$name == env_factor_name]) %>%
        dplyr::rename("amount" = env_factor_name) %>%
        dplyr::filter(abs(round(amount, digits = 2)) >= 0.01) %>% 
        # now we create amount as an environmental pressure by dividing it by total_production
        dplyr::mutate(amount = ifelse(total_production == 0, 0, amount/total_production)) %>% 
        dplyr::select(from_region, from_product, env_factor, year, amount) %>% 
        as.data.frame()
      
      # append env_intensity with the amount for the environmental factor currently in loop
      # RPostgres::dbAppendTable(db, name = "env_intensity", value = insert_data)
      RPostgres::dbWriteTable(db, name = "env_intensity", value = insert_data, append = TRUE)
    }
  }
  
  rm(data, insert_data)
}

print("6. Populating env_intensity took")
print(Sys.time()-start)
# Time difference of 1.938291 mins

# env_intensity <- RPostgres::dbReadTable(db, "env_intensity")
