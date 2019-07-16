##################################################################
### 9. B_inv.rds - add input-output leontief for the hybrid
##################################################################

print("09_B_inv.R")

library(Matrix)

# # if run as standalone, run:
# setwd("db/input-output-to-db") # only needed if you execute manually, not bash
# source('01-03_setup.R')

# ----------------------------------------------------------------
# preparation ----------------------------------------------------
# ----------------------------------------------------------------

# get other tables -----------------------------------------------
allocation <- RPostgres::dbReadTable(db, "allocation")

# ----------------------------------------------------------------
# get data and insert --------------------------------------------
# ----------------------------------------------------------------

# DBI::dbSendQuery(db, 'DELETE FROM "input-output_leontief";')

for(t in c(1:nrow(allocation))){
  
  print(paste("Now allocation", t))
  # t <- 2
  
  # Check for which years we already have data for, for the current allocation
  # care: in case you stopped an operation to the db or changed the original data
  # you should remove the data from the db before any other operations.
  year_range <- year_range_orig
  # get all years from the db
  query <- sprintf('SELECT DISTINCT year FROM "%s" 
                   WHERE allocation = %s
                   AND to_region IN (%s);', 
                   "input-output_leontief",
                   allocation[t,]$id,
                   paste(region_exio$id, collapse = ","))
  result <- RPostgres::dbGetQuery(db, query)$year
  # get all years that are NOT in the db
  year_range <- year_range[!(year_range %in% result)]
  
  rm(query, result)
  
  for(year in year_range){
    # year <- 2013 # temporarily just 2013
    
    data <- read_file_function(sprintf(file_format_subfolder, "hybrid", year, file_names$B[t]))
    
    start <- Sys.time()
    print(paste("Preparing data for io_leontief for", 
                year, 
                "and filtering it already"))
    
    
    
    # change the data format from 192*130 columns to 2 columns and more rows
    # to easier get it into the database
    insert_data <- as.data.frame(as.matrix(data)) %>%
      as_tibble() %>% # as_tibble is needed because gather needs it
      gather(key = "product.region", value = "amount")
    
    rm(data)
    
    gc()
    
    insert_data <- insert_data %>% 
      # care: gather works column-wise, meaning our order is now different
      # it starts with the first region for every product to region and product (again working the same way)
      # e.g. FROM_REG 1, FROM_PROD 1 and TO_REG 1, TO_PROD 1
      # then FROM_REG 1, FROM_PROD 2 and TO_REG 1, TO_PROD 1
      # ...
      # then FROM_REG 192, FROM_PROD 130 and TO_REG 1, TO_PROD 1
      # then FROM_REG 1, FROM_PROD 1 and TO_REG 1, TO_PROD 2
      # ...
      # then FROM_REG 192, FROM_PROD 130 and TO_REG 1, TO_PROD 130
      # then FROM_REG 1, FROM_PROD 1 and TO_REG 2, TO_PROD 1
      # ...
      # finally
      # then FROM_REG 192, FROM_PROD 129 and TO_REG 192, TO_PROD 130
      # then FROM_REG 192, FROM_PROD 130 and TO_REG 192, TO_PROD 130
      dplyr::select(-product.region) %>% 
      dplyr::mutate(from_region = rep(region_fabio$id, each = nrow(product_fabio), times = nrow(product_exio)*nrow(region_exio))) %>% # each for 130 products, 130 products * 192 countries
      dplyr::mutate(from_product = rep(product_fabio$id, times = nrow(region_fabio)*nrow(product_exio)*nrow(region_exio))) %>% # 130 products * 192 countries * 192 countries
      dplyr::mutate(to_region = rep(region_exio$id, each = nrow(product_fabio)*nrow(region_fabio)*nrow(product_exio))) %>% # first, there is always region 1, then region 2 etc.
      dplyr::mutate(to_product = rep(product_exio$id, each = nrow(product_fabio)*nrow(region_fabio), times = nrow(region_exio))) %>% # the first column is to_product 1 and to_region 1 130*192 times
      dplyr::mutate(year = year) %>%
      dplyr::mutate(allocation = allocation[t,]$id) %>%
      # dplyr::filter(amount != 0) %>% # this significantly reduces our number of rows (e.g. for 2013 it's down by over 90%!)
      # dplyr::filter(amount > 0.01 | amount < -0.01) %>% # this significantly reduces our number of rows (e.g. for 2013 it's down by over 99.99955%!)
      dplyr::filter(abs(round(amount, digits = 2)) >= 0.01) %>% # this significantly reduces our number of rows (e.g. for 2013 it's down by over 99.99939%!)
      dplyr::select(from_region, to_region, from_product, to_product, year, allocation, amount)
    
    print(Sys.time()-start)
    
    start <- Sys.time()
    print("Saving current year to db")
    RPostgres::dbAppendTable(db, name = "input-output_leontief", value = insert_data)
    print(Sys.time()-start)
    # Time difference of 3.603777 hours
    
    rm(insert_data)
    
    gc()
  }

}

start <- Sys.time()
print("Retrieving data from db")
io_data <- RPostgres::dbReadTable(db, "input-output_leontief")
print(Sys.time()-start)
# Time difference of 1.868098 mins

rm(io_data)
