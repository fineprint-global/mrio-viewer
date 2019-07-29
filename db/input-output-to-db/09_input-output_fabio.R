##################################################################
### 8. L.rds - add input-output leontief
##################################################################

print("08_input-output.R")

# # if run as standalone, run:
# setwd("db/input-output-to-db") # only needed if you execute manually, not bash
# source('01-03_setup.R')

# ----------------------------------------------------------------
# preparation ----------------------------------------------------
# ----------------------------------------------------------------

# allocation table -----------------------------------------------
allocation <- RPostgres::dbReadTable(db, "allocation")

if(nrow(allocation) == 0){
  insert_data <- data.frame(
    name = allocation_type
  )
  
  DBI::dbAppendTable(db, name = "allocation", value = insert_data)
  
  rm(insert_data)
  
  allocation <- RPostgres::dbReadTable(db, "allocation")
}

# get other tables -----------------------------------------------
product <- product_fabio
region <- region_fabio
product_group <- RPostgres::dbReadTable(db, "product_group")

n_product <- nrow(product)
n_region <- nrow(region)

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
                   paste(region_fabio$id, collapse = ","))
  result <- RPostgres::dbGetQuery(db, query)$year
  # get all years that are NOT in the db
  year_range <- year_range[!(year_range %in% result)]
  
  rm(query, result)
  
  for(year in year_range){
    # year <- 2013 # temporarily just 2013
    
    data <- read_file_function(sprintf(file_format, year, file_names$io_leontief[t]))
    
    start <- Sys.time()
    print(paste("Preparing data for io_leontief for", 
                year, 
                "and filtering it already"))
    
    # change the data format from 192*130 columns to 2 columns and more rows
    # to easier get it into the database
    insert_data <- data %>%
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
      dplyr::mutate(from_region = rep(region$id, each = n_product, times = n_product*n_region)) %>% # each for 130 products, 130 products * 192 countries
      dplyr::mutate(from_product = rep(product$id, times = n_product*n_region^2)) %>% # 130 products * 192 countries * 192 countries
      dplyr::mutate(to_region = rep(region$id, each = n_product*n_region*n_product)) %>% # first, there is always region 1, then region 2 etc.
      dplyr::mutate(to_product = rep(product$id, each = n_product*n_region, times = n_region)) %>% # the first column is to_product 1 and to_region 1 130*192 times
      dplyr::mutate(year = year) %>%
      dplyr::mutate(allocation = allocation[t,]$id)
    
    # TEMPORARY until updated in underlying data
    # change the amount of livestock_products from 1000 head to head 
    livestock_products <- product$id[product$product_group == product_group$id[product_group$name == "Livestock"]]
    
    insert_data <- insert_data %>% 
      dplyr::mutate(amount = if_else(from_product %in% livestock_products, 
                                     amount * 1000,
                                     amount)) %>% 
      dplyr::mutate(amount = if_else(to_product %in% livestock_products, 
                                     amount / 1000,
                                     amount))
    
    # now we filter for amounts >= 0.01
    insert_data <- insert_data %>% 
      # dplyr::filter(amount != 0) %>% # this significantly reduces our number of rows (e.g. for 2013 it's down by over 90%!)
      # dplyr::filter(amount > 0.01 | amount < -0.01) %>% # this significantly reduces our number of rows (e.g. for 2013 it's down by over 99.99955%!)
      dplyr::filter(abs(round(amount, digits = 2)) >= 0.01) %>% # this significantly reduces our number of rows (e.g. for 2013 it's down by over 99.99939%!)
      dplyr::select(from_region, to_region, from_product, to_product, year, allocation, amount)
    
    print(Sys.time()-start)
    
    start <- Sys.time()
    print("Saving current year to db")
    RPostgres::dbAppendTable(db, name = "input-output_leontief", value = insert_data)
    print(Sys.time()-start)
    # Time difference of 1.808534 days (with 50M rows)
    # Time difference of 17.7575 mins (with 380k rows)
    
    rm(insert_data)
    
    gc()
  }

}

start <- Sys.time()
print("Retrieving data from db")
# io_data <- RPostgres::dbReadTable(db, "input-output_leontief")
print(Sys.time()-start)
# Time difference of 1.868098 mins

rm(io_data)
