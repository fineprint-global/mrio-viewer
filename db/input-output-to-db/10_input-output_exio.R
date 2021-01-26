##################################################################
### 10. B_inv.rds - add input-output leontief for the hybrid
##################################################################

print("10_input-output_exio.R")

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
      dplyr::mutate(allocation = allocation[t,]$id)
    
    # TEMPORARY until updated in underlying data
    # change the amount of livestock_products from 1000 head to head 
    livestock_products <- product$id[product$product_group == product_group$id[product_group$name == "Live animals"]]
    insert_data <- insert_data %>% 
      dplyr::mutate(amount = if_else(from_product %in% livestock_products, 
                                     amount * 1000,
                                     amount))
    
    # now we filter for amounts >= 0.005
    insert_data <- insert_data %>% 
      dplyr::filter(abs(round(amount, digits = 2)) >= 0.01) %>% # this significantly reduces our number of rows
      dplyr::select(from_region, to_region, from_product, to_product, year, allocation, amount)
    
    print(Sys.time()-start)
    # 1.212636 mins
    
    start <- Sys.time()
    print("Saving current year to db")
    RPostgres::dbWriteTable(db, name = "input-output_leontief", value = insert_data, append = TRUE)
    print(Sys.time()-start)
    # dbAppendTable - Time difference of 16.63703 hours
    # dbWriteTable  - Time difference of 15.78698 mins
    
    rm(insert_data)
    gc()
  }
}

# io_data <- RPostgres::dbReadTable(db, "input-output_leontief")
