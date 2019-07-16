##################################################################
### 7. Y.rds - add final demand
##################################################################

print("07_final-demand.R")

# --------------------------------------------------------------
# preparation --------------------------------------------------
# --------------------------------------------------------------

y_info <- data.frame(
  element = c("Food", "OtherUses", "StockVariation", "Balancing"),
  type = c("Food", "Nonfood", "Food", "Food")
)

# type table --------------------------------------------------
type <- RPostgres::dbReadTable(db, "type")

if(nrow(type) == 0){
  
  insert_data <- data.frame(
    name = unique(y_info$type)
  )
  
  DBI::dbAppendTable(db, name = "type", value = insert_data)
  
  rm(insert_data)

  type <- RPostgres::dbReadTable(db, "type")
}

y_info <- y_info %>% 
  dplyr::left_join(type, by = c("type" = "name")) %>% 
  dplyr::rename("type_name" = "type", "type" = "id")

# element table --------------------------------------------------
element <- RPostgres::dbReadTable(db, "element")

if(nrow(element) == 0){
    
  insert_data <- data.frame(
    name = y_info$element,
    type = y_info$type
  )
  
  DBI::dbAppendTable(db, name = "element", value = insert_data)
  
  rm(insert_data)
  
  element <- RPostgres::dbReadTable(db, "element")
}

element_fabio <- element %>% dplyr::slice(1:4)

# other tables ---------------------------------------------------
product <- product_fabio
region <- region_fabio
element <- element_fabio

# ----------------------------------------------------------------
# check years ----------------------------------------------------
# ----------------------------------------------------------------

# Check for which years we already have data for
# care: in case you stopped an operation to the db or changed the original data
# you should remove the data from the db before any other operations.
year_range <- year_range_orig
# get all years from the db
query <- sprintf('SELECT DISTINCT year FROM "%s" WHERE element IN (%s);', 
                 "final_demand", paste(element$id, collapse = ","))
result <- RPostgres::dbGetQuery(db, query)$year
# get all years that are NOT in the db
year_range <- year_range[!(year_range %in% result)]

rm(query, result)

# ----------------------------------------------------------------
# get data and insert --------------------------------------------
# ----------------------------------------------------------------

for(year in year_range){
  # year <- 2013 # temporarily just 2013
  
  print(paste("Year", year, "/", year_range[length(year_range)]))
  
  data <- read_file_function(sprintf(file_format, year, file_names$final_demand[1]))
  
  # change the data format from 192*4 columns to 2 columns and more rows
  # to easier get it into the database
  insert_data <- data %>%
    as_tibble() %>% # as_tibble is needed because gather needs it
    gather(key = "REG_element", value = "amount") %>% 
    # care: gather works column-wise, meaning our order is now different
    # it starts with the first region and element for every product
    # (e.g. ARM_Food*130, then ARM_OtherUses*130) etc
    dplyr::mutate(iso3 = substr(REG_element, 1, 3)) %>% # get region code 
    dplyr::mutate(element = substr(REG_element, 5, nchar(REG_element))) %>% # get element name
    dplyr::select(-REG_element) %>%
    # join region
    dplyr::left_join(region, by = c("iso3")) %>% 
    dplyr::select(-name, -iso3) %>% 
    dplyr::rename("from_region" = "id") %>% 
    # join element
    dplyr::left_join(element, by = c("element" = "name")) %>% 
    dplyr::select(-element, -type) %>% 
    dplyr::rename("element" = "id") %>% 
    dplyr::mutate(year = year) %>%
    dplyr::mutate(to_region = rep(region$id, each = 130, times = 4*192)) %>% # each for 130 products, 4 elem * 192 countries
    dplyr::mutate(product = rep(product$id, times = 4*192^2)) %>% # 4 elements * 192 countries * 192 countries
    dplyr::select(from_region, to_region, product, element, year, amount) %>% 
    dplyr::filter(abs(round(amount, digits = 2)) >= 0.01) # filter the amount
  
  rm(data)
  
  start <- Sys.time()
  RPostgres::dbAppendTable(db, name = "final_demand", value = insert_data)
  print(Sys.time()-start)
  # Time difference of 13.09839 hours
  
  rm(insert_data)
  gc()
}

start <- Sys.time()
y_data <- RPostgres::dbReadTable(db, "final_demand")
print(Sys.time()-start)
# Time difference of 39.64467 secs

rm(insert_data)

