##################################################################
### 7. Y.rds - add final demand
##################################################################

print("07_final-demand_fabio.R")

# --------------------------------------------------------------
# preparation --------------------------------------------------
# --------------------------------------------------------------

y_info <- data.frame(
  # element = c("Food", "OtherUses", "StockVariation", "Balancing"),
  # type = c("Food", "-none-", "-none-", "-none-"),
  element = c("balancing", "food", "losses", "other",  "stockaddition", "unspecified"),
  type =    c("-none-",    "Food", "-none-", "-none-", "-none-",        "-none-"),
  stringsAsFactors = FALSE
)

# type table --------------------------------------------------
type <- RPostgres::dbReadTable(db, "type")

if(nrow(type) == 0){
  
  insert_data <- data.frame(
    name = unique(c(y_info$type, "Nonfood"))
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

element_fabio <- element %>% dplyr::slice(1:nrow(y_info))

rm(y_info)

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

data_Y <- read_file_function(sprintf(file_format_noyear, file_names$final_demand[1]))

# prepare df with index that we can left-join to the summary of the sparse matrix
# normally this will coincide with the id
region_incl_index <- region_fabio %>% dplyr::select(region_id = id)
region_incl_index$index <- 1:nrow(region_fabio)

region_input_fabio <- read.csv("../data/regions.csv", encoding = "latin1") %>% 
  dplyr::left_join(region_fabio, by = c("iso3c" = "iso3")) %>% 
  dplyr::select(code, id)

for(year in year_range){
  print(paste("Year", year, "/", year_range[length(year_range)]))
  
  data <- data_Y[[as.character(year)]]
  
  # this is a sparse matrix, so handling it is a bit different to normal matrices
  
  df <- as.data.frame(summary(data)) # i are rows, j are columns
  # take care of rows:
  df$from_region <- ceiling(df$i/n_product_fabio)
  # df$product <- ceiling(df$i/n_region_fabio)
  # take care of columns:
  df$to_region <- ceiling(df$j/6) # there are 6 different categories
  df$y_category <- ceiling(df$j/192)
  
  # row name is product id 
  df$prod_other_id <- data@Dimnames[[1]][df$i]
  # col name is regionid_category, e.g. 1_food
  df$col_name <- data@Dimnames[[2]][df$j]
  # remove the _ in order to be able to apply tidyr::separate()
  df$col_name <- str_replace(df$col_name, "stock_addition", "stockaddition")
  
  insert_data <- df %>% 
    dplyr::left_join(product_fabio[,c("other_id","id")], by = c("prod_other_id" = "other_id")) %>% 
    dplyr::rename(product = id) %>% 
    dplyr::left_join(region_incl_index, by = c("from_region" = "index")) %>% 
    dplyr::select(-from_region, from_region = region_id) %>% 
    # dplyr::left_join(region_incl_index, by = c("to_region" = "index")) %>% 
    tidyr::separate(col = col_name, into = c("to_region", "element"), convert = TRUE) %>% 
    dplyr::left_join(region_input_fabio, by = c("to_region" = "code")) %>% # convert FAO IDs to our IDs
    dplyr::select(-to_region, to_region = id) %>% 
    # join element
    dplyr::left_join(element[,c("id", "name")], by = c("element" = "name")) %>%
    dplyr::select(-element, element = id) %>%
    dplyr::mutate(year = year) %>% 
    dplyr::select(from_region, to_region, product, element, year, amount = x) %>% 
    # dplyr::mutate(amount = round(amount, digits = 2)) %>% 
    dplyr::filter(abs(round(amount, digits = 2)) >= 0.01) %>%  # filter the amount
    as.data.frame()
  
  rm(df)
  
  start <- Sys.time()
  RPostgres::dbWriteTable(db, name = "final_demand", value = insert_data, append = TRUE)
  print(Sys.time()-start)
  # dbAppendTable - Time difference of 2.963786 hours
  # dbWriteTable  - Time difference of 50.27912 secs (less than 0.5% of the prev time)
  
  rm(insert_data)
  gc()
}
