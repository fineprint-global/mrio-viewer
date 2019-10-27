##################################################################
### 8. Final demand from EXIOBASE
##################################################################

print("09_final-demand_exio.R")

# --------------------------------------------------------------
# preparation --------------------------------------------------
# --------------------------------------------------------------

# element table --------------------------------------------------
element <- RPostgres::dbReadTable(db, "element")

if(nrow(element) <= 4){
  
  # get type table for the nonfood-id
  type <- RPostgres::dbReadTable(db, "type")
  
  # load the Y.codes to get the 7 element-names
  # (e.g. Final concumption expenditure by households)
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/Y.codes.RData"))
  
  # get the 7 elements
  insert_data <- data.frame(
    name = unique(Y.codes$`Final Demand Category`),
    type = type$id[type$name == "Nonfood"]
  )
  
  rm(Y.codes)
  
  DBI::dbAppendTable(db, name = "element", value = insert_data)
  
  rm(insert_data)
  
  element <- RPostgres::dbReadTable(db, "element")
}

element_exio <- element %>% dplyr::slice(5:nrow(element))

# ----------------------------------------------------------------
# check years ----------------------------------------------------
# ----------------------------------------------------------------

# Check for which years we already have data for
# care: in case you stopped an operation to the db or changed the original data
# you should remove the data from the db before any other operations.
year_range <- year_range_orig
# get all years from the db
query <- sprintf('SELECT DISTINCT year FROM "%s" WHERE element IN (%s);', 
                 "final_demand", paste(element_exio$id, collapse = ","))
result <- RPostgres::dbGetQuery(db, query)$year
# get all years that are NOT in the db
year_range <- year_range[!(year_range %in% result)]

rm(query, result)

# change year range to start only at 1995
year_range <- year_range[!(year_range < 1995)]

# --------------------------------------------------------------
# preparation --------------------------------------------------
# --------------------------------------------------------------

# ----------------------------------------------------------------
# get data and insert --------------------------------------------
# ----------------------------------------------------------------

for(year in year_range){
  print(paste("10 Y2: Year", year, "/", year_range[length(year_range)]))

  # 343 columns, 49 regions, 7 stages per region
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
  Y_exio <- Y
  rm(Y)
  
  insert_data <- Y_exio %>%
    as_tibble() %>% # as_tibble is needed because gather needs it
    gather(key = "region_element", value = "amount")
  
  rm(Y_exio)
  
  # we have AT-household, all the FABIO rows
  # then AT-something else, all the FABIO rows
  # etc.
  insert_data <- insert_data %>% 
    dplyr::select(-region_element) %>% 
    dplyr::mutate(from_region = rep(region_exio$id, each = nrow(product_exio), times = nrow(element_exio)*nrow(region_exio))) %>% # each for 130 products, 130 products * 192 countries
    dplyr::mutate(product = rep(product_exio$id, times = nrow(region_exio)*nrow(element_exio)*nrow(region_exio))) %>% # 130 products * 192 countries * 192 countries
    dplyr::mutate(to_region = rep(region_exio$id, each = nrow(product_exio)*nrow(region_exio)*nrow(element_exio))) %>% # first, there is always region 1, then region 2 etc.
    dplyr::mutate(element = rep(element_exio$id, each = nrow(product_exio)*nrow(region_exio), times = nrow(region_exio))) %>% # the first column is to_product 1 and to_region 1 130*192 times
    dplyr::mutate(year = year) %>%
    # dplyr::filter(amount != 0) %>% # this significantly reduces our number of rows (e.g. for 2013 it's down by over 90%!)
    # dplyr::filter(amount > 0.01 | amount < -0.01) %>% # this significantly reduces our number of rows (e.g. for 2013 it's down by over 99.99955%!)
    dplyr::filter(abs(round(amount, digits = 2)) >= 0.01) %>% # this significantly reduces our number of rows (e.g. for 2013 it's down by over 99.99939%!)
    dplyr::select(from_region, to_region, product, element, year, amount)
  
  start <- Sys.time()
  RPostgres::dbAppendTable(db, name = "final_demand", value = insert_data)
  print(Sys.time()-start)
  # Time difference of 59.15825 mins
  
  rm(insert_data)
  gc()
}