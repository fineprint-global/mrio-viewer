library(tidyverse)

##########################################################################
### 4. E.rds - add general info about product, region, landuse and biomass
##########################################################################

print("04_general-info.R")

year <- 2013
data <- read_file_function(sprintf(file_format, year, file_names$E[1]))

# refactor data$Country to match the order in the data
data$Country <- factor(data$Country, levels = unique(data$Country))

# region table --------------------------------------------------

region <- RPostgres::dbReadTable(db, "region")

if(nrow(region) == 0){
  
  ## 1. read the country codes from the final demand file
  tmp <- read_file_function(sprintf(file_format, year, file_names$final_demand[1]))
  
  # change the data format from 192*4 columns to 2 columns and more rows
  tmp_gathered <- tmp %>%
    as_tibble() %>% 
    gather(key = "REG_element", value = "amount")
  
  rm(tmp)
  
  country_codes <- unique(substr(tmp_gathered$REG_element, 1, 3)) # %>% 
  # enframe(name = NULL)
  
  rm(tmp_gathered)
  
  ## 2. insert the actual data into the database
  insert_data <- data.frame(
    name = levels(data$Country),
    iso3 = country_codes # TODO: ROW is not an ISO3 code, so probably add a column for other_code
    # countrycode::countrycode(sourcevar = levels(data$Country),
    #                               origin = "country.name",
    #                               destination = "iso3c") # get the iso3 code for the country
  )
  
  DBI::dbAppendTable(db, name = "region", value = insert_data)
  
  rm(insert_data)
  
  region <- RPostgres::dbReadTable(db, "region")
}

# NOTE: this would also be one potential place to populate
#       region_aggregate or region_cluster

# product_group table --------------------------------------------------
product_group <- RPostgres::dbReadTable(db, "product_group")

if(nrow(product_group) == 0){
  
  insert_data <- data.frame(
    name = unique(data$Group)
  )
  
  DBI::dbAppendTable(db, name = "product_group", value = insert_data)
  
  rm(insert_data)
  
  product_group <- RPostgres::dbReadTable(db, "product_group")
}

# product_unit table --------------------------------------------------

product_unit <- RPostgres::dbReadTable(db, "product_unit")

if(nrow(product_unit) == 0){
  
  insert_data <- data.frame(
    name = c("tonnes", "heads") # head for lifestock, tonnes for the rest
  )
  
  number_product_units <- length(insert_data$name)
  
  DBI::dbAppendTable(db, name = "product_unit", value = insert_data)
  
  rm(insert_data)
  
  product_unit <- RPostgres::dbReadTable(db, "product_unit")
}

# product table --------------------------------------------------
product <- RPostgres::dbReadTable(db, "product")

if(nrow(product) == 0){

  insert_data <- data[!duplicated(data$Item),] %>% # get all products that are not duplicated
    dplyr::rename("name" = "Item", "product_group" = "Group", "other_id" = "Com.Code") %>% # or Item.Code instead of Com.Code
    dplyr::select(name, product_group, other_id) %>% 
    # if product_group contains "Lifestock", we use unit-id 2 for "heads", otherwise "tonnes"
    dplyr::mutate(product_unit = ifelse(grepl("Livestock", as.character(product_group)), 2, 1)) %>% # TODO: check with Martin, if only Livestock or also Livestock products are counted in head
    # we can access the group id via as.numeric as we passed on the levels of the factor to the database
    dplyr::mutate(product_group = as.numeric(product_group))
  # note: we do not arrange the products by name here, because otherwise we would not match the order in Y.rds or any other files
  
  DBI::dbAppendTable(db, name = "product", value = insert_data)
  
  rm(insert_data)
  
  product <- RPostgres::dbReadTable(db, "product")
}

# env_factor_unit table --------------------------------------------------
env_factor_unit <- RPostgres::dbReadTable(db, "env_factor_unit")

if(nrow(env_factor_unit) == 0){
    
  insert_data <- data.frame(
    name = c("ha", "tonnes") #, "NA") # ha for landuse, tonnes for biomass
  )
  
  DBI::dbAppendTable(db, name = "env_factor_unit", value = insert_data)
  
  rm(insert_data)
  
  env_factor_unit <- RPostgres::dbReadTable(db, "env_factor_unit")
}
  
# env_factor table --------------------------------------------------
env_factor <- RPostgres::dbReadTable(db, "env_factor")

if(nrow(env_factor) == 0){
    
  insert_data <- data.frame(
    name = c("landuse", "biomass"), #, "total production"),
    env_factor_unit = env_factor_unit$id
  )
  
  DBI::dbAppendTable(db, name = "env_factor", value = insert_data)
  
  rm(insert_data)
  
  env_factor <- RPostgres::dbReadTable(db, "env_factor")
}

rm(data)
