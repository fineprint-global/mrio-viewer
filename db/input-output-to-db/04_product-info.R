##########################################################################
### 4. E.rds - add product info
##########################################################################

print("04_product-info.R")

year <- 2013
data <- read_file_function(sprintf(file_format, year, file_names$E[1]))

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

# FABIO data for product table -------------------------------------------------
product <- RPostgres::dbReadTable(db, "product")

if(nrow(product) == 0){

  insert_data <- data[!duplicated(data$Item),] %>% # get all products that are not duplicated
    dplyr::rename("name" = "Item", "product_group" = "Group", "other_id" = "Com.Code") %>% # or Item.Code instead of Com.Code
    dplyr::select(name, product_group, other_id) %>% 
    # if product_group contains "Lifestock", we use unit-id 2 for "heads", otherwise "tonnes"
    dplyr::mutate(product_unit = ifelse(grepl("Livestock", as.character(product_group)), product_unit$id[product_unit$name=="heads"], product_unit$id[product_unit$name=="tonnes"])) %>% # TODO: check with Martin, if only Livestock or also Livestock products are counted in head
    # we can access the group id via as.numeric as we passed on the levels of the factor to the database
    dplyr::mutate(product_group = as.numeric(product_group))
  # note: we do not arrange the products by name here, because otherwise we would not match the order in Y.rds or any other files
  
  DBI::dbAppendTable(db, name = "product", value = insert_data)
  
  rm(insert_data)
  
  product <- RPostgres::dbReadTable(db, "product")
}

rm(data)

# EXIOBASE data for table ------------------------------------------------------

if(nrow(product) <= 130){
  insert_data <- read.csv2("../data/items_exio.csv") %>% 
    dplyr::rename(other_id = Code,
                  name = Item) %>% 
    dplyr::mutate(product_unit = product_unit$id[product_unit$name=="tonnes"]) # TODO: check if they are actually in tonnes
  
  DBI::dbAppendTable(db, name = "product", value = insert_data)
  
  rm(insert_data)
  
  product <- RPostgres::dbReadTable(db, "product")
}

product_fabio <- product %>% dplyr::slice(1:130)
product_exio <- product %>% dplyr::slice(131:nrow(product))