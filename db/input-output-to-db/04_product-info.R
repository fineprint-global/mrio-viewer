##########################################################################
### 4. E.rds - add product info
##########################################################################

print("04_product-info.R")

# get information about FABIO products
product_info <- read.csv2("../data/items_fabio.csv") %>% 
  dplyr::select(Com.Code, Item, Unit, Group) %>% #, Com.Group)
  # change the unit for Livestock from 1000 Head to head
  dplyr::mutate(Unit = as.character(Unit)) %>% 
  dplyr::mutate(Unit = if_else(Unit == "1000 Head", "head", Unit))

# product_group table --------------------------------------------------
product_group <- RPostgres::dbReadTable(db, "product_group")

if(nrow(product_group) == 0){
  
  insert_data <- data.frame(
    name = unique(product_info$Group)
  )
  
  DBI::dbAppendTable(db, name = "product_group", value = insert_data)
  
  rm(insert_data)
  
  product_group <- RPostgres::dbReadTable(db, "product_group")
}

# product_unit table --------------------------------------------------

product_unit <- RPostgres::dbReadTable(db, "product_unit")

if(nrow(product_unit) == 0){
  
  insert_data <- data.frame(
    name = unique(product_info$Unit)
  )
  
  DBI::dbAppendTable(db, name = "product_unit", value = insert_data)
  
  rm(insert_data)
  
  product_unit <- RPostgres::dbReadTable(db, "product_unit")
}

# FABIO data for product table -------------------------------------------------
product <- RPostgres::dbReadTable(db, "product")

if(nrow(product) == 0){

  insert_data <- product_info %>%
    dplyr::left_join(product_group, by = c("Group" = "name"), suffix = c("", ".group")) %>% 
    dplyr::rename(product_group = id) %>%
    dplyr::left_join(product_unit, by = c("Unit" = "name"), suffix = c("", ".unit")) %>% 
    dplyr::rename(product_unit = id) %>% 
    dplyr::select(-Unit, -Group) %>% 
    dplyr::rename(name = Item, other_id = Com.Code)
  
  DBI::dbAppendTable(db, name = "product", value = insert_data)
  
  rm(insert_data)
  
  product <- RPostgres::dbReadTable(db, "product")
}

rm(product_info)

# EXIOBASE data for table ------------------------------------------------------

if(nrow(product) <= 130){
  insert_data <- read.csv2("../data/items_exio.csv") %>% 
    dplyr::rename(other_id = Code,
                  name = Item) %>% 
    dplyr::mutate(product_unit = product_unit$id[product_unit$name=="tonnes"])
  
  DBI::dbAppendTable(db, name = "product", value = insert_data)
  
  rm(insert_data)
  
  product <- RPostgres::dbReadTable(db, "product")
}

product_fabio <- product %>% dplyr::slice(1:130)
product_exio <- product %>% dplyr::slice(131:nrow(product))
