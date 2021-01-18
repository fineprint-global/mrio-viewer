##########################################################################
### 4. E.rds - add product info
##########################################################################

# in folder: items.csv, regions.csv
# in ./hybrid: fabio-exiobase.ods

print("04_product-info.R")

# # get information about FABIO products
# product_info <- read.csv2("./data/old/items_fabio.csv") %>% 
#   dplyr::select(Com.Code, Item, Unit, Group) %>% #, Com.Group)
#   # change the unit for Livestock from 1000 Head to head
#   # this will be changed automatically in the next version of FABIO
#   dplyr::mutate(Unit = as.character(Unit)) %>% 
#   dplyr::mutate(Unit = if_else(Unit == "1000 Head", "head", Unit))

product_info <- read.csv(paste0(folder_path, "items.csv")) %>% 
  dplyr::rename(Com.Code = comm_code,
                Item = item,
                Unit = unit,
                # Group = group,
                Group = comm_group)

# product_group table --------------------------------------------------
product_group <- RPostgres::dbReadTable(db, "product_group")

# if no information exists yet in this table we populate it
if(nrow(product_group) == 0){
  
  # adding EXIOBASE here as an additional "group"
  insert_data <- data.frame(
    name = c(unique(as.character(product_info$Group)),name_exio)
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
    dplyr::rename(name = Item, other_id = Com.Code) %>% 
    dplyr::select(name, product_unit, product_group, other_id)
  
  DBI::dbAppendTable(db, name = "product", value = insert_data)
  
  rm(insert_data)
  
  product <- RPostgres::dbReadTable(db, "product")
}

rm(product_info)

# EXIOBASE data for table ------------------------------------------------------

if(nrow(product) <= n_product_fabio){
  products_exio <- readODS::read_ods(paste0(folder_path, "hybrid/fabio-exiobase.ods"), sheet = 4)
  
  exio_ids <- colnames(products_exio)[colnames(products_exio)!=""]
  
  insert_data <- data.frame(
    name = as.vector(t(products_exio[1,exio_ids])),
    product_group = product_group$id[product_group$name == name_exio],
    product_unit = product_unit$id[product_unit$name=="tonnes"],
    other_id = exio_ids
  )
  
  DBI::dbAppendTable(db, name = "product", value = insert_data)
  
  rm(insert_data)
  
  product <- RPostgres::dbReadTable(db, "product")
}

product_fabio <- product %>% dplyr::slice(1:n_product_fabio)
product_exio <- product %>% dplyr::slice((n_product_fabio+1):nrow(product))
