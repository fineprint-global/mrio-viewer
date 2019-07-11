##################################################################
### 5. Add region info
##################################################################

print("05_region-info.R")

name_fabio <- "FABIO"
name_exio <- "EXIOBASE"

regions_fabio <- read.csv2("../data/regions_fabio.csv")
regions_fao_exio <- read.csv2("../data/regions_fao-exio.csv")

regions_exio <- regions_fao_exio %>% 
  dplyr::select(EXIOregion, EXIOcode) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(EXIOcode) %>% 
  dplyr::filter(!is.na(EXIOregion)) %>% # filter NA, used for RoW
  dplyr::rename(name = EXIOregion) %>% 
  dplyr::select(name)

# FABIO data for region table --------------------------------------------------

region <- RPostgres::dbReadTable(db, "region")

if(nrow(region) == 0){
  
  # we keep data arranged by Country.Code as in the .csv
  insert_data <- regions_fabio %>% 
    dplyr::select(Country, ISO) %>% 
    dplyr::rename(name = Country,
                  iso3 = ISO)
  
  DBI::dbAppendTable(db, name = "region", value = insert_data)
  
  rm(insert_data)
  
  region <- RPostgres::dbReadTable(db, "region")
}

# EXIOBASE data for region table -----------------------------------------------

region <- RPostgres::dbReadTable(db, "region")

if(nrow(region) <= 192){ # this means that only FABIO regions are in the table so far
  
  insert_data <- data.frame(
    name = regions_exio$name
  )
  
  DBI::dbAppendTable(db, name = "region", value = insert_data)
  
  rm(insert_data)
  
  region <- RPostgres::dbReadTable(db, "region") 
}

# region_cluster table ---------------------------------------------------------

region_cluster <- RPostgres::dbReadTable(db, "region_cluster")

if(nrow(region_cluster) == 0){
  insert_data <- data.frame(
    name = c(name_fabio, name_exio)
  )
  
  DBI::dbAppendTable(db, name = "region_cluster", value = insert_data)
  
  rm(insert_data)
  
  region_cluster <- RPostgres::dbReadTable(db, "region_cluster") 
}

# region_cluster_region table --------------------------------------------------

region_cluster_region <- RPostgres::dbReadTable(db, "region_cluster_region")

if(nrow(region_cluster_region) == 0){
  region_cluster_fabio <- region %>% 
    dplyr::slice(1:192) %>% 
    dplyr::select(id) %>% 
    dplyr::rename(id_region = id) %>% 
    dplyr::mutate(id_region_cluster = region_cluster$id[region_cluster$name == name_fabio])
  
  region_cluster_exio <- region %>% 
    dplyr::slice(193:nrow(region)) %>% 
    dplyr::select(id) %>% 
    dplyr::rename(id_region = id) %>% 
    dplyr::mutate(id_region_cluster = region_cluster$id[region_cluster$name == name_exio])
  
  DBI::dbAppendTable(db, name = "region_cluster_region", value = region_cluster_fabio)
  DBI::dbAppendTable(db, name = "region_cluster_region", value = region_cluster_exio)
  
  region_cluster_region <- RPostgres::dbReadTable(db, "region_cluster_region")
}

region_cluster_tbl <- dplyr::tbl(db, "region_cluster")
region_cluster_region_tbl <- dplyr::tbl(db, "region_cluster_region")

region_tbl <- dplyr::tbl(db, "region") %>% 
  dplyr::full_join(region_cluster_region_tbl, by = c("id" = "id_region")) %>% 
  dplyr::left_join(region_cluster_tbl, by = c("id_region_cluster" = "id"), suffix=c("",".cluster")) %>% 
  dplyr::filter(name.cluster %in% c(name_fabio, name_exio)) %>% 
  dplyr::select(-geometry, -id_region_cluster)

region_fabio <- region_tbl %>% 
  dplyr::filter(name.cluster == name_fabio) %>% 
  dplyr::select(-name.cluster) %>% 
  dplyr::collect()

region_exio <- region_tbl %>% 
  dplyr::filter(name.cluster == name_exio) %>% 
  dplyr::select(-name.cluster) %>% 
  dplyr::collect()

# FABIO continent-data for region_cluster --------------------------------------
## now we take the data for Continent and create a region cluster

# TODO
