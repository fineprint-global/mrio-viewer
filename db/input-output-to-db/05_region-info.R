##################################################################
### 5. Add region info
##################################################################

print("05_region-info.R")

regions_fao_exio <- readODS::read_ods(paste0(folder_path, "hybrid/fabio-exiobase.ods"), sheet = 3)

regions_fabio <- regions_fao_exio %>% 
  dplyr::select(name = Country, iso3 = ISO)

regions_exio <- regions_fao_exio %>% 
  dplyr::select(EXIOBASE, EXIOBASE_code) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(EXIOBASE_code) %>% 
  dplyr::filter(!is.na(EXIOBASE)) %>% # filter NA, used for RoW
  dplyr::select(name = EXIOBASE)

# FABIO data for region table --------------------------------------------------

region <- RPostgres::dbReadTable(db, "region")

if(nrow(region) == 0){
  
  insert_data <- regions_fabio
  
  DBI::dbAppendTable(db, name = "region", value = insert_data)
  
  rm(insert_data)
  
  region <- RPostgres::dbReadTable(db, "region")
}

# EXIOBASE data for region table -----------------------------------------------

if(nrow(region) <= n_region_fabio){ # this means that only FABIO regions are in the table so far
  
  insert_data <- regions_exio
  
  DBI::dbAppendTable(db, name = "region", value = insert_data)
  
  rm(insert_data)
  
  region <- RPostgres::dbReadTable(db, "region") 
}

# region_cluster table ---------------------------------------------------------
# we use two main clusters, FABIO and EXIOBASE

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
# now we populate the FABIO and EXIOBASE clusters

region_cluster_region <- RPostgres::dbReadTable(db, "region_cluster_region")

if(nrow(region_cluster_region) == 0){
  region_cluster_fabio <- region %>% 
    dplyr::slice(1:n_region_fabio) %>% 
    dplyr::select(id) %>% 
    dplyr::rename(id_region = id) %>% 
    dplyr::mutate(id_region_cluster = region_cluster$id[region_cluster$name == name_fabio])
  
  region_cluster_exio <- region %>% 
    dplyr::slice((n_region_fabio+1):nrow(region)) %>% 
    dplyr::select(id) %>% 
    dplyr::rename(id_region = id) %>% 
    dplyr::mutate(id_region_cluster = region_cluster$id[region_cluster$name == name_exio])
  
  DBI::dbAppendTable(db, name = "region_cluster_region", value = region_cluster_fabio)
  DBI::dbAppendTable(db, name = "region_cluster_region", value = region_cluster_exio)
  
  region_cluster_region <- RPostgres::dbReadTable(db, "region_cluster_region")
}

# now add the information about region_cluster to the region so we get 
# region_fabio and region_exio that are used for further processing
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

# function for region_aggregate ------------------------------------------------
add_region_aggregate <- function(db, 
                                 region_aggregate_id, 
                                 region_in_aggregate_ids){
  
  ## 1. check if the db is valid
  if(!DBI::dbIsValid(db)){
    base::stop("Database connection is invalid.")
  }
  
  ## 2. check the args with the ids from the db to make sure all are region ids
  ids <- dplyr::tbl(db, "region") %>% dplyr::select(id) %>% dplyr::collect()
  
  if(!(region_aggregate_id %in% ids$id)){
    base::stop("Region_aggregate_id is not in the list of possible ids from the database.")
  }
  if(any(!(region_in_aggregate_ids %in% ids$id))){
    base::stop("At least one of the region_in_aggregate_ids is not 
                in the list of possible ids from the database.")
  }
  
  ## 3. before we insert the data, we check for data already in the db
  reg_agg <- dplyr::tbl(db, "region_aggregate") %>% 
    dplyr::filter(region_aggregate == region_aggregate_id) %>% 
    dplyr::select(region_in_aggregate) %>% 
    dplyr::collect()
  # we only insert the ids that are not yet in the db
  region_in_aggregate_ids <- region_in_aggregate_ids[!(region_in_aggregate_ids %in%
                                                       reg_agg$region_in_aggregate)]
  # return if there are no new ids to insert
  if(length(region_in_aggregate_ids) == 0){ return() }
  
  ## 4. create the insert_data
  insert_data <- data.frame(
    region_aggregate = region_aggregate_id,
    region_in_aggregate = region_in_aggregate_ids
  )
  
  ## 5. insert data into the db, that doesn't exist yet
  DBI::dbAppendTable(db, name = "region_aggregate", value = insert_data)
}

# match FABIO and EXIOBASE regions in region_aggregate -------------------------
# e.g. the FABIO region Austria will be matched with EXIO region Austria
#      but FABIO regions Albania, Andorra and others with EXIO region RoW Europe
for(reg_exio_id in region_exio$id){
  reg_exio_name <- region_exio$name[region_exio$id == reg_exio_id]
  
  # get all FAO countries in this EXIOregion
  regions_in_aggregate <- regions_fao_exio$Country[regions_fao_exio$EXIOregion == 
                                                     reg_exio_name &
                                                   !is.na(regions_fao_exio$EXIOregion)]
  
  # now get the ids from the database for all exact matches in the names
  ids <- region_fabio$id[region_fabio$name %in% regions_in_aggregate]
  
  add_region_aggregate(db, reg_exio_id, ids)
}

# continent-data for region_cluster_region -------------------------------------
## now we take the data for Continent and create a region cluster

## methodology behind choosing what cluster to use:
# 1. if FABIO country = EXIO region, 
#    then take the continent data from regions_fabio
# 2. if RoW EXIO region = FABIO continent, take the continent data
# 3. if RoW EXIO region covers multiple FABIO continents
#    i.e. RoW Middle East, I choose the dominant one (which is Asia & Pacific)

# region_cluster_region table --------------------------------------------------
# now we populate the FABIO and EXIOBASE clusters

region_cluster_region <- RPostgres::dbReadTable(db, "region_cluster_region")

if(nrow(region_cluster_region) <= (n_region_fabio+n_region_exio)){ # 241 = 192 (FABIO reg) + 49 (EXIO reg)
  
  # first, the Continent information is manually recoded
  # to allow for displaying the new names directly
  region_cluster_fabio <- read.csv("../data/regions.csv", encoding = "latin1")
  region_cluster_fabio$continent <- 
    dplyr::recode(region_cluster_fabio$continent,
                  `AFR` = "Africa",
                  `ASI` = "Asia and Pacific",
                  `EU`  = "European Union",
                  `EUR` = "Europe (non-EU)",
                  `LAM` = "Latin America",
                  `NAM` = "North America",
                  `OCE` = "Asia and Pacific",
                  `ROW` = "Rest of World"
    )
  region_cluster_fabio$continent <- as.character(region_cluster_fabio$continent)
  # NOTE: the term "Continent" might be a bit confusing here
  # as EU or RoW are not actual continents but we keep using it for now
  
  region_cluster_exio <- regions_fao_exio %>% 
    dplyr::left_join(region_cluster_fabio[,c("iso3c","continent")], 
                     by = c("ISO" = "iso3c")) %>%
    dplyr::select(EXIOBASE, continent) %>%
    dplyr::mutate(continent = if_else(EXIOBASE == "RoW Middle East",
                                      "Asia and Pacific",
                                      continent)) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(EXIOBASE) %>%
    dplyr::filter(!is.na(EXIOBASE),
                  !is.na(continent))
  
  ## we create our continents in the region_cluster table
  # region_cluster table ---------------------------------------------------------
  region_cluster <- RPostgres::dbReadTable(db, "region_cluster")
  
  if(nrow(region_cluster) <= 2){
    insert_data <- data.frame(
      name = unique(region_cluster_fabio$continent) %>% sort()
    )
    
    DBI::dbAppendTable(db, name = "region_cluster", value = insert_data)
    
    rm(insert_data)
    
    region_cluster <- RPostgres::dbReadTable(db, "region_cluster") 
  }
  
  ## now we join ids from the concordance tables
  # insert continent-clusters for FABIO regions
  insert_data <- region_cluster_fabio %>% 
    dplyr::left_join(region_cluster, by = c("continent" = "name")) %>% 
    dplyr::rename(id_region_cluster = id) %>% 
    dplyr::left_join(region_fabio[,c("id", "name")], by = c("name" = "name")) %>% 
    dplyr::rename(id_region = id) %>% 
    dplyr::select(id_region_cluster, id_region)
  # insert into db
  DBI::dbAppendTable(db, name = "region_cluster_region", value = insert_data)
  
  # insert continent-clusters for EXIOBASE regions
  insert_data <- region_cluster_exio %>% 
    dplyr::left_join(region_cluster, by = c("continent" = "name")) %>% 
    dplyr::rename(id_region_cluster = id) %>% 
    dplyr::left_join(region_exio[,c("id", "name")], by = c("EXIOBASE" = "name")) %>% 
    dplyr::rename(id_region = id) %>% 
    dplyr::select(id_region_cluster, id_region) %>% 
    dplyr::distinct()
  # insert into db
  DBI::dbAppendTable(db, name = "region_cluster_region", value = insert_data)
  
  rm(insert_data)
  
  region_cluster_region <- RPostgres::dbReadTable(db, "region_cluster_region")
}