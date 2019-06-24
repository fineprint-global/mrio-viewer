##########################################################################
### 5. E.rds - add environmental use (landuse and biomass)
##########################################################################

# start <- Sys.time()
# 
# for(year in year_range){
#   # year <- 2013 # temporarily just 2013
#   data <- read_file_function(sprintf(file_format, year, "E"))
# 
#   # get environmental use variables - make sure they match the format used in your data
#   env_factor$name_data <- paste0(toupper(substr(env_factor$name, 1, 1)),
#                                  substr(env_factor$name, 2, nchar(env_factor$name)))
# 
#   # prepare environmental data -> unselect unnecessary cols, add ids for region and product
#   env_data <- data %>%
#     dplyr::select(-Country.Code, -Item.Code, -Com.Code, -Group, -ID) %>%
#     # REGION: join table, add ID, remove unnecessary cols
#     dplyr::left_join(region, by = c("Country" = "name"), suffix = c("", ".region")) %>%
#     dplyr::rename("from_region" = "id") %>%
#     dplyr::select(-Country, -iso3, -geometry) %>%
#     # PRODUCT: join table, add ID, remove unnecessary cols
#     dplyr::left_join(product, by = c("Item" = "name"), suffix = c("", ".product")) %>%
#     dplyr::rename("from_product" = "id") %>%
#     dplyr::select(-Item, -product_unit, -product_group, -other_id) %>%
#     # add the year
#     dplyr::mutate(year = year)
# 
#   # loop through env_use_vars, for us this is landuse and biomass
#   for(i in length(row.names(env_factor))){
#     insert_data <- env_data %>%
#       dplyr::mutate(env_factor = env_factor[i,]$id) %>%
#       dplyr::rename("amount" = env_factor[i,]$name_data) %>%
#       dplyr::select(from_region, from_product, env_factor, year, amount)
# 
#     # append env_use with the amount for the environmental factor currently in loop
#     RPostgres::dbAppendTable(db, name = "env_use", value = insert_data)
#   }
# 
#   rm(data, env_data, insert_data)
# 
#   print(paste("Year", year, "/", year_range[length(year_range)]))
# }
# 
# print("5. Populating env_use took")
# print(Sys.time()-start)
# # Time difference of 28.80357 mins
# 
# # env_use <- RPostgres::dbReadTable(db, "env_use")
