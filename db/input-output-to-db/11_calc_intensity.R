# Here we calculate the missing environmental intensities in ex-post

# ----------------------------------------------------------------
# preparation ----------------------------------------------------
# ----------------------------------------------------------------

# get other tables -----------------------------------------------
setwd("../../app")
source("global.R")

for(env_factor_id in env_factor_conc$id){
  for(allocation_id in allocation_conc$id){
    for(year in year_max_min$min:year_max_min$max){
      start <- Sys.time()
      
      io_filtered <- io_leontief_tbl %>% 
        dplyr::filter(year == !!year,
                      allocation == !!allocation_id)
      
      used_as_from <- io_filtered %>% 
        dplyr::group_by(from_region, from_product) %>% 
        dplyr::summarise() %>% 
        dplyr::collect()
      
      # 1. prepare extension, get environmental intensity
      env_intensity <- env_intensity_tbl %>% 
        dplyr::filter(from_region %in% !!used_as_from$from_region,
                      from_product %in% !!used_as_from$from_product,
                      year == !!year,
                      env_factor == !!env_factor_id) %>% 
        dplyr::select(-id, -year, -env_factor) %>% 
        dplyr::collect() %>% 
        # check if all region&product combinations are present in final demand
        dplyr::filter(paste(from_region,from_product) %in% paste(used_as_from$from_region, 
                                                                 used_as_from$from_product))
      e <- used_as_from %>% 
        dplyr::left_join(env_intensity, by = c("from_region", "from_product"))
      
      e_missing <- e %>% dplyr::filter(is.na(amount))
      
      e_io_leontief_env_int <- io_filtered %>% 
        # get all from_regions and from_products that feed into our region&product combo
        dplyr::filter(to_region %in% !!unique(e_missing$from_region),
                      to_product %in% !!unique(e_missing$from_product)) %>% 
        dplyr::select(from_region, from_product, to_region, to_product, amount) %>% 
        dplyr::collect() %>% 
        dplyr::filter(paste(to_region,to_product) %in% paste(e_missing$from_region, 
                                                             e_missing$from_product)) %>% 
        # now we remove entries with same from and to stats
        dplyr::filter(paste(from_region,from_product) != paste(to_region, to_product))
      
      # now we join the e
      e_io_leontief_env_int_new <- e_io_leontief_env_int %>% 
        dplyr::left_join(e, by = c("from_region", "from_product"), suffix = c("", ".e")) %>% 
        dplyr::mutate(e = amount * amount.e) %>% 
        dplyr::group_by(to_region, to_product) %>% 
        # we can remove NAs because they are not primary commodities
        # and this is already considered with the Leontief inverse
        dplyr::summarise(e = sum(e, na.rm = TRUE)) %>% 
        dplyr::rename(from_region = to_region,
                      from_product = to_product,
                      amount = e) %>% 
        dplyr::mutate(year = year,
                      allocation = allocation_id,
                      env_factor = env_factor_id)
      
      RPostgres::dbWriteTable(pool, 
                              name = "env_intensity_calculated", 
                              value = e_io_leontief_env_int_new,
                              append = TRUE)
      
      print(Sys.time()-start)
    }
  }
}

# get a db object from pool since the sendQuery does not support pool
db_temp <- poolCheckout(pool)
DBI::dbSendQuery(db_temp, "GRANT SELECT ON TABLE env_intensity_calculated TO app;")
poolReturn(db_temp)
