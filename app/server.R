### server.R

## STRUCTURE
# 0. Functions for calculations
# 1. Sankey visualization

# Define server logic
server <- function(input, output, session) {
  
  no_data_plotly <- function(message = "There is not enough data to display this plot."){
    p <- plotly::plotly_empty() %>% 
      plotly::layout(
        title = message,
        margin = list(l=75, t = 60, b=80)
      )
    return(p)
  }
  
  ##############################################################################
  ### 0. Functions for calculations
  ##############################################################################
  
  ## 0.1 mode 1 ----------------------------------------------------------------
  
  ## calculate environmental intensity
  calc_env_intensity <- function(env_factor, from_region, from_product, year, allocation){
  
    start <- Sys.time()
    
    # 1. prepare extension, get environmental intensity
    env_intensity <- env_intensity_tbl %>% 
      dplyr::filter(from_region == !!from_region,
                    from_product == !!from_product,
                    year == !!year,
                    env_factor == !!env_factor_conc$id[env_factor_conc$name == env_factor]) %>% 
      dplyr::collect()
    
    print("calc_env_intensity 1")
    print(Sys.time()-start)
    
    e <- ifelse(nrow(env_intensity) > 0, env_intensity[1,]$amount, NA)
    
    # is NA? this means that our product is not a primary crop
    if(is.na(e)){
      
      start <- Sys.time()
      
      e <- env_intensity_calculated_tbl %>% 
        dplyr::filter(year == !!year,
                      env_factor == !!env_factor_conc$id[env_factor_conc$name == env_factor],
                      from_region == !!from_region,
                      from_product == !!from_product,
                      allocation == !!allocation) %>% 
        dplyr::collect()
      
      e <- ifelse(nrow(e) > 0, e[1,]$amount, NA)
      
      print("calc_env_intensity 2")
      print(Sys.time()-start)
    }
    return(e)
  }
  
  ## calculate footprints for either Food (FABIO IO-Leontief and final demand) 
  ##                          or Nonfood products (EXIO IO-L and final demand)
  calc_footprint <- function(from_region, from_product, year, allocation, e, type = c("Food", "Nonfood")){
    type <- match.arg(type)
    
    reg_cluster <- switch(type,
                   Food = region_fabio,
                   Nonfood = region_exio)
    
    print("calc_footprint 1")
    start <- Sys.time()
    
    # 1. get io-leontief
    io_leontief <- io_leontief_tbl %>% 
      dplyr::filter(from_region == !!from_region,
                    from_product == !!from_product,
                    year == !!year,
                    to_region %in% !!reg_cluster$id,
                    allocation == !!allocation) %>% 
      dplyr::collect()
    
    print(Sys.time()-start)
    
    new_from_regions <- unique(io_leontief$to_region)
    new_products <- unique(io_leontief$to_product)
    
    # known issues with the filter below: region-product combos
    # where the region does not receive one product but another are also included
    
    # 2. get final demand
    final_demand <- final_demand_tbl %>%
      dplyr::filter(from_region %in% !!new_from_regions,
                    product %in% !!new_products,
                    year == !!year,
                    element %in% !!element_type$id[element_type$name.type == type]) %>%
      dplyr::group_by(from_region, to_region, product) %>%
      dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
      dplyr::collect()
    
    if(nrow(final_demand) > 0){
      # 3. footprints
      # comFP: multiply IO amount with final demand
      # and envFP: multiply with e
      result <- io_leontief %>% 
        dplyr::left_join(final_demand, 
                         by = c("to_region" = "from_region", "to_product" = "product"), 
                         suffix = c("_io", "_y")) %>% 
        dplyr::filter(!is.na(amount_io) &
                        !is.na(amount_y)) %>% 
        dplyr::select(from_region, from_product, to_region, to_product, to_region_y, amount_io, amount_y)
      
      result <- result %>% 
        dplyr::mutate(comFP = (amount_io * amount_y)) %>%
        dplyr::mutate(envFP = comFP * e) %>% 
        dplyr::ungroup() # we need this ungroup to be able to rbind the data afterwards
      
      # for type Food, which included FABIO regions up until now
      # we now aggregate those regions to match the EXIOBASE regions
      if(type == "Food"){
        result <- result %>% 
          dplyr::left_join(reg_cluster[,c("id", "exio_id")], by = c("to_region" = "id")) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(to_region = if_else(is.na(exio_id), 192L, exio_id)) %>% # recode FABIO to EXIO regions, except for RoW (id=192)
          dplyr::select(-exio_id) %>%
          dplyr::left_join(reg_cluster[,c("id", "exio_id")], by = c("to_region_y" = "id")) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(to_region_y = if_else(is.na(exio_id), 192L, exio_id)) %>% # recode FABIO to EXIO regions, except for RoW (id=192)
          dplyr::select(-exio_id) %>%
          dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>%
          dplyr::summarise(envFP = sum(envFP, na.rm = TRUE)) %>% 
          dplyr::ungroup() # we need this ungroup to be able to rbind the data afterwards
      } else {
        result <- result %>% 
          dplyr::select(from_region, from_product, to_region, to_product, to_region_y, envFP)
      }
    } else {
      result <- NULL
    }
    return(result)
  }
  
  calc_results_for_region <- function(from_region, from_product, year, allocation, env_factor){
    # get/calculate environmental intensity from database
    e <- calc_env_intensity(env_factor, from_region, from_product, year, allocation)
    
    if(is.na(e)){
      return()
    }
    
    # FABIO --------------------------------------------------------------------
    result_fabio <- calc_footprint(from_region, from_product, year, allocation, e, type = "Food")
    
    # EXIOBASE -----------------------------------------------------------------
    result_exio <- calc_footprint(from_region, from_product, year, allocation, e, type = "Nonfood")
    
    results <- base::rbind(result_exio, result_fabio)
    
    return(results)
  }
  
  ## 0.2 mode 2 ----------------------------------------------------------------
  
  calc_env_intensity_m2 <- function(env_factor, unique_combos, year){
    # 1. prepare extension, get environmental intensity
    env_intensity <- env_intensity_tbl %>% 
      dplyr::filter(from_region %in% !!unique_combos$from_region,
                    from_product %in% !!unique_combos$from_product,
                    year == !!year,
                    env_factor == !!env_factor_conc$id[env_factor_conc$name == env_factor]) %>% 
      dplyr::select(-id, -year, -env_factor) %>% 
      dplyr::collect() %>% 
      # check if all region&product combinations are present in final demand
      dplyr::filter(paste(from_region,from_product) %in% paste(unique_combos$from_region, 
                                                               unique_combos$from_product))
    
    e <- unique_combos %>% 
      dplyr::left_join(env_intensity, by = c("from_region", "from_product")) %>% 
      # change env impact to 0 as specified in the E matrix
      # since primary crop inputs are already accounted for with the L inverse
      dplyr::mutate(amount = ifelse(is.na(amount), 0, amount))
    
    return(e)
  }
  
  calc_results_m2 <- function(to_region_y, to_product, year, allocation, env_factor){
    
    # for the reverse mode, the order is reversed:
    # 1. get final demand
    # 2. check what columns in L inverse we need
    # 3. get the environmental intensity for those
    
    # 1. get final demand
    final_demand <- final_demand_tbl %>% 
      dplyr::filter(
        to_region %in% !!to_region_y &
          product %in% !!to_product &
          year == !!year
      ) %>% 
      # we could group by and thus omit the "element" col which we don't use
      dplyr::group_by(from_region, to_region, product, year) %>%
      dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
      dplyr::collect()
    
    if(nrow(final_demand) == 0){
      return(NULL)
    }
    
    # 2. get columns in L inverse we need
    # filtering separately and afterwards filtering again to have exact combo-matches
    # is by far the fastest way to get the info
    io_leontief <- io_leontief_tbl %>% 
      dplyr::filter(year == !!year,
                    to_region %in% !!final_demand$from_region,
                    to_product %in% !!to_product,
                    allocation == !!allocation) %>% 
      dplyr::select(-id) %>% 
      dplyr::collect() %>% 
      # check if all region&product combinations are present in final demand
      dplyr::filter(paste(to_region,to_product) %in% paste(final_demand$from_region, 
                                                           final_demand$product))
    # prepare results df containing almost all relevant info, only e is needed
    result <- io_leontief %>% 
      dplyr::left_join(final_demand, 
                       by = c("to_region" = "from_region", "to_product" = "product"), 
                       suffix = c("_io", "_y")) %>% 
      dplyr::filter(!is.na(amount_io) &
                      !is.na(amount_y)) %>% 
      dplyr::select(from_region, from_product, to_region, to_product, to_region_y, amount_io, amount_y)
    
    # 3. get the environmental intensity for every from_product&from_region combination
    unique_combos <- result %>% 
      dplyr::group_by(from_region, from_product) %>% 
      dplyr::summarise() %>% 
      dplyr::distinct()
    
    e <- calc_env_intensity_m2(env_factor, unique_combos, year)
    
    # 4. join e with result df to calculate footprints
    result <- result %>% 
      dplyr::left_join(e, by = c("from_region", "from_product")) %>% 
      dplyr::rename(amount_e = amount) %>% 
      dplyr::mutate(comFP = (amount_io * amount_y)) %>%
      dplyr::mutate(envFP = comFP * amount_e) %>% 
      dplyr::ungroup() # we need this ungroup to be able to rbind the data afterwards
    
    # # separate food and nonfood to adjust food-regions to EXIOBASE ones
    # # usually one of them has 0 rows, but that does not change our operations
    # food <- result %>% filter(to_region %in% region_fabio$id)
    # nonfood <- result %>% filter(to_region %in% region_exio$id)
    # 
    # food <- food %>% 
    #   dplyr::left_join(region_fabio[,c("id", "exio_id")], by = c("to_region" = "id")) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(to_region = if_else(is.na(exio_id), 192L, exio_id)) %>% # recode FABIO to EXIO regions, except for RoW (id=192)
    #   dplyr::select(-exio_id) %>%
    #   dplyr::left_join(region_fabio[,c("id", "exio_id")], by = c("to_region_y" = "id")) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(to_region_y = if_else(is.na(exio_id), 192L, exio_id)) %>% # recode FABIO to EXIO regions, except for RoW (id=192)
    #   dplyr::select(-exio_id) %>%
    #   dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>%
    #   dplyr::summarise(envFP = sum(envFP, na.rm = TRUE)) %>% 
    #   dplyr::ungroup() # we need this ungroup to be able to rbind the data afterwards
    # 
    # nonfood <- nonfood %>% 
    #   dplyr::select(from_region, from_product, to_region, to_product, to_region_y, envFP)
    # 
    # results <- base::rbind(food, nonfood)
    results <- result
    
    return(results)
  }
  
  # Trigger bookmarking with either button
  # observeEvent(input$bookmark, {
  #   session$doBookmark()
  # })
  
  ##############################################################################
  ### 1. Sankey visualization
  ##############################################################################
  
  # here, we render the sankey
  output$sankey_plot <- plotly::renderPlotly({
    # only run this if "update"-button is pressed
    input$run
    
    # the rest of the statement is in isolate() to prevent other inputs
    # from updating the plot
    isolate({
      req(input$year)
      req(input$allocation)
      req(input$env_factor)
      req(input$top_n)
      req(input$agg_percent)
      req(input$mode)
      
      time <- Sys.time()
      
      # create a Progress object
      progress <- shiny::Progress$new()
      # make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      # set the first message and provide more detail
      # message will be displayed as a bold text whereas 
      # detail displays more info in a normal font weight
      progress$set(message = "Fetching data from the database", 
                   detail = "environmental impact data", 
                   value = 0)
      
      # get user inputs
      mode <- input$mode
      
      if(mode == modes[1]){
        # is input-region just one region or a cluster?
        if(input$from_region %in% continents$name.cluster){ # is cluster
          cluster <- input$from_region
          from_regions <- (region_continent %>% 
                             dplyr::filter(name.cluster == cluster) %>% 
                             dplyr::select(id) %>% 
                             dplyr::collect())$id
          
          # only take regions in FABIO because there is no 
          # FROM product from an EXIO region
          from_regions <- from_regions[from_regions %in% region_fabio$id]
          
          # define the number of steps for the progress bar to reach 100%
          n_steps <- 5 + length(from_regions) # 5 "normal" steps + 1 per region
          
          cluster_mode <- TRUE
        } else{ # just one region
          # define the number of steps for the progress bar to reach 100%
          n_steps <- 6
          
          cluster_mode <- FALSE
        }
        
        from_region <- region_fabio$id[region_fabio$name==input$from_region]
        from_product <- product_fabio$id[product_fabio$name==input$from_product]
      } else { # mode == modes[2]
        
        if(input$destination_mode == dest_modes[1]){ # Food
          input_to_region_y <- input$to_region_y_food
          to_product <- product_conc$id[product_conc$name==input$to_product_food]
        } else { # Nonfood
          input_to_region_y <- input$to_region_y_nonfood
          to_product <- product_conc$id[product_conc$name==input$to_product_nonfood]
        }
        
        # is input-region just one region or a cluster?
        if(input_to_region_y %in% continents$name.cluster){ # is cluster
          cluster <- input_to_region_y
          to_region_y <- (region_continent %>% 
                             dplyr::filter(name.cluster == cluster) %>% 
                             dplyr::select(id) %>% 
                             dplyr::collect())$id
          
          # only take regions in EXIOBASE because those are the ones
          # for which we have final demand
          
          # to_region_y <- to_region_y[to_region_y %in% region_exio$id]
          
          cluster_mode <- TRUE
        } else{ # just one region
          cluster_mode <- FALSE
          
          to_region_y <- region_conc$id[region_conc$name==input_to_region_y]
        }
        # define the number of steps for the progress bar to reach 100%
        n_steps <- 6
      }
      
      year <- input$year
      allocation <- allocation_conc$id[allocation_conc$name == input$allocation]
      
      env_factor <- input$env_factor
      
      top_n <- input$top_n
      
      agg_percent <- input$agg_percent/100
      
      # # uncomment this in case you want to execute parts of the code manually     
      # from_region <- region_fabio$id[region_fabio$name=="Brazil"]
      # from_product <- product_fabio$id[product_fabio$name=="Soyabeans"]
      # year <- 2013
      # allocation <- allocation_conc$id[allocation_conc$name == "value"]
      # env_factor <- "biomass"
      # top_n <- 5
      # agg_percent <- 1
      
      total_footprint <- 0
      
      ############################################################################
      # Calculate footprints
      ############################################################################
      
      # the default query results are saved
      if(mode == modes[1] &&
         from_region %in% region_conc$id[region_conc$name=="Brazil"] &&
         from_product == product_conc$id[product_conc$name=="Soyabeans"]){
        results <- readRDS("results_BRA_Soy.rds")
      } else if(cluster_mode && mode == modes[1]){
        results <- NULL
        
        for(from_region in from_regions){
          
          progress$inc(1/n_steps, detail = sprintf("region %.0f/%.0f, calculating footprints", 
                                                   match(from_region, from_regions), # get position in list for current region
                                                   length(from_regions))) # update progress
          
          result_region <- calc_results_for_region(from_region, from_product, year, allocation, env_factor)
          
          if(!is.null(result_region)){
            if(is.null(results)){
              results <- result_region
            } else {
              results <- base::rbind(results, result_region)
            }
          }
        }
        
        # redefine region_conc for the current context
        region_conc <- region_continent %>% 
          dplyr::select(id_region_cluster, name.cluster) %>% 
          dplyr::distinct() %>% 
          dplyr::rename(id = id_region_cluster,
                        name = name.cluster)
        
        # make sure results are not NULL
        if(!is.null(results)){
          # aggregate regions
          # results <- results_to_region_cluster(results)
          # results$from_region <- region_continent$id_region_cluster[region_continent]
          results <- results %>% 
            dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>% 
            dplyr::summarise(envFP = sum(envFP, na.rm = TRUE)) %>% 
            dplyr::ungroup() %>% 
            # mutate from_region to cluster
            dplyr::left_join(region_continent[,c("id", "id_region_cluster")], by = c("from_region" = "id")) %>% 
            dplyr::mutate(from_region = id_region_cluster) %>% 
            dplyr::select(-id_region_cluster) %>% 
            # mutate to_region to cluster
            dplyr::left_join(region_continent[,c("id", "id_region_cluster")], by = c("to_region" = "id")) %>% 
            dplyr::mutate(to_region = id_region_cluster) %>% 
            dplyr::select(-id_region_cluster) %>% 
            # mutate to_region_y to cluster
            dplyr::left_join(region_continent[,c("id", "id_region_cluster")], by = c("to_region_y" = "id")) %>% 
            dplyr::mutate(to_region_y = id_region_cluster) %>% 
            dplyr::select(-id_region_cluster) %>%
            # now we summarize everything by cluster
            dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>% 
            dplyr::summarise(envFP = sum(envFP, na.rm = TRUE)) %>% 
            dplyr::ungroup()
        }
      } else if(mode == modes[1]) {
        progress$inc(1/n_steps, detail = paste(name_fabio, "&", name_exio, "data and calculating footprints")) # update progress
        
        results <- calc_results_for_region(from_region, from_product, year, allocation, env_factor)
      } else{ # mode 2
        progress$inc(1/n_steps, detail = paste(name_fabio, "&", name_exio, "data and calculating footprints")) # update progress
        
        results <- calc_results_m2(to_region_y, to_product, year, allocation, env_factor)
        
        if(cluster_mode){
          # redefine region_conc for the current context
          region_conc <- region_continent %>% 
            dplyr::select(id_region_cluster, name.cluster) %>% 
            dplyr::distinct() %>% 
            dplyr::rename(id = id_region_cluster,
                          name = name.cluster)
          
          # make sure results are not NULL
          if(!is.null(results)){
            # aggregate regions
            # results <- results_to_region_cluster(results)
            # results$from_region <- region_continent$id_region_cluster[region_continent]
            results <- results %>% 
              dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>% 
              dplyr::summarise(envFP = sum(envFP, na.rm = TRUE)) %>% 
              dplyr::ungroup() %>% 
              # mutate from_region to cluster
              dplyr::left_join(region_continent[,c("id", "id_region_cluster")], by = c("from_region" = "id")) %>% 
              dplyr::mutate(from_region = id_region_cluster) %>% 
              dplyr::select(-id_region_cluster) %>% 
              # mutate to_region to cluster
              dplyr::left_join(region_continent[,c("id", "id_region_cluster")], by = c("to_region" = "id")) %>% 
              dplyr::mutate(to_region = id_region_cluster) %>% 
              dplyr::select(-id_region_cluster) %>% 
              # mutate to_region_y to cluster
              dplyr::left_join(region_continent[,c("id", "id_region_cluster")], by = c("to_region_y" = "id")) %>% 
              dplyr::mutate(to_region_y = id_region_cluster) %>% 
              dplyr::select(-id_region_cluster) %>%
              # now we summarize everything by cluster
              dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>% 
              dplyr::summarise(envFP = sum(envFP, na.rm = TRUE)) %>% 
              dplyr::ungroup()
          }
        }
      }
      
      if(is.null(results)){
        if(mode == modes[1]){
          return(no_data_plotly(sprintf("There is not enough data to display %s-based allocation for %s from %s (%.0f).",
                                        allocation_conc$name[allocation_conc$id == allocation], 
                                        product_conc$name[product_conc$id == from_product], 
                                        region_fabio$name[region_fabio$id == from_region], 
                                        year)))
        } else {
          return(no_data_plotly(sprintf("There is not enough data to display %s-based allocation for %s from %s (%.0f).",
                                        allocation_conc$name[allocation_conc$id == allocation], 
                                        product_conc$name[product_conc$id == to_product], 
                                        input_to_region_y, 
                                        year)))
        }
      }
      
      ############################################################################
      # Aggregating results
      ############################################################################
      
      progress$inc(1/n_steps, message = "Aggregating results", detail = "") # update progress
      
      # Now: let's aggregate
      # we only have one from_region and one from_product
      # we take the top 5 (or top_n if user input) to_regions
      # we take the top 5 (or top_n if user input) to_regions_y
      
      if(!cluster_mode){
        # this is not necessary for mode=modes[1]
        if(mode == modes[2]){
          top_from_reg <- results %>% 
            dplyr::group_by(from_region) %>% 
            dplyr::summarise(envFP = sum(envFP, na.rm = T)) %>%
            dplyr::top_n(top_n, envFP)
        } else {
          top_from_reg <- results %>% dplyr::group_by(from_region) %>% dplyr::summarise()
        }
        
        top_to_reg <- results %>% 
          dplyr::group_by(to_region) %>% 
          dplyr::summarise(envFP = sum(envFP, na.rm = T)) %>%
          dplyr::top_n(top_n, envFP)
        
        # this is not necessary for mode=modes[2]
        if(mode == modes[1]){
          top_to_reg_y <- results %>%
            dplyr::group_by(to_region_y) %>%
            dplyr::summarise(envFP = sum(envFP, na.rm = T)) %>%
            dplyr::top_n(top_n, envFP)
        } else {
          top_to_reg_y <- results %>% dplyr::group_by(to_region_y) %>% dplyr::summarise()
        }
        
        agg <- results %>%
          dplyr::mutate(from_region = if_else(from_region %in% top_from_reg$from_region, from_region, 192L)) %>% 
          dplyr::mutate(to_region = if_else(to_region %in% top_to_reg$to_region, to_region, 192L)) %>% 
          dplyr::mutate(to_region_y = if_else(to_region_y %in% top_to_reg_y$to_region_y, to_region_y, 192L)) %>%
          dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>% 
          dplyr::summarise(envFP = sum(envFP, na.rm = T))
        
        rm(top_from_reg, top_to_reg, top_to_reg_y)
      } else {
        agg <- results
      }
      
      step1 <- agg %>% 
        dplyr::group_by(from_region, from_product, to_region) %>% 
        dplyr::summarise(envFP = sum(envFP, na.rm = T))
      
      total_footprint <- sum(step1$envFP)
      
      step2 <- agg %>% 
        dplyr::group_by(to_region, to_product, to_region_y) %>% 
        dplyr::summarise(envFP = sum(envFP, na.rm = T))
      
      # only aggregate if there is a agg_percent > 0 specified
      if(agg_percent > 0){
        # for step 1
        step1 <- step1 %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(from_product = if_else((envFP / total_footprint) > agg_percent,
                                               from_product,
                                               if_else(from_product %in% product_fabio$id,
                                                       product_other$food,
                                                       product_other$nonfood))) %>% 
          dplyr::group_by(from_region, from_product, to_region) %>% 
          dplyr::summarise(envFP = sum(envFP, na.rm = T))
        
        # for step 2
        step2 <- step2 %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(to_product = if_else((envFP / total_footprint) > agg_percent,
                                             to_product,
                                             if_else(to_product %in% product_fabio$id,
                                                     product_other$food,
                                                     product_other$nonfood))) %>% 
          dplyr::group_by(to_region, to_product, to_region_y) %>% 
          dplyr::summarise(envFP = sum(envFP, na.rm = T))
      }
      
      ## this is the third set of nodes: production country & top products
      step_production_product <- step2 %>% 
        dplyr::group_by(to_region, to_product) %>% 
        dplyr::rename(region = to_region, product = to_product) %>% 
        dplyr::summarise(envFP = sum(envFP, na.rm = T))
      
      # for now I commented out the top_n command here
      # this means that the user has the full control on how many nodes are displayed
      # and there is 1 node per link
      top_spp <- step_production_product %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(product != product_other$food &
                        product != product_other$nonfood) # %>% 
      # dplyr::top_n(5, envFP)
      
      # we only do the top_n if there is no aggregate specified to avoid overwhelmed browsers
      # plus the user does not get any info out of this setting otherwise because it's too much
      if(agg_percent <= 0){
        top_spp <- step_production_product %>% 
          dplyr::ungroup() %>% 
          dplyr::filter(product != product_other$food &
                          product != product_other$nonfood) %>% 
          dplyr::top_n(8, envFP)
      }
      
      ### if a product + region - combo is now in the top, the name will remain the same, 
      ### otherwise it will be renamed to Food/Nonfood (Region)
      
      # we add the product_type (food or nonfood) to the top_spp, too
      # because we need that info to determine if there is already a food for
      # that region that is displayed seperately, then we call the aggregate
      # "Other food", otherwise just "Food"
      top_spp <- top_spp %>% 
        dplyr::mutate(product_type = if_else(product %in% product_fabio$id, 
                                             product_other$food, 
                                             product_other$nonfood))
      
      # now we change the third step (production & product according to our new aggregate)
      step_production_product <- step_production_product %>% 
        dplyr::mutate(product_agg = 
                        if_else(paste0(region, product) %in% paste0(top_spp$region, top_spp$product), 
                                product, 
                                if_else(product %in% product_fabio$id, 
                                        product_other$food,
                                        product_other$nonfood))) %>%
        dplyr::left_join(region_conc[,c("id", "name")], by = c("region" = "id")) %>% 
        dplyr::rename(region_name = name) %>% 
        dplyr::left_join(product_conc[,c("id", "name", "product_group")], by = c("product_agg" = "id")) %>% 
        dplyr::rename(product_agg_name = name) %>% 
        dplyr::mutate(product_agg_name = if_else(!is.na(product_agg_name) & !grepl("aggregate", product_agg_name), 
                                                 product_agg_name,
                                                 if_else(paste0(region, product_agg) %in% 
                                                           paste0(top_spp$region, top_spp$product_type),
                                                         if_else(product_agg == product_other$food,
                                                                 "Other food", "Other nonfood"),
                                                         if_else(product_agg == product_other$food,
                                                                 "Food", "Nonfood")
                                                 ))) %>% 
        dplyr::mutate(node_name = sprintf("%s (%s)", product_agg_name, region_name))
      
      ############################################################################
      # Preparing nodes and links for the sankey
      ############################################################################
      
      # NODES --------------------------------------------------------------------
      progress$inc(1/n_steps, message = "Preparing data for plotting", detail = "preparing nodes") # update progress
      
      ## start nodes
      nodes_1 <- data.frame(
        id = unique(step1$from_region),
        step = 0 # 0 for start
      )
      ## raw product goes to country for production
      nodes_2 <- data.frame(
        id = unique(step1$to_region),
        step = 1 # 1 for countries receiving the raw product
      )
      ## these product are produced in the production country 
      ### different variable name is used because it is treated differently
      nodes_production_product <- step_production_product %>% 
        dplyr::group_by(region, product_agg, node_name, region_name) %>% 
        dplyr::summarise(envFP = sum(envFP, na.rm = TRUE)) %>% 
        dplyr::mutate(step = 2) %>% 
        dplyr::mutate(id = sprintf("%.0f_%.0f", region, product_agg)) %>% 
        dplyr::rename(name = node_name, region_id = region) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(id, step, name, region_name, region_id)
      
      ## final consumption of product in:
      nodes_3 <- data.frame(
        id = unique(step2$to_region_y),
        step = 3 # 3 for end
      )
      
      nodes <- rbind(nodes_1, nodes_2, nodes_3)
      rm(nodes_1, nodes_2, nodes_3)
      nodes <- nodes %>% 
        dplyr::left_join(region_conc[,c("id", "name")], by = c("id" = "id")) %>% 
        dplyr::mutate(region_id = id) 
      # we keep this extra column because the id is something different for
      # nodes_production_product
      
      # define the number of colors
      n_cols <- length(unique(nodes$name))
      
      ## generate colors and make sure that regions with the same name have the same color
      reg_colors <- data.frame(
        color = viridis::viridis_pal(alpha = .8)(n_cols), # 80% opacity
        region = unique(nodes$name) %>% sort()
        # sort region names to avoid ordering by appearance (would mean that countries closer in the list have similar colors)
      )
      rm(n_cols)
      
      nodes$color <- reg_colors$color[match(nodes$name, reg_colors$region)]
      
      nodes_production_product$color <- reg_colors$color[match(nodes_production_product$region_name, reg_colors$region)]
      
      nodes_production_product <- nodes_production_product %>% 
        dplyr::select(-region_name)
      
      if(mode == modes[1]){
        nodes$name[nodes$step == 0] <- sprintf("%s (%s)", 
                                               product_fabio$name[product_fabio$id==from_product], 
                                               input$from_region)
      } else if (mode == modes[2]){
        # nodes$name[nodes$step == 3] <- sprintf("%s (%s)", 
        #                                        product_conc$name[product_conc$id==to_product], 
        #                                        input$to_region)
      }
      
      nodes <- rbind(nodes, nodes_production_product)
      
      nodes$index <- c(0:(nrow(nodes)-1))
      
      # now we store the information about the index in step_production_product
      # to make matching easier
      step_production_product$index <- nodes[nodes$step > 0,]$index[match(step_production_product$node_name, 
                                                                          nodes[nodes$step > 0,]$name)]
      
      # LINKS --------------------------------------------------------------------
      progress$inc(1/n_steps, message = "Preparing data for plotting", detail = "linking the nodes") # update progress
      
      link_cols <- viridis::viridis_pal(alpha = .3, direction = -1, begin = 0, end = .7)(2)
      # [1] "#43BF714D" "#4401544D"
      
      color_link_food <- link_cols[1]
      color_link_nonfood <- link_cols[2]
      
      step1 <- step1 %>% 
        dplyr::mutate(
          source = nodes[nodes$step == 0,]$index[match(from_region, nodes[nodes$step == 0,]$id)],
          target = nodes[nodes$step == 1,]$index[match(to_region, nodes[nodes$step == 1,]$id)]
        ) %>% 
        dplyr::mutate(color = if_else(from_product %in% product_fabio$id, color_link_food, color_link_nonfood)) %>% 
        dplyr::left_join(product_conc[,c("id", "name", "product_group")], by = c("from_product" = "id"), suffix = c("", "_product")) %>% 
        dplyr::rename(product = name, amount = envFP) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(source, target, amount, product, color)
      
      step1_2 <- step_production_product %>% 
        dplyr::mutate(
          source = nodes[nodes$step == 1,]$index[match(region, nodes[nodes$step == 1,]$id)],
          target = index
        ) %>% 
        dplyr::mutate(color = if_else(product %in% product_fabio$id, color_link_food, color_link_nonfood)) %>% 
        dplyr::left_join(product_conc[,c("id", "name", "product_group")], by = c("product" = "id"), suffix = c("", "_product")) %>% 
        dplyr::rename(product_id = product, product = name, amount = envFP) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(source, target, amount, product, color)
      
      step2 <- step2 %>% 
        dplyr::mutate(
          source = step_production_product$index[match(paste0(to_region, to_product), paste0(step_production_product$region,step_production_product$product))],
          target = nodes[nodes$step == 3,]$index[match(to_region_y, nodes[nodes$step == 3,]$id)]
        ) %>% 
        dplyr::mutate(color = if_else(to_product %in% product_fabio$id, color_link_food, color_link_nonfood)) %>%
        dplyr::left_join(product_conc[,c("id", "name", "product_group")], by = c("to_product" = "id"), suffix = c("", "_product")) %>% 
        dplyr::rename(product = name, amount = envFP) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(source, target, amount, product, color)
      
      if(mode == modes[2]){
        final_node <- nodes[nodes$step == 3,]$index
        step1_2$target <- final_node
        links <- rbind(step1, step1_2)
        
        nodes <- nodes %>% dplyr::filter(step != 2)
      } else {
        links <- rbind(step1, step1_2, step2)
      }
      
      source_links <- links %>% 
        dplyr::group_by(source) %>% 
        dplyr::summarise(amount = sum(amount, na.rm = TRUE))
      
      target_links <- links %>% 
        dplyr::group_by(target) %>% 
        dplyr::summarise(amount = sum(amount, na.rm = TRUE))
      
      # NODES, again -------------------------------------------------------------
      nodes <- nodes %>% 
        dplyr::left_join(source_links, by = c("index" = "source")) %>% 
        dplyr::left_join(target_links, by = c("index" = "target"), suffix = c("", ".tgt")) %>% 
        dplyr::group_by(id, step) %>% 
        dplyr::mutate(amount = max(amount, amount.tgt, na.rm = TRUE)) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-amount.tgt) %>% 
        dplyr::mutate(percent = amount / total_footprint * 100)
      
      if (mode == modes[2]){
        nodes$name[nodes$step == 3] <- sprintf("%s (%s)",
                                               product_conc$name[product_conc$id==to_product],
                                               input_to_region_y)
      }
      
      # SANKEY: NODES
      # create list of nodes
      node_list <- list(
        label = if_else(nodes$percent > 0.5,
                        sprintf("%s (%.0f%%)", nodes$name, nodes$percent),
                        sprintf("%s (<0.5%%)", nodes$name)),
        color = nodes$color,
        pad = 15,
        thickness = 30,
        line = list(
          #color = nodes$color,
          width = 0 # 0 width because it doesn't look good
        )
      )
      
      # SANKEY: LINKS
      # create the list of links
      link_list <- list(
        source = links$source,
        target = links$target,
        value = links$amount,
        color = links$color,
        label = sprintf("<b>%s</b><br>%.2f %% of total", links$product, links$amount/total_footprint*100)
      )
      
      progress$inc(1/n_steps, message = "Preparing plot", detail = "") # update progress
      
      p <- plotly::plot_ly(
        type = "sankey",
        orientation = "h", # alternative: v
        # valueformat = ".0f",
        valuesuffix = if_else(env_factor %in% env_factor_conc$name,
                              sprintf(" (%s %s footprint)",
                                      gsub("/product unit", "", env_factor_unit_conc$name[env_factor_unit_conc$id == 
                                                                                            env_factor_conc$env_factor_unit[env_factor_conc$name == env_factor]][1]),
                                      env_factor),
                              "product units"),
        # iterations = 0,
        
        arrangement = "snap", # default: "snap"
        textfont = list(
          # family = ,
          size = 12,
          color = "black"
        ),
        
        node = node_list,
        
        link = link_list
      ) %>%
        plotly::layout(
          title = list(
            text = sprintf("total %s footprint: %.2e %s (%s-based allocation, %.0f)", 
                           env_factor,
                           sum(total_footprint),
                           if_else(env_factor %in% env_factor_conc$name,
                                   gsub("/product unit", "", env_factor_unit_conc$name[env_factor_unit_conc$id == 
                                                                                         env_factor_conc$env_factor_unit[env_factor_conc$name == env_factor]][1]),
                                   "product units"),
                           allocation_conc$name[allocation_conc$id == allocation], 
                           year),
            x = 0.035
          ),
          xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
          yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
        ) %>%
        plotly::config(displaylogo = FALSE)
      
      # print out the time
      print(Sys.time() - time)
      
      progress$inc(1/n_steps, detail = "... now plotting...") # update progress
      
      p
    })
  })
}
