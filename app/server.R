### server.R

## STRUCTURE
# 1. Sankey visualization
# 2. About

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
  
  # Trigger bookmarking with either button
  # observeEvent(input$bookmark, {
  #   session$doBookmark()
  # })
  
  ##############################################################################
  ### 1. Sankey visualization
  ##############################################################################
  
  # here, we render the sankey
  output$sankey_plot <- plotly::renderPlotly({
    req(input$from_region)
    req(input$from_product)
    req(input$year)
    req(input$allocation)
    req(input$top_n)
    
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
    # define the number of steps for the progress bar to reach 100%
    n_steps <- 7
    
    # get user inputs
    from_region <- region_fabio$id[region_fabio$name==input$from_region]
    from_product <- product_fabio$id[product_fabio$name==input$from_product]
    year <- input$year
    allocation <- allocation_conc$id[allocation_conc$name == input$allocation]
    
    env_factor <- input$env_factor
    
    top_n <- input$top_n
    
    agg_percent <- input$agg_percent
    
    # # uncomment this in case you want to execute parts of the code manually     
    # from_region <- region_fabio$id[region_fabio$name=="Brazil"]
    # from_product <- product_fabio$id[product_fabio$name=="Soyabeans"]
    # year <- 2010
    # allocation <- allocation_conc$id[allocation_conc$name == "price"]
    # env_factor <- "biomass"
    # top_n <- 5
    # agg_percent <- 0.01
    
    total_footprint <- 0
    
    ############################################################################
    # Calculate footprints
    ############################################################################
    
    if(env_factor == "product unit"){
      e <- 1
    } else {
      # 1. prepare extension, get environmental intensity
      env_intensity <- env_intensity_tbl %>% 
        dplyr::filter(from_region == !!from_region,
                      from_product == !!from_product,
                      year == !!year,
                      env_factor == !!env_factor_conc$id[env_factor_conc$name == env_factor]) %>% 
        dplyr::collect()
      
      e <- env_intensity$amount
      
      # is null? this means that our product is not a primary crop
      if(is.null(e)){
        
        # 1. pre-filter the env_intensity_tbl because we'll join it below
        env_intensity_tbl_filtered <- env_intensity_tbl %>% 
          dplyr::filter(year == !!year,
                        env_factor == !!env_factor_conc$id[env_factor_conc$name == env_factor])
        
        # 2. get IO leontief for this product because we need to aggregate
        #    the column of the primary crops (for each: amount * e)
        #    to get the environmental factor for our product.
        #    This is why we join the env_intensity_tbl_filtered, to get e
        e_io_leontief_env_int <- io_leontief_tbl %>% 
          # get all from_regions and from_products that feed into our region&product combo
          dplyr::filter(to_region == !!from_region,
                        to_product == !!from_product,
                        year == !!year,
                        allocation == !!allocation) %>% 
          # we are only interested in the regions and products
          dplyr::select(from_region, from_product, amount) %>% 
          # get the environmental impact for those products
          dplyr::left_join(env_intensity_tbl_filtered, 
                           by = c("from_region" = "from_region",
                                  "from_product" = "from_product"),
                           suffix = c("", ".env")) %>% 
          dplyr::filter(!is.na(amount),
                        !is.na(amount.env)) %>% 
          # calculate the environmental impact for our product
          # = (amount that feeds into it * amount.env)
          dplyr::mutate(e = amount * amount.env) %>% 
          dplyr::select(from_region, from_product, e) %>% 
          # here, just summarise because we are only interested in the sum of e
          dplyr::summarise(e = sum(e, na.rm = TRUE)) %>% 
          dplyr::collect()
        
        if(is.na(e_io_leontief_env_int$e)){
          return(no_data_plotly(sprintf("There is not enough data to display %s-based allocation for %s from %s (%.0f). You can select 'product unit' as env. factor.",
                                        allocation_conc$name[allocation_conc$id == allocation], 
                                        product_conc$name[product_conc$id == from_product], 
                                        region_conc$name[region_conc$id == from_region], 
                                        year)))
        }
        
        e <- e_io_leontief_env_int$e
      }
    }
    
    # FABIO --------------------------------------------------------------------
    
    progress$inc(1/n_steps, detail = "food data & calculating footprints") # update progress
    
    # 2. get io-leontief
    FABIO_io_leontief <- io_leontief_tbl %>% 
      dplyr::filter(from_region == !!from_region,
                    from_product == !!from_product,
                    year == !!year,
                    to_region %in% !!region_fabio$id,
                    allocation == !!allocation) %>% 
      dplyr::collect()
    
    new_from_regions <- unique(FABIO_io_leontief$to_region)
    new_products <- unique(FABIO_io_leontief$to_product)
    
    # known issues with the filter below: region-product combos
    # where the region does not receive one product but another are also included
    
    # 3. get final demand for FOOD
    FABIO_final_demand <- final_demand_tbl %>% 
      dplyr::filter(from_region %in% !!new_from_regions,
                    product %in% !!new_products,
                    year == !!year,
                    element %in% !!element_type$id[element_type$name.type == "Food"]) %>%
      dplyr::group_by(from_region, to_region, product) %>% 
      dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>%
      dplyr::collect()
    
    if(nrow(FABIO_final_demand) > 0){
      # 4. Footprints
      # comFP: multiply IO amount with final demand
      # and envFP: multiply with e
      result_fabio <- FABIO_io_leontief %>% 
        dplyr::left_join(FABIO_final_demand, 
                         by = c("to_region" = "from_region", "to_product" = "product"), 
                         suffix = c("_io", "_y")) %>% 
        dplyr::filter(!is.na(amount_io) &
                      !is.na(amount_y)) %>% 
        dplyr::select(from_region, from_product, to_region, to_product, to_region_y, amount_io, amount_y)
      
      result_fabio <- result_fabio %>% 
        dplyr::mutate(comFP = (amount_io * amount_y)) %>%
        dplyr::mutate(envFP = comFP * e) %>% 
        dplyr::ungroup() # we need this ungroup to be able to rbind the data afterwards
      
      result_fabio <- result_fabio %>% 
        dplyr::left_join(region_fabio[,c("id", "exio_id")], by = c("to_region" = "id")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(to_region = exio_id) %>% # recode FABIO to EXIO regions
        dplyr::select(-exio_id) %>%
        dplyr::left_join(region_fabio[,c("id", "exio_id")], by = c("to_region_y" = "id")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(to_region_y = exio_id) %>% # recode FABIO to EXIO regions
        dplyr::select(-exio_id) %>%
        dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>%
        dplyr::summarise(envFP = sum(envFP, na.rm = TRUE)) %>% 
        dplyr::ungroup()
    } else {
      result_fabio <- NULL
    }
    
    # EXIOBASE -----------------------------------------------------------------
    
    progress$inc(1/n_steps, detail = "nonfood data & calculating footprints") # update progress
    
    # 2. get io-leontief
    EXIO_io_leontief <- io_leontief_tbl %>% 
      dplyr::filter(from_region == !!from_region,
                    from_product == !!from_product,
                    year == !!year,
                    to_region %in% !!region_exio$id) %>% 
      dplyr::collect()
    
    new_from_regions <- unique(EXIO_io_leontief$to_region)
    new_products <- unique(EXIO_io_leontief$to_product)
    
    # known issues with the filter below: region-product combos
    # where the region does not receive one product but another are also included
    
    # 3. get final demand for NONFOOD
    EXIO_final_demand <- final_demand_tbl %>% 
      dplyr::filter(from_region %in% !!new_from_regions,
                    product %in% !!new_products,
                    year == !!year,
                    element %in% !!element_type$id[element_type$name.type == "Nonfood"]) %>%
      dplyr::collect()
    
    if(nrow(EXIO_final_demand) > 0){
      # 4. Footprints
      # comFP: multiply IO amount with final demand
      # and envFP: multiply with e
      result_exio <- EXIO_io_leontief %>% 
        dplyr::left_join(EXIO_final_demand, 
                         by = c("to_region" = "from_region", "to_product" = "product"), 
                         suffix = c("_io", "_y")) %>% 
        dplyr::filter(!is.na(amount_io) &
                      !is.na(amount_y)) %>% 
        dplyr::select(from_region, from_product, to_region, to_product, to_region_y, amount_io, amount_y)
      
      result_exio <- result_exio %>% 
        dplyr::mutate(comFP = (amount_io * amount_y)) %>%
        dplyr::mutate(envFP = comFP * e) %>% 
        dplyr::ungroup() %>%  # we need this ungroup to be able to rbind the data afterwards
        dplyr::select(from_region, from_product, to_region, to_product, to_region_y, envFP)
    } else {
      result_exio <- NULL
    }
    
    ############################################################################
    # Aggregating results
    ############################################################################
    
    progress$inc(1/n_steps, message = "Aggregating results", detail = "") # update progress
    
    results <- base::rbind(result_exio, result_fabio)
    
    # Now: let's aggregate
    # we only have one from_region and one from_product
    # we take the top 5 (or top_n if user input) to_regions
    # we take the top 5 (or top_n if user input) to_regions_y
    
    top_to_reg <- results %>% 
      dplyr::group_by(to_region) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T)) %>%
      dplyr::top_n(top_n, envFP)
    
    top_to_reg_y <- results %>% 
      dplyr::group_by(to_region_y) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T)) %>%
      dplyr::top_n(top_n, envFP)
    
    agg <- results %>%
      dplyr::mutate(to_region = if_else(to_region %in% top_to_reg$to_region, to_region, 192L)) %>% 
      dplyr::mutate(to_region_y = if_else(to_region_y %in% top_to_reg_y$to_region_y, to_region_y, 192L)) %>% 
      dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T))
    
    step1 <- agg %>% 
      dplyr::group_by(from_region, from_product, to_region) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T))
    
    total_footprint <- sum(step1$envFP)
    
    step2 <- agg %>% 
      dplyr::group_by(to_region, to_product, to_region_y) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T))
    
    # only aggregate if there is a agg_percent > 0 specified
    if(agg_percent > 0){
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
      dplyr::left_join(region_conc[,c("id", "name", "iso3")], by = c("region" = "id")) %>% 
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
    
    ## start node
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
      dplyr::group_by(region, iso3, product_agg, node_name, region_name) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = TRUE)) %>% 
      dplyr::mutate(step = 2) %>% 
      dplyr::mutate(id = sprintf("%.0f_%.0f", region, product_agg)) %>% 
      dplyr::rename(name = node_name, region_id = region) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(id, step, name, iso3, region_name, region_id)
    
    ## final consumption of product in:
    nodes_3 <- data.frame(
      id = unique(step2$to_region_y),
      step = 3 # 3 for end
    )
    
    nodes <- rbind(nodes_1, nodes_2, nodes_3)
    rm(nodes_1, nodes_2, nodes_3)
    nodes <- nodes %>% 
      dplyr::left_join(region_conc[,c("id", "name", "iso3")], by = c("id" = "id")) %>% 
      dplyr::mutate(region_id = id) 
      # we keep this extra column because the id is something different for
      # nodes_production_product
    
    # define the number of colors
    n_cols <- length(unique(nodes$name))
    
    ## generate colors and make sure that regions with the same name have the same color
    if(n_cols < 12){ # maximum number of colors in default palettes
      colors <- RColorBrewer::brewer.pal(n_cols, "Set3")
      # now we get the colors
      reg_colors <- data.frame(
        # we have this subset here for the special case of 2 regions
        # as otherwise there would be an error as RColorBrewer returns at least 3 colors
        color = colors[1:n_cols], 
        region = unique(nodes$name)
      )
      rm(colors)
    } else {
      # now we get the colors
      reg_colors <- data.frame(
        color = colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_cols),
        region = unique(nodes$name)
      )
    }
    
    nodes$color <- reg_colors$color[match(nodes$name, reg_colors$region)]
    
    nodes_production_product$color <- reg_colors$color[match(nodes_production_product$region_name, reg_colors$region)]
    
    nodes_production_product <- nodes_production_product %>% 
      dplyr::select(-region_name)
    
    nodes <- rbind(nodes, nodes_production_product)
    
    nodes$index <- c(0:(nrow(nodes)-1))
    
    # now we store the information about the index in step_production_product
    # to make matching easier
    step_production_product$index <- nodes$index[match(step_production_product$node_name, 
                                                       nodes$name)]
    
    # LINKS --------------------------------------------------------------------
    progress$inc(1/n_steps, message = "Preparing data for plotting", detail = "linking the nodes") # update progress
    
    step1 <- step1 %>% 
      dplyr::mutate(
        source = nodes[nodes$step == 0,]$index[match(from_region, nodes[nodes$step == 0,]$id)],
        target = nodes[nodes$step == 1,]$index[match(to_region, nodes[nodes$step == 1,]$id)]
      ) %>% 
      dplyr::mutate(color = if_else(from_product %in% product_fabio$id, "rgba(38, 166, 91, .3)", "rgba(149, 165, 166, .3)")) %>% 
      dplyr::left_join(product_conc[,c("id", "name", "product_group")], by = c("from_product" = "id"), suffix = c("", "_product")) %>% 
      dplyr::rename(product = name, amount = envFP) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(source, target, amount, product, color)
    
    step1_2 <- step_production_product %>% 
      dplyr::mutate(
        source = nodes[nodes$step == 1,]$index[match(region, nodes[nodes$step == 1,]$id)],
        target = index
      ) %>% 
      dplyr::mutate(color = if_else(from_product %in% product_fabio$id, "rgba(38, 166, 91, .3)", "rgba(149, 165, 166, .3)")) %>% 
      dplyr::left_join(product_conc[,c("id", "name", "product_group")], by = c("product" = "id"), suffix = c("", "_product")) %>% 
      dplyr::rename(product_id = product, product = name, amount = envFP) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(source, target, amount, product, color)
    
    step2 <- step2 %>% 
      dplyr::mutate(
        source = step_production_product$index[match(paste0(to_region, to_product), paste0(step_production_product$region,step_production_product$product))],
        target = nodes[nodes$step == 3,]$index[match(to_region_y, nodes[nodes$step == 3,]$id)]
      ) %>% 
      dplyr::mutate(color = if_else(to_product %in% product_fabio$id, "rgba(38, 166, 91, .3)", "rgba(149, 165, 166, .3)")) %>%
      dplyr::left_join(product_conc[,c("id", "name", "product_group")], by = c("to_product" = "id"), suffix = c("", "_product")) %>% 
      dplyr::rename(product = name, amount = envFP) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(source, target, amount, product, color)
    
    links <- rbind(step1, step1_2, step2)
    
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
    
    # SANKEY: NODES
    # create list of nodes
    node_list <- list(
      label = sprintf("%s (%.0f%%)", nodes$name, nodes$percent),
      color = nodes$color,
      pad = 15,
      thickness = 30,
      line = list(
        #color = colors$nodes[5],
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
      label = sprintf("<b>%s</b><br>%.2f %% of total", links$product, links$amount/sum(links$amount)*100)
    )
    
    progress$inc(1/n_steps, message = "Preparing plot", detail = "") # update progress

    p <- plotly::plot_ly(
      type = "sankey",
      orientation = "h", # alternative: v
      #valueformat = ".0f",
      valuesuffix = " (land footprint)",
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
        title = sprintf("%s-based allocation for %s from %s (%.0f) | %s footprint (total): %.2e %s", 
                        allocation_conc$name[allocation_conc$id == allocation], 
                        product_conc$name[product_conc$id == from_product], 
                        region_conc$name[region_conc$id == from_region], 
                        year,
                        env_factor,
                        sum(total_footprint),
                        if_else(env_factor %in% env_factor_conc$name,
                                env_factor_unit_conc$name[env_factor_unit_conc$id == 
                                                          env_factor_conc$env_factor_unit[env_factor_conc$name == env_factor]][1],
                                "product units")),
        # paper_bgcolor = "green",
        xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
        yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
      )
    
    # print out the time
    print(Sys.time() - time)
    
    progress$inc(1/n_steps, detail = "... now plotting...") # update progress
    
    p
  })
  
  ### Bookmarking: Read values from state$input when we restore, in order to call them in uiOutput
  # onRestore(function(state) {
  #   
  #   input$from_region <- state$input$from_region
  #   input$from_product <- state$input$from_product
  #   input$year <- state$input$year
  #   
  # })
  
  ##################################################################
  ### 2. Map
  ##################################################################
  
  output$map_title <- shiny::renderUI({
    req(input$sel_geo)
    h2("footprint of ", input$from_region, " and ", input$from_product)
  })
  
  output$map_plot <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(zoomDelta = 0.25, zoomSnap = 0, crs = leaflet::leafletCRS("L.CRS.EPSG4326"))) %>% 
      leaflet::setView(lng = -5, lat = 20, zoom = 1.75) %>% 
      leaflet.extras::setMapWidgetStyle(style = list(background = "#ccccff"))
  })
  # this is needed because otherwise no map is ever drawn
  # outputOptions(output, "consumption_map", suspendWhenHidden = FALSE)
  
  # leafletProxy for the map --------------------------------------------------------------------------
  counter <- 0
  observe({
    ### Load low-detail shape file for world map
    # world_shape = sf::st_as_sf(rnaturalearth::countries110) // the function call below is more efficient
    world_shape <- rnaturalearth::ne_countries(type = 'countries', scale = 'small', returnclass = "sf")
    
    ### remove Antarctica
    world_shape <- world_shape %>%
      dplyr::filter(admin != "Antarctica")
    
    start <- Sys.time()
    
    counter <- counter + 1
    
    chart_data <- flowdet_2013 %>%
      dplyr::filter(`destination` == !!geo_conc[geo_conc$name == input$sel_geo,]$code) %>% 
      dplyr::group_by(source) %>%
      dplyr::summarise(amount = sum(amount, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::collect() %>%
      dplyr::select(source, amount)
    # GRL (Greenland) is in the data with amount = 0
    
    chart_data <- chart_data %>%
      dplyr::right_join(., world_shape, by = c("source" = "gu_a3"))
    
    bins <- BAMMtools::getJenksBreaks(chart_data$amount, 6)
    nacol <- "#FFFFFF"
    # pal <- leaflet::colorBin(palette = "viridis", domain = chart_data$amount, bins = bins, reverse = TRUE, na.color = nacol)
    pal <- leaflet::colorBin(c("#d8b2b2", "#800000"), domain = chart_data$amount, bins = bins, na.color = nacol)
    
    chart_data <- chart_data %>% sf::st_as_sf()
    
    chart_data$label <- sprintf("%s (%d)<br>%s %s", chart_data$name, 2013, format(round(as.numeric(chart_data$amount)), nsmall=0, big.mark=","), ifelse(is.na(chart_data$amount), "", "tonnes")) %>% lapply(HTML)
    
    print(paste0("Before proxy: ", Sys.time() - start))
    
    round_digits <- ifelse(bins[2]/1e3 < 100, 0, -2)
    
    proxy <- leaflet::leafletProxy("consumption_map", data = chart_data) %>% 
      leaflet::addPolygons(
        group = paste("countries", counter, sep=""),
        weight = 0.5, #2
        opacity = 1,
        color = "white", # ch_country_profiles$color[ch_country_profiles$cat == "Consumption"],
        dashArray = "",
        fillColor = ~pal(amount),
        fillOpacity = 1,
        highlight = leaflet::highlightOptions(
          weight = 2,
          color = "#555",
          fillOpacity = 0.8,
          bringToFront = TRUE),
        label = ~label,
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      leaflet::clearGroup(paste("countries", (counter-1), sep="")) %>% # clear the previous country shapes
      leaflet::clearControls() %>%                                     # clear the previous legend
      leaflet::addLegend("bottomleft", pal = pal, values = ~amount,
                         title = NULL,
                         labFormat = leaflet::labelFormat(suffix = paste0(" ", "tonnes"), digits = round_digits, transform = divide_by_1k),
                         opacity = 0.7
      )
  })
  
  ##################################################################
  ### 3. About
  ##################################################################
  
  output$about_text <- shiny::renderText({
    "<strong>Lorem ipsum</strong> dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."
  })
  
  output$about_licence_text <- shiny::renderText({
    "This project gratefully acknowledges financial support from the ERC as part 
    of the <a href='https://www.fineprint.global/' target='_blank'>FINEPRINT</a> 
    project."
  })
  
}
