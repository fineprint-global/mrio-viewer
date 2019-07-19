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
  
  ##################################################################
  ### 1. Sankey visualization
  ##################################################################
  
  # here, we render the sankey
  output$sankey_plot <- plotly::renderPlotly({
    req(input$from_region)
    req(input$from_product)
    req(input$year)
    req(input$allocation)
    
    time <- Sys.time()
    
    from_region <- region_fabio$id[region_fabio$name==input$from_region]
    from_product <- product_fabio$id[product_fabio$name==input$from_product]
    year <- input$year
    allocation <- allocation_conc$id[allocation_conc$name == input$allocation]
    
    # replace with input
    # from_region <- region_fabio$id[region_fabio$name=="Brazil"]
    # from_product <- product_fabio$id[product_fabio$name=="Soyabeans"]
    # year <- 2013
    # allocation <- allocation_conc$id[allocation_conc$name == "price"]
    
    env_factor <- input$env_factor
    
    ############################################################################
    # Calculate footprints
    ############################################################################
    
    # 1. prepare extension, get environmental intensity
    env_intensity <- env_intensity_tbl %>% 
      dplyr::filter(from_region == !!from_region,
                    from_product == !!from_product,
                    year == !!year,
                    env_factor == !!env_factor_conc$id[env_factor_conc$name == env_factor]) %>% 
      dplyr::collect()
    
    e <- env_intensity$amount
    
    if(is.null(e)){
      return(no_data_plotly(sprintf("There is not enough data to display %s-based allocation for %s from %s (%.0f)",
                                    allocation_conc$name[allocation_conc$id == allocation], 
                                    product_conc$name[product_conc$id == from_product], 
                                    region_conc$name[region_conc$id == from_region], 
                                    year)))
    }
    
    # FABIO --------------------------------------------------------------------
    
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
    
    # recode FABIO to EXIO regions
    FABIO_io_leontief <- FABIO_io_leontief %>% 
      dplyr::left_join(region_fabio[,c("id", "exio_id")], by = c("to_region" = "id")) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(to_region = exio_id) %>% # recode FABIO to EXIO regions
      dplyr::group_by(from_region, from_product, to_region, to_product) %>% 
      dplyr::summarise(amount = sum(amount, na.rm = TRUE))
    
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
    
    # recode FABIO to EXIO regions
    FABIO_final_demand <- FABIO_final_demand %>% 
      dplyr::left_join(region_fabio[,c("id", "exio_id")], by = c("from_region" = "id")) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(from_region = exio_id) %>% # recode FABIO to EXIO regions
      dplyr::select(-exio_id) %>% 
      dplyr::left_join(region_fabio[,c("id", "exio_id")], by = c("to_region" = "id")) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(to_region = exio_id) %>% # recode FABIO to EXIO regions
      dplyr::select(-exio_id) %>% 
      dplyr::group_by(from_region, product, to_region) %>% 
      dplyr::summarise(amount = sum(amount, na.rm = TRUE))
    
    # from_reg_product_rowsum <- FABIO_final_demand %>% 
    #   dplyr::group_by(from_region, product) %>% 
    #   dplyr::summarise(amount = sum(amount, na.rm = TRUE))
    
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
    
    # EXIOBASE -----------------------------------------------------------------
    
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
      dplyr::ungroup() # we need this ungroup to be able to rbind the data afterwards
    
    results <- base::rbind(result_exio, result_fabio)
    
    # Now: let's aggregate
    # we only have one from_region and one from_product
    # we take the top 5 to_regions
    # we take the top 5 to_regions_y
    
    top_to_reg <- results %>% 
      dplyr::group_by(to_region) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T)) %>%
      dplyr::top_n(5, envFP)
    
    top_to_reg_y <- results %>% 
      dplyr::group_by(to_region_y) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T)) %>%
      dplyr::top_n(5, envFP)
    
    agg <- results %>%
      dplyr::mutate(to_region = if_else(to_region %in% top_to_reg$to_region, to_region, 192L)) %>% 
      dplyr::mutate(to_region_y = if_else(to_region_y %in% top_to_reg_y$to_region_y, to_region_y, 192L)) %>% 
      dplyr::group_by(from_region, from_product, to_region, to_product, to_region_y) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T))
    
    step1 <- agg %>% 
      dplyr::group_by(from_region, from_product, to_region) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T))
    
    step2 <- agg %>% 
      dplyr::group_by(to_region, to_product, to_region_y) %>% 
      dplyr::summarise(envFP = sum(envFP, na.rm = T))
    
    # nodes --------------------------------------------------------------------
    nodes_1 <- data.frame(
      id = unique(step1$from_region),
      step = 0 # 0 for start
    )
    nodes_2 <- data.frame(
      id = unique(step1$to_region),
      step = 1 # 1 for middle
    )
    nodes_3 <- data.frame(
      id = unique(step2$to_region_y),
      step = 2 # 2 for end
    )
    
    nodes <- rbind(nodes_1, nodes_2, nodes_3)
    rm(nodes_1, nodes_2, nodes_3)
    nodes <- nodes %>% dplyr::left_join(region_conc[,c("id", "name", "iso3")], by = c("id" = "id"))
    
    ## generate colors and make sure that regions with the same name have the same color
    reg_colors <-
      with(nodes,
           data.frame(region = unique(nodes$name),
                      color = I(brewer.pal(length(unique(nodes$name)), name = 'Set3'))))
    
    nodes$color <- reg_colors$color[match(nodes$name, reg_colors$region)]
    
    nodes$index <- c(0:(nrow(nodes)-1))
    
    # links --------------------------------------------------------------------
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
    
    step2 <- step2 %>% 
      dplyr::mutate(
        source = nodes[nodes$step == 1,]$index[match(to_region, nodes[nodes$step == 1,]$id)],
        target = nodes[nodes$step == 2,]$index[match(to_region_y, nodes[nodes$step == 2,]$id)]
      ) %>% 
      dplyr::mutate(color = if_else(to_product %in% product_fabio$id, "rgba(38, 166, 91, .3)", "rgba(149, 165, 166, .3)")) %>%
      dplyr::left_join(product_conc[,c("id", "name", "product_group")], by = c("to_product" = "id"), suffix = c("", "_product")) %>% 
      dplyr::rename(product = name, amount = envFP) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(source, target, amount, product, color)
    
    links <- rbind(step1, step2)
    
    # SANKEY: NODES
    # create list of nodes
    node_list <- list(
      label = nodes$name,
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
        title = sprintf("%s-based allocation for %s from %s (%.0f) | %s footprint (total): %e ha", 
                        allocation_conc$name[allocation_conc$id == allocation], 
                        product_conc$name[product_conc$id == from_product], 
                        region_conc$name[region_conc$id == from_region], 
                        year,
                        env_factor,
                        sum(links$amount)),
        # paper_bgcolor = "green",
        xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
        yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
      )
    
    # print out the time
    print(Sys.time() - time)
    
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
  ### 2. About
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
