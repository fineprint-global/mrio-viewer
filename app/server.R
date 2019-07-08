### server.R

## STRUCTURE
# 1. Sankey visualization
# 2. About

# Define server logic
server <- function(input, output, session) {
  
  # Trigger bookmarking with either button
  # observeEvent(input$bookmark, {
  #   session$doBookmark()
  # })
  
  ##################################################################
  ### 1. Sankey visualization
  ##################################################################
  
  # here, we render the sankey
  output$sankey_plot <- plotly::renderPlotly({
    req(input$input_from_region)
    req(input$input_from_product)
    req(input$input_year)
    req(input$input_allocation)
    
    time <- Sys.time()
    
    ############################################################################
    # example: Sankey for Brazilian Cattle Supply Chain                        #
    ############################################################################
    
    ############################################################################
    # Control Panel                                                            #
    ############################################################################
    
    # 1. IDN, Oil, palm fruit
    # 2. BRA, Soyabeans
    # 3. BRA, Cattle
    # price_or_mass <- c("price", "mass")[2]
    price_or_mass <- str_to_lower(input$input_allocation)
    
    ### Nr. determines country & commodity:
    # country <- c("IDN", "BRA")[ifelse(nr==1, 1, 2)]
    country <- ifelse(input$input_from_region == "Brazil", "BRA", "IDN")
    commodity <- input$input_from_product
      # c("Oil, palm fruit", "Soyabeans", "Cattle, Buffaloes")[nr]
    type <- "food"
    
    # for now, just assign the country, no matter what the input is
    if(commodity == "Oil, palm fruit"){
      country <- "IDN"
      nr <- 1
    } else {
      country <- "BRA"
      if(commodity == "Soyabeans"){
        nr <- 2
      } else {
        nr <- 3
      }
    }
    
    ### Aggregation settings
    aggr_from_region <- c(0.09, 0.1, 0.01)[nr]
    aggr_to_region <- c(0.1, 0.1, 0.01)[nr]
    aggr_item <- c(0.1, 0.07, 0.03)[nr]
    aggr_item_region <- c(0.05, 0.04, 0.01)[nr]
    
    ### Input file
    input_data <- read.csv(paste0("www/data/results_2013_", country,"_", commodity,"_", price_or_mass,".csv"), sep = ";", header = T, stringsAsFactors = F)
    
    ############################################################################
    # generate colors for the sankey-nodes - based on continent                #
    ############################################################################
    # colors based on country are generated near the end of the code
    
    regions <- read.csv2("www/data/regions_fabio.csv")

    ############################################################################
    # Data Preparation                                                         #
    ############################################################################
    
    # gather country-columns to separate rows
    data <- input_data %>%
      gather("to_region", "percentage_of_whole", 8:56)
    
    # change "region" to "from_region"
    colnames(data)[1] <- "from_region"
    
    # calculate land footprint going into each to_region
    data$value <- data$percentage_of_whole * data$landFP
    
    ########################################
    # AGGREGATE DATA
    
    total_sum <- sum(input_data$landFP)
    
    ## aggregate from-regions
    levels(data$from_region) <- c(levels(data$from_region), "Rest")
    
    for(from_region in unique(data$from_region)){
      # if smaller than 1% of total: change name to "Rest"
      if(sum(input_data$landFP[input_data$region == from_region]) < aggr_from_region*total_sum){
        data$from_region[data$from_region == from_region] <- "Rest"
      }
    }
    
    # data$from_region <- plyr::revalue(data$from_region, c("Venezuela (Bolivarian Republic of)" = "Venezuela", "C\xf4te d'Ivoire" = "Cote d'Ivoire", "China, mainland" = "China"))
    data$item <- plyr::revalue(data$item, c("Leather and leather products" = "Leather"))
    
    ## aggregate to-regions
    levels(data$to_region) <- c(levels(data$to_region), "Rest")
    
    for(to_region in unique(data$to_region)){
      # if smaller than 4% of total: change name to "Rest"
      if(sum(data$value[data$to_region == to_region]) < aggr_to_region*total_sum){
        data$to_region[data$to_region == to_region] <- "Rest"
      }
    }
    
    # group by from_region, item, and to_region
    data <- data %>% group_by(from_region, item, to_region, type) %>% summarize(value = sum(value))
    
    ## aggregate items
    levels(data$item) <- c(levels(data$item), "Other food", "Other nonfood")
    
    for(item in unique(data$item)){
      if(sum(data$value[data$item == item]) < aggr_item*total_sum){
        # "Other food" or "Other nonfood"
        data$item[data$item == item] <- paste("Other", data$type[data$item == item][1])
      }
    }
    
    # group by from_region, item, and to_region
    data <- data %>% group_by(from_region, item, to_region, type) %>% summarize(value = sum(value))
    
    
    data$item_region <- sprintf("%s (%s)", data$item, data$from_region)
    
    for(item_region in unique(data$item_region)){
      
      if(sum(data$value[data$item_region == item_region]) < aggr_item_region*total_sum){
        
        other_lvl <- sprintf("Other %s (%s)", data$type[data$item_region == item_region][1], data$from_region[data$item_region == item_region][1])
        
        if((any(other_lvl == levels(data$item_region)))){
          levels(data$item_region) <- c(levels(data$item_region), other_lvl)
        }
        
        data$item_region[data$item_region == item_region] <- other_lvl
      }
    }
    
    data <- data %>% group_by(from_region, item, to_region, item_region, type) %>% summarize(value = sum(value))
    
    for(from_region in unique(data$from_region)){
      if(
        # here we check if there is just 2 item_regions for this region and check if they are Other food and Other nonfood
        (length(unique(data$item_region[data$from_region == from_region])) == 2 &
         any(grepl("Other food", unique(data$item_region[data$from_region == from_region]))) &
         any(grepl("Other nonfood", unique(data$item_region[data$from_region == from_region])))) |
        # OR if there is just 1 item_region for this region, which is either Other food or Other nonfood
        (length(unique(data$item_region[data$from_region == from_region])) <= 1 &
         (any(grepl("Other food", unique(data$item_region[data$from_region == from_region]))) |
          any(grepl("Other nonfood", unique(data$item_region[data$from_region == from_region])))))){
        # then we rename it to *just* Food or Nonfood
        data$item_region[data$from_region == from_region & data$type == "food"] <- sprintf("%s (%s)", "Food", from_region)
        data$item_region[data$from_region == from_region & data$type == "nonfood"] <- sprintf("%s (%s)", "Nonfood", from_region)
      }
    }
    
    ############################################################################
    # Sankey Preparation                                                       #
    ############################################################################
    
    # create the node-names and their percentages of total that will be displayed
    data <- data %>% 
      # from_region_node
      group_by(from_region) %>% 
      dplyr::mutate(from_region_node = sprintf("%s (%.1f %%)" ,from_region, sum(value) / total_sum * 100)) %>%
      ungroup() %>%
      # item_region_node
      group_by(item_region) %>%
      dplyr::mutate(item_region_node = sprintf("%s (%.1f %%)" ,item_region, sum(value) / total_sum * 100)) %>%
      ungroup() %>%
      # to_region_node
      group_by(to_region) %>%
      dplyr::mutate(to_region_node = sprintf("%s (%.1f %%)" ,to_region, sum(value) / total_sum * 100)) %>%
      ungroup()
    
    # set link-colors for food (light green) and nonfood (light grey)
    data$color <- ifelse(data$type=="food", "rgba(38, 166, 91, .3)", "rgba(149, 165, 166, .3)")
    
    # create all nodes as factors
    data$from_region <- as.factor(data$from_region)
    data$to_region <- as.factor(data$to_region)
    data$item_region <- as.factor(data$item_region)
    data$from_region_node <- as.factor(data$from_region_node)
    data$to_region_node <- as.factor(data$to_region_node)
    data$item_region_node <- as.factor(data$item_region_node)
    
    from_regions <- levels(data$from_region_node)
    item_regions <- levels(data$item_region_node)
    to_regions <- levels(data$to_region_node)
    
    # for(i in c(1:length(item_regions))){
    #   item_region <- item_regions[i]
    #   
    #   if(sum(data$value[data$item_region == item_region]) < 0.005*total_sum){
    #     item_regions[i] <- ""
    #   }
    # }
    
    ##########################################################
    # generate colors for the sankey-nodes - based on country
    
    all_nodes <- data.frame(reg = c(country, levels(data$from_region), as.character(data$from_region[match(item_regions, data$item_region_node)]), levels(data$to_region)))
    all_nodes$reg[1] <- regions$Country[regions$ISO == country]
    
    unique_reg <- unique(all_nodes$reg)
    
    reg_colors <-
      with(all_nodes,
           data.frame(region = unique(all_nodes$reg),
                      color = I(brewer.pal(length(unique(all_nodes$reg)), name = 'Set3'))))
    
    all_nodes$color <- reg_colors$color[match(all_nodes$reg, reg_colors$region)]

    # SANKEY: NODES
    # create list of nodes
    node_list <- list(
      label = c(sprintf("%s (%s)", commodity, country), from_regions, item_regions, to_regions),
      color = all_nodes$color,
      pad = 15,
      thickness = 30,
      line = list(
        #color = colors$nodes[5],
        width = 0 # 0 width because it doesn't look good
      )
    )
    
    # SANKEY: LINKS
    
    # links for the very first connection
    start_data <- data %>% group_by(from_region) %>% summarize(value = sum(value))
    start_data$item <- commodity
    start_data$color <- ifelse(type=="food", "rgba(38, 166, 91, .3)", "rgba(149, 165, 166, .3)")
    start_source <- rep(1, nrow(start_data))
    
    link_list_df <- data.frame(
      # 1. e.g. BRA buffaloes), 2. initial from region, 3. from: item_region
      source = c(start_source, as.numeric(data$from_region)+1, as.numeric(data$item_region)+length(levels(data$from_region))+1)-1,
      # 1. initial from region,                2. item_region                                                   3. to_region
      target = c(as.numeric(start_data$from_region)+1, as.numeric(data$item_region)+length(levels(data$from_region))+1, as.numeric(data$to_region)+length(levels(data$from_region))+length(levels(data$item_region))+1)-1,
      value = c(start_data$value, data$value, data$value),
      color = c(start_data$color, data$color, data$color),
      item = c(start_data$item, data$item, data$item)
    )
    
    link_list_df <- link_list_df %>% group_by(source, target, color, item) %>% summarize(value = sum(value))
    
    # create the list of links
    link_list <- list(
      source = link_list_df$source,
      target = link_list_df$target,
      value = link_list_df$value,
      color = link_list_df$color,
      label = sprintf("<b>%s</b><br>%.2f %% of total", link_list_df$item, link_list_df$value/total_sum*100)
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
        title = sprintf("%s-based allocation for %s (%s) | land footprint (total): %e ha", price_or_mass, commodity, country, total_sum),
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
  #   input$input_from_region <- state$input$input_from_region
  #   input$input_from_product <- state$input$input_from_product
  #   input$input_year <- state$input$input_year
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
