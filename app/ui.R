### ui.R

## STRUCTURE
# 1. <HEAD> elements
# 2. Layout definitions
# 3. Sankey Visualization
# 4. About

# Define UI
ui <- function(request) {
  options(spinner.color="#2c3e50")
  
  tagList(
    ##################################################################
    ### 1. <HEAD> elements
    ##################################################################
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    ##################################################################
    ### 2. Layout definitions
    ##################################################################
    navbarPage(
      # Application title
      title = "FABIO Viewer",
      selected = "Sankey",
      # inverse = TRUE,
      collapsible = TRUE,
      # theme = "bootstrap.css",
      theme = shinythemes::shinytheme("flatly"),
      
      # add GitHub repository link with GitHub logo on the right side of navbar
      tags$script(HTML("var header = $('.navbar > .container-fluid > .navbar-collapse.collapse');
                       header.append('<div style=\"float:right\"><a href=\"https://github.com/fineprint-global/io-visualization\" target=\"_blank\"><img src=\"img/github-light.png\" style=\"float:right;height:41px;padding-top:17px;\"></a></div>');")
      ),
      # add About-Link as Menu Item
      tags$script(HTML("var navbar = $('.navbar > .container-fluid > .navbar-collapse.collapse > .navbar-nav');
                       navbar.append('<li><a href=\"//fineprint.global/resources/fabio\" target=\"_blank\">About</a></li>');")
      ),
      
      ##################################################################
      ### 3. Sankey visualization
      ##################################################################
      tabPanel(
        title = "Sankey",
        fluidRow(
          # ----------------------------------------------------------------
          # input elements -------------------------------------------------
          # ----------------------------------------------------------------
          column(
            width = 1,
            offset = 0,
            selectizeInput(inputId = "mode",
                           label = "Visualisation mode",
                           choices = modes,
                           selected = modes[1])
          ),
          # mode: normal direction
          conditionalPanel(condition = paste0("input.mode == '",modes[1],"'"),
            column(
              width = 2,
              selectizeInput(inputId = "from_region",
                             label = "Region of origin",
                             choices = list(
                               Region = region_fabio$name,
                               Cluster = continents$name.cluster
                             ),
                             selected = "Brazil")
            ),
            column(
              width = 2,
              selectizeInput(inputId = "from_product",
                             label = "Product of origin",
                             choices = product_dropdown,
                             selected = "Soyabeans")
            )
          ),
          # mode: reverse direction
          conditionalPanel(condition = paste0("input.mode == '",modes[2],"'"),
            column(
              width = 1,
              selectizeInput(inputId = "destination_mode",
                             label = "Use",
                             choices = dest_modes,
                             selected = dest_modes[1])
            ),
            conditionalPanel(
              condition = paste0("input.destination_mode == '",dest_modes[1],"'"),
              column(
                width = 1,
                selectizeInput(inputId = "to_region_y_food",
                               label = "Destination region",
                               choices = list(
                                 "Region" = region_fabio$name,
                                 "Cluster" = continents$name.cluster
                               ),
                               selected = "European Union")
              ),
              column(
                width = 2,
                selectizeInput(inputId = "to_product_food",
                               label = "Destination product",
                               choices = product_dropdown,
                               selected = "Bovine Meat")
              )
            ),
            conditionalPanel(
              condition = paste0("input.destination_mode == '",dest_modes[2],"'"),
              column(
                width = 1,
                selectizeInput(inputId = "to_region_y_nonfood",
                               label = "Destination region",
                               choices = list(
                                 "Region" = 
                                   # we sort regions and RoW regions separately
                                   c(region_exio$name[1:(nrow(region_exio)-5)] %>% 
                                       base::sort(),
                                     region_exio$name[(nrow(region_exio)-4):nrow(region_exio)] %>% 
                                       base::sort()),
                                 "Cluster" = continents$name.cluster
                               ),
                               selected = "European Union")
              ),
              column(
                width = 2,
                selectizeInput(inputId = "to_product_nonfood",
                               label = "Destination product",
                               choices = product_destination_dropdown["Non-food"],
                               selected = product_destination_dropdown[["Non-food"]][4])
              )
            )
          ),
          column(
            width = 1,
            sliderInput(inputId = "year", 
                        label = "Year", 
                        min = year_max_min$min,
                        max = year_max_min$max,
                        value = year_max_min$max,
                        step = 1,
                        sep = "")
          ),
          column(
            width = 1,
            selectizeInput(inputId = "allocation",
                           label = "Allocation",
                           choices = allocation_conc$name)
          ),
          column(
            width = 1,
            selectizeInput(inputId = "env_factor",
                           label = "Environmental factor",
                           choices = env_factor_conc$name,
                           selected = env_factor_conc[1,]$name)
          ),
          column(
            width = 2,
            div(
              class = "settings_small clearfix",
              div(class = "settings_label", "Settings"),
              numericInput(inputId = "top_n",
                           label = "regions shown separately",
                           value = 5,
                           min = 1,
                           max = 49,
                           step = 1),
              numericInput(inputId = "agg_percent",
                           label = "aggregate flows smaller than [%]",
                           value = 3,
                           min = 0,
                           max = 100,
                           step = 1)
            )
          ),
          # column(
          #   width = 1,
          #   numericInput(inputId = "top_n",
          #                label = "# regions to not aggregate",
          #                value = 5,
          #                min = 1,
          #                max = 49,
          #                step = 1)
          # ),
          # column(
          #   width = 1,
          #   numericInput(inputId = "agg_percent",
          #                label = "aggregate flows smaller than [%]",
          #                value = 3,
          #                min = 0,
          #                max = 100,
          #                step = 1)
          # ),
          column(
            width = 1,
            align = "center",
            # using action button instead of submitButton because of 
            actionButton(inputId = "run",
                         label = "Update",
                         icon = icon("refresh")
                         )
            # submitButton(text = "Update", # submit button removes reactivity (only reloads when clicked)
            #              icon = icon("refresh"))
          ),
          column(
            width = 1,
            bookmarkButton(label = "Share")
          )
        ),
        # ----------------------------------------------------------------
        # visualization --------------------------------------------------
        # ----------------------------------------------------------------
        fluidRow(
          plotly::plotlyOutput(outputId = "sankey_plot") %>% 
            shinycssloaders::withSpinner(size = 1.5)
        ),
        fluidRow(
          conditionalPanel(condition = "input.mode == 'origin'",
                           tags$ul(class = "flow-legend clearfix",
                                   tags$li(HTML(paste("<span>&nbsp;</span>", "Agricultural products"))),
                                   tags$li(HTML(paste("<span>&nbsp;</span>", "Nonfood products")))),
                           tags$ul(class = "description clearfix",
                                   tags$li("region & product of origin"),
                                   tags$li("(final) producing region"),
                                   tags$li("final product"),
                                   tags$li("consuming region"))),
          conditionalPanel(condition = "input.mode == 'destination'",
                           tags$ul(class = "flow-legend2 clearfix",
                                   tags$li(HTML(paste("<span>&nbsp;</span>", "Agricultural products"))),
                                   tags$li(HTML(paste("<span>&nbsp;</span>", "Nonfood products")))),
                           tags$ul(class = "description2 clearfix",
                                   tags$li("region of origin"),
                                   tags$li("(final) producing region"),
                                   tags$li("consuming region")))
        )
      )
    )
  )
}
