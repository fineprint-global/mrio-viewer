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
            width = 2,
            offset = 0,
            # here, we put the input elements
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
          ),
          column(
            width = 2,
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
            width = 2,
            selectizeInput(inputId = "env_factor",
                           label = "Environmental factor",
                           choices = c(env_factor_conc$name, "product unit"),
                           selected = env_factor_conc[1,]$name)
          ),
          column(
            width = 1,
            numericInput(inputId = "top_n",
                         label = "# regions to not aggregate",
                         value = 5,
                         min = 1,
                         max = 49,
                         step = 1)
          ),
          column(
            width = 1,
            numericInput(inputId = "agg_percent",
                         label = "aggregate flows smaller than [%]",
                         value = 3,
                         min = 0,
                         max = 100,
                         step = 1)
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
          tags$ul(class = "flow-legend clearfix",
                  # style = "position: absolute; bottom: 30px; list-style-type: none;", # "#43BF714D" "#4401544D"
                  tags$li(HTML("<span>&nbsp;</span> food")),
                  tags$li(HTML("<span>&nbsp;</span> nonfood"))),
          tags$ul(class = "description clearfix",
                  tags$li("from-region & from-product"),
                  tags$li("(final) producing region"),
                  tags$li("final product"),
                  tags$li("consuming region"))
        )
      ),
      
      ##################################################################
      ### 4. About
      ##################################################################
      tabPanel(
        title = "About",
        fluidRow(
          column( width = 6,
                  offset = 3,
                  includeMarkdown("about.md")
          )
        )
      )
    )
  )
}
