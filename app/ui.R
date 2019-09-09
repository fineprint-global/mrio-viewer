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
      title = "IO Visualization",
      selected = "Sankey",
      # inverse = TRUE,
      collapsible = TRUE,
      # theme = "bootstrap.css",
      theme = shinythemes::shinytheme("flatly"),
      
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
                           label = "From Region",
                           choices = region_fabio$name,
                           selected = "Brazil")
          ),
          column(
            width = 2,
            selectizeInput(inputId = "from_product",
                           label = "From Product",
                           choices = product_fabio$name,
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
                         label = "aggregate flows % smaller than",
                         value = 0.03,
                         min = 0,
                         max = 1,
                         step = 0.01)
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
          column( width = 12,
                  h1("About"),
                  # we use uiOutput for our renderText because otherwise HTML is not rendered  
                  uiOutput(outputId = "about_text"),
                  h1("Licence"),
                  uiOutput(outputId = "about_licence_text")
          )
        )
      )
    )
  )
}
