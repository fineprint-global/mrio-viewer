### ui.R

## STRUCTURE
# 1. <HEAD> elements
# 2. Layout definitions
# 3. Sankey Visualization
# 4. About


# Define UI
ui <- function(request) {
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
            offset = 1,
            # here, we put the input elements
            selectizeInput(inputId = "input_from_region",
                           label = "From Region",
                           choices = region_fabio$name)
          ),
          column(
            width = 2,
            selectizeInput(inputId = "input_from_product",
                           label = "From Product",
                           choices = product_fabio$name)
          ),
          column(
            width = 2,
            sliderInput(inputId = "input_year", 
                        label = "Year", 
                        min = year_max_min$min,
                        max = year_max_min$max,
                        value = year_max_min$max,
                        step = 1,
                        sep = "")
          ),
          column(
            width = 2,
            selectizeInput(inputId = "input_allocation",
                           label = "Allocation",
                           choices = allocation_conc$name)
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
          plotly::plotlyOutput(outputId = "sankey_plot")
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
