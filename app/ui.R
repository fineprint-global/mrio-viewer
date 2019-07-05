### ui.R

# Define UI
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  navbarPage(
    # Application title
    title = "IO Visualization",
    selected = "Sankey",
    # inverse = TRUE,
    collapsible = TRUE,
    # theme = "bootstrap.css",
    theme = shinythemes::shinytheme("flatly"),
    
    # Sankey Visualization
    tabPanel(
      title = "Sankey",
      fluidRow(
        column(
          width = 2,
          offset = 2,
          # here, we put the input elements
          selectizeInput(inputId = "input_from_region",
                         label = "From Region",
                         choices = c("Brazil", "Indonesia"))
        ),
        column(
          width = 2,
          selectizeInput(inputId = "input_from_product",
                         label = "From Product",
                         choices = c("Cattle", "Soy"))
        ),
        column(
          width = 3,
          sliderInput(inputId = "input_year", 
                      label = "Year", 
                      min = 1986,
                      max = 2013,
                      value = 2013,
                      step = 1,
                      sep = "")
        )
      ),
      fluidRow(
        plotly::plotlyOutput(outputId = "sankey_plot")
      )
    ),
    
    # About
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