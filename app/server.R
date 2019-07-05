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
    
    plotly::plot_ly(mtcars, x = ~mpg, y = ~wt) %>% 
      plotly::layout(
        title = paste(input$input_from_region, 
                      "-", 
                      input$input_from_product,
                      input$input_year)
      )
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
