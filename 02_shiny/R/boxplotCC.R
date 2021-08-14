# UI function

boxplotCCUI <- function(id) {
  tagList(
    fluidRow(
      selectInput(NS(id, "location"),
                  "Location",
                  choices = sort(unique(df_loyalty$location)),
                  selected = NULL, # to set default to show all locations
                  multiple = FALSE, # to set to TRUE to allow multi location selection
                  width = NULL)
    ),
    fluidRow(
      plotlyOutput(NS(id, "boxplotcc"), height = "800px", width = "100%")
    )
  )
}

# Server function

boxplotCCServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    cc_location <- reactive({
      df_cc %>%
        filter(location == input$location)
    })
    
    output$boxplotcc <- renderPlotly({
      req(cc_location())
      cc_loc_selected <- cc_location()
      
      bPlot_cc <- plot_ly(data = cc_loc_selected,
                         x = ~price,
                         color = I("lightyellow4"),
                         alpha = 0.5,
                         boxpoints = 'Outliers') %>%
        
        add_boxplot(y = ~reorder(location, desc(location))) %>%
        
        layout(xaxis = list(title = "Price"),
               yaxis = list(title = ""))
      
      ggplotly(bPlot_cc, tooltip = "text")
    })
  })
}