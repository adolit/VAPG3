# UI function

boxplotLCUI <- function(id) {
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
          plotlyOutput(NS(id, "boxplotlc"), height = "800px", width = "100%")
      )
  )
}

# Server function

boxplotLCServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    lc_location <- reactive({
      df_loyalty %>%
        filter(location == input$location)
      })

    output$boxplotlc <- renderPlotly({
      req(lc_location())
      lc_loc_selected <- lc_location()

      bPlot_lc <- plot_ly(data = lc_loc_selected,
                         x = ~price,
                         color = I("lightyellow4"),
                         alpha = 0.5,
                         boxpoints = 'Outliers') %>%
        
        add_boxplot(y = ~reorder(location, desc(location))) %>%
        
        layout(xaxis = list(title = "Price"),
               yaxis = list(title = ""))
      
      ggplotly(bPlot_lc, tooltip = "text")
    })
  })
}