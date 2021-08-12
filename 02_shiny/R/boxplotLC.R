# UI function

boxplotLCUI <- function(id) {
  tagList(
    plotlyOutput(NS(id, "boxplotlc"))
  )
}

# Server function

boxplotLCServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$boxplotlc <- renderPlotly({

      bPlot_lc <- plot_ly(data = df_loyalty,
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