# UI function

boxplotCCUI <- function(id) {
  tagList(
    plotlyOutput(NS(id, "boxplotcc"))
  )
}

# Server function

boxplotCCServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$boxplotcc <- renderPlotly({

      bPlot_cc <- plot_ly(data = df_cc,
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