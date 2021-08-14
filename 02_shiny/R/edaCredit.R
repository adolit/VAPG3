edaCreditUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Exploratory Data Analysis based on Credit Card Data")
      ),
      
      column(width = 6,
             h3("Most Popular Locations by Frequency"),
             plotlyOutput(ns("barcc"), 
                          width = "100%", 
                          height = "800px")
             
      ),
      
      column(width = 6,
             h3("Credit Cards by Locations"),
             p("Click on barchart/heatmap location to show the credit card transaction"),
             plotlyOutput(ns("hmccdate"), 
                          width = "100%", 
                          height = "800px")
             
      ),
      
      column(width = 6,
             h3("Most Popular Locations by Hours of the Day"),
             plotlyOutput(ns("hmcchour"), 
                          width = "100%", 
                          height = "800px")
             
      ),
      
      column(width = 6,
             h3("Transaction Price by Locations"),
             p("Click on barchart/heatmap location to show the transaction price"),
             plotlyOutput(ns("boxplotcc"), 
                          width = "100%", 
                          height = "800px")
             
      )
    )
  ) #tagList
} #edaCreditUI

edaCreditServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      output$barcc <- renderPlotly({
        bar <- df_cc %>%
          count(location) %>%
          plot_ly(x= ~n,
                  y= ~reorder(location,n),
                  type = 'bar',
                  marker = list(color = high_color),
                  hovertemplate = paste('Location: %{y}<br>',
                                        'Count: %{x}')) %>% 
          layout(yaxis = list(title = ""),
                 xaxis = list(title = "No. of credit card transactions"))
      })
      
      output$hmccdate <- renderPlotly({
        d <- event_data("plotly_click")
        if (is.null(d)) return(NULL)
        
        hmcc <- df_cc %>%
          filter(location %in% d$y) %>%
          count(last4ccnum, date) %>%
          mutate(date = as.factor(date)) %>%
          plot_ly(x= ~date,
                  y= ~last4ccnum,
                  z = ~n,
                  type = 'heatmap',
                  colors = colorRamp(c(low_color, high_color)),
                  hovertemplate = paste('Date of Transaction: %{x}<br>',
                                        'Credit Card No: %{y}<br>',
                                        'Count: %{z}')) %>%
          layout(title = paste(d$y, "Credit Transactions"),
                 yaxis = list(title = "Last 4 CC Numbers"),
                 xaxis = list(title = "Date of Transaction"))
        
      })
      
      output$hmcchour <- renderPlotly({
        
        hmhour <- df_cc %>%
          count(location, hour) %>%
          plot_ly(x= ~hour,
                  y= ~reorder(location, desc(location)),
                  z = ~n,
                  type = 'heatmap',
                  colors = colorRamp(c(low_color, high_color)),
                  hovertemplate = paste('Location: %{y}<br>',
                               'Hour of the day: %{x}<br>',
                               'Count: %{z}')) %>%
          layout(yaxis = list(title = ""),
                 xaxis = list(title = "Hours of the day"))
      })
      
      output$boxplotcc <- renderPlotly({
        d <- event_data("plotly_click")
        if (is.null(d)) return(NULL)
        
        bp <- df_cc %>%
          filter(location %in% d$y) %>%
          plot_ly(y= ~price,
                  type = 'box',
                  boxpoints = "all",
                  jitter = 0.3,
                  marker = list(color = high_color),
                  line = list(color = high_color),
                  fillcolor  = list(color = low_color),
                  name = "All Points")
        bp %>% add_boxplot(y = ~price,
                           name = "Suspected Outliers",
                           boxpoints = 'suspectedoutliers',
                           marker = list(color = "indianred",
                                         outliercolor = "darkred",
                                         line = list(outliercolor = "darkred",
                                                     outlierwidth = 5)),
                           line = list(color = "indianred")) %>%
          layout(title = paste(d$y, "Transaction Price"),
                 yaxis = list(title = "Price"),
                 xaxis = list(title = ""))
      })
      
      
      
    }) #moduleServer
}#edaCreditServer