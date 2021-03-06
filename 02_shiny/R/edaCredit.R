edaCreditUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Exploratory Data Analysis based on Credit Card Data"),
             dateRangeInput(ns("date"),
                            label="Select Date of Transaction:",
                            start  = "2014-01-06",
                            end    = "2014-01-19",
                            min    = "2014-01-06",
                            max    = "2014-01-19",
                            startview = "month",
                            separator = " to ",
                            format = "dd/m/yyyy")
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
             h3("Transaction Values for Selected Location"),
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
          filter(date >= input$date[1],
                 date <= input$date[2]) %>%
          count(location) %>%
          plot_ly(x= ~n,
                  y= ~reorder(location,n),
                  type = 'bar',
                  marker = list(color = high_color),
                  hovertemplate = paste('Location: %{y}<br>',
                                        'Count: %{x}',
                                        '<extra></extra>')) %>%
          layout(yaxis = list(title = ""),
                 xaxis = list(title = "No. of credit card transactions"),
                 hoverlabel=list(bgcolor=bg_color))
      })
      
      output$hmccdate <- renderPlotly({
        edacc <- event_data("plotly_click")
        if (is.null(edacc)) return(NULL)
        
        hmcc <- df_cc %>%
          filter(date >= input$date[1],
                 date <= input$date[2]) %>%
          filter(location %in% edacc$y) %>%
          mutate(date = as.factor(date)) %>%
          group_by(last4ccnum, date) %>%
          summarise(total_price = sum(price)) %>%
          plot_ly(x= ~date,
                  y= ~last4ccnum,
                  z = ~total_price,
                  type = 'heatmap',
                  colors = colorRamp(c(low_color, high_color)),
                  hovertemplate = paste('Location: %{y}<br>',
                                        'Credit Card No: %{x}<br>',
                                        'Total Amount Spent: %{z}',
                                        '<extra></extra>')) %>%
          layout(title = paste(edacc$y, "Credit Card Transaction Values"),
                 xaxis = list(title = "Last 4 CC Numbers"),
                 yaxis = list(title = ""),
                 hoverlabel=list(bgcolor=bg_color))
      })
      
      output$hmcchour <- renderPlotly({
        
        hmhour <- df_cc %>%
          filter(date >= input$date[1],
                 date <= input$date[2]) %>%
          count(location, hour) %>%
          plot_ly(x= ~hour,
                  y= ~reorder(location, desc(location)),
                  z = ~n,
                  type = 'heatmap',
                  colors = colorRamp(c(low_color, high_color)),
                  hovertemplate = paste('Location: %{y}<br>',
                               'Hour of the day: %{x}<br>',
                               'Count: %{z}',
                               '<extra></extra>')) %>%
          layout(yaxis = list(title = ""),
                 xaxis = list(title = "Hours of the day"),
                 hoverlabel=list(bgcolor=bg_color))
      })
      
      output$boxplotcc <- renderPlotly({
        edacc <- event_data("plotly_click")
        if (is.null(edacc)) return(NULL)
        
        bp <- df_cc %>%
          filter(date >= input$date[1],
                 date <= input$date[2]) %>%
          filter(location %in% edacc$y) %>%
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
          layout(title = paste(edacc$y, "Transaction Price"),
                 yaxis = list(title = "Price"),
                 xaxis = list(title = ""),
                 hoverlabel=list(bgcolor=bg_color))
      })
      
    }) #moduleServer
}#edaCreditServer