edaLoyaltyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Exploratory Data Analysis based on Loyalty Card Data")
      ),
      
      column(width = 6,
             h3("Most Popular Locations by Frequency"),
             plotlyOutput(ns("barlc"), 
                          width = "100%", 
                          height = "800px")
      ),
      
      column(width = 6,
             h3("Loyalty Cards by Locations"),
             p("Click on barchart/heatmap location to show the loyalty card transaction"),
             plotlyOutput(ns("hmlcdate"), 
                          width = "100%", 
                          height = "800px")
             
      ),
      
      column(width = 6,
             h3("Most Popular Locations by Day of the Week"),
             plotlyOutput(ns("hmlcday"), 
                          width = "100%", 
                          height = "800px")
             
      ),
      
      column(width = 6,
             h3("Suspected Outliers by Location"),
             plotlyOutput(ns("boxplotlc"), 
                          width = "100%", 
                          height = "800px")
             
      )
    )
  )
}

edaLoyaltyServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$barlc <- renderPlotly({
      bar <- df_loyalty %>%
        count(location) %>%
        plot_ly(x= ~n,
                y= ~reorder(location,n),
                type = 'bar',
                marker = list(color = high_color),
                hovertemplate = paste('Location: %{y}<br>',
                                      'Count: %{x}')) %>% 
        layout(yaxis = list(title = ""),
               xaxis = list(title = "No. of Loyalty card transactions"))
    })
    
    output$hmlcdate <- renderPlotly({
      d <- event_data("plotly_click")
      if (is.null(d)) return(NULL)
      
      hmcc <- df_loyalty %>%
        filter(location %in% d$y) %>%
        count(loyaltynum, date) %>%
        mutate(date = as.factor(date)) %>%
        plot_ly(x= ~date,
                y= ~loyaltynum,
                z = ~n,
                type = 'heatmap',
                colors = colorRamp(c(low_color, high_color)),
                hovertemplate = paste('Date of Transaction: %{x}<br>',
                                      'Credit Card No: %{y}<br>',
                                      'Count: %{z}')) %>%
        layout(title = paste(d$y, "Credit Transactions"),
               yaxis = list(title = "Loyalty Numbers"),
               xaxis = list(title = "Date of Transaction"))
      
    })
    
    output$hmlcday <- renderPlotly({
      
      hmhour <- df_loyalty %>%
        count(location, day) %>%
        plot_ly(x= ~day,
                y= ~reorder(location, desc(location)),
                z = ~n,
                type = 'heatmap',
                colors = colorRamp(c(low_color, high_color)),
                hovertemplate = paste('Location: %{y}<br>',
                                      'Day of the Week: %{x}<br>',
                                      'Count: %{z}')) %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = "Day of the Week"))
    })
    
    output$boxplotlc <- renderPlotly({
      bp <- df_loyalty %>%
        plot_ly(x = ~price,
                name = "Suspected Outliers",
                boxpoints = 'suspectedoutliers',
                marker = list(color = "indianred",
                              outliercolor = "darkred",
                              line = list(outliercolor = "darkred",
                                          outlierwidth = 5)),
                line = list(color = "indianred"),
                fillcolor  = list(color = "indianred"),
                alpha = 0.5) %>%
        add_boxplot(y = ~reorder(location, desc(location))) %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = ""))
    })
    
  })
}
