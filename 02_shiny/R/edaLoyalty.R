edaLoyaltyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Exploratory Data Analysis based on Loyalty Card Data"),
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
             h3("Suspected Outliers for All Locations"),
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
               xaxis = list(title = "No. of Loyalty card transactions"),
               hoverlabel=list(bgcolor=bg_color))
    })
    
    output$hmlcdate <- renderPlotly({
      edalc <- event_data("plotly_click")
      if (is.null(edalc)) return(NULL)
      
      hmlc <- df_loyalty %>%
        filter(date >= input$date[1],
               date <= input$date[2]) %>%
        filter(location %in% edalc$y) %>%
        count(loyaltynum, date) %>%
        plot_ly(x= ~date,
                y= ~loyaltynum,
                z = ~n,
                type = 'heatmap',
                colors = colorRamp(c(low_color, high_color)),
                hovertemplate = paste('Date of Transaction: %{x}<br>',
                                      'Loyalty card No: %{y}<br>',
                                      'Count: %{z}',
                                      '<extra></extra>')) %>%
        layout(title = paste(edalc$y, "Loyalty Card Transactions Frequency"),
               yaxis = list(title = "Loyalty Card Numbers"),
               xaxis = list(title = ""),
               hoverlabel=list(bgcolor=bg_color))
      
    })
    
    output$hmlcday <- renderPlotly({
      
      hmhour <- df_loyalty %>%
        filter(date >= input$date[1],
               date <= input$date[2]) %>%
        count(location, day) %>%
        plot_ly(x= ~day,
                y= ~reorder(location, desc(location)),
                z = ~n,
                type = 'heatmap',
                colors = colorRamp(c(low_color, high_color)),
                hovertemplate = paste('Location: %{y}<br>',
                                      'Day of the Week: %{x}<br>',
                                      'Count: %{z}',
                                      '<extra></extra>')) %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = "Day of the Week"),
               hoverlabel=list(bgcolor=bg_color))
    })
    
    output$boxplotlc <- renderPlotly({
      bp <- df_loyalty %>%
        filter(date >= input$date[1],
               date <= input$date[2]) %>%
        plot_ly(x = ~price,
                name = "Suspected Outliers",
                boxpoints = 'suspectedoutliers',
                marker = list(color = "indianred",
                              outliercolor = "darkred",
                              line = list(outliercolor = "darkred",
                                          outlierwidth = 10)),
                line = list(color = high_color),
                fillcolor  = list(color = low_color),
                alpha = 0.5) %>%
        add_boxplot(y = ~reorder(location, desc(location))) %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = ""),
               hoverlabel=list(bgcolor=bg_color))
    })
    
  })
}
