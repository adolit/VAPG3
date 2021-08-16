edaCombineUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Exploratory Data Analysis based on Combined Credit Card and Loyalty Card Data")
      ),
      
      column(width = 6,
             h3("Missing Credit Card Transaction"),
             plotlyOutput(ns("miss_cc"), 
                          width = "100%", 
                          height = "800px")
      ),
      
      column(width = 6,
             h3("Missing Loyalty Card Transaction"),
             plotlyOutput(ns("miss_lc"), 
                          width = "100%", 
                          height = "800px")
      ),
      
      column(width = 12,
             p("Click on barchart/heatmap location to show transactions"),
             checkboxInput(ns( "showDetails"),
                           "Check to view Transaction Details",
                           value = FALSE),
             DT::dataTableOutput(ns("cl_table"),width = "100%")
      )
    )
  )
}

edaCombineServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$miss_cc <- renderPlotly({
      hm_cc <- df_cc_loyalty %>%
        filter(is.na(last4ccnum)) %>%
        plot_ly(x= ~date,
                y= ~reorder(location, desc(location)),
                z = ~price,
                type = 'heatmap',
                colors = colorRamp(c(low_color, high_color)),
                hovertemplate = paste('Location: %{y}<br>',
                                      'Date: %{x}<br>',
                                      'Price: %{z}',
                                      '<extra></extra>')) %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = "Date of Transaction"),
               hoverlabel=list(bgcolor=bg_color))
    })
    
    output$miss_lc <- renderPlotly({
      hm_cc <- df_cc_loyalty %>%
        filter(is.na(loyaltynum)) %>%
        plot_ly(x= ~date,
                y= ~reorder(location, desc(location)),
                z = ~price,
                type = 'heatmap',
                colors = colorRamp(c(low_color, high_color)),
                hovertemplate = paste('Location: %{y}<br>',
                                      'Date: %{x}<br>',
                                      'Price: %{z}',
                                      '<extra></extra>')) %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = "Date of Transaction"),
               hoverlabel=list(bgcolor=bg_color))
    })

    output$cl_table <- DT::renderDataTable({
      
      edacl <- event_data("plotly_click")
      if (is.null(edacl)) return(NULL)
      
      if (input$showDetails & !is.null(edacl)) {
        DT::datatable(data = df_cc_loyalty %>%
                        select("date", "location", "last4ccnum", "loyaltynum", "price"),
                      options = list(
                        pageLength = 10,
                        search = list(regex = TRUE, caseInsensitive = TRUE, search = edacl$y)
                      ),
                      colnames = c("Date","Location", "Last 4 Numbers of CC", "Loyalty Card Number", "Amount Spent"),
                      rownames = FALSE) %>%
          formatDate(1, method = 'toLocaleString', params = list(month = 'numeric',
                                                                 day = 'numeric',
                                                                 year = 'numeric',
                                                                 hour = 'numeric',
                                                                 minute = 'numeric'))
      }
    })
    
  })
}