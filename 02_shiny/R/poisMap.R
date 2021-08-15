poisMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Credit Card and Loyalty Number Owners"),
             h4("Infer the owners of credit card and loyalty card by adding the vehicle data"),
      ),
      
      column(width = 12,
             h3("Step 1:"),
             p("Explore the credit card transaction and take note of the timestamp"),
      ), 
      
      column(width = 6,
             h4("Credit Card"),
             DT::dataTableOutput(ns("cc_data_table"), width = "100%")
      ),
      
      column(width = 6,
             h4("Credit Card Spending Behaviour"),
             plotlyOutput(ns("hmcc_loc"), 
                          width = "100%", 
                          height = "800px")
      ),
      
    ),
    
    fluidRow(
      
      column(width = 12,
             h3("Step 2:"),
             p("Based on the credit card number and timestamp, use the date filter to narrow down the transactions.
               Use the 'Lasso Select' to focus on particular location which will then update the Interactive Map Table.")
      ), 
      
      column(width = 6,
             h4("Interactive Map"),
             fluidRow(
               column(width = 5,
                      dateRangeInput(ns("date"),
                                     label="Select Date:",
                                     start  = "2014-01-06",
                                     end    = "2014-01-19",
                                     min    = "2014-01-06",
                                     max    = "2014-01-19",
                                     startview = "month",
                                     separator = " to ",
                                     format = "dd/m/yyyy")
               ),
               column(width = 7,
                      selectInput(ns("car_id"),
                                  "Select Car ID(s):",
                                  selected = unique(df_gps$CarID),
                                  multiple = TRUE,
                                  width = "90%",
                                  choices = sort(unique(df_gps$CarID)))
               )
             ),
             plotlyOutput(ns("map"), height = 700)
      ),
      
      column(width = 6,
             h4("Interactive Map Table"),
             DT::dataTableOutput(NS(id, "map_data_table"), width = "100%")
      )
    ),

    fluidRow(
      column(width = 12,
             h3("Step 3:"),
             p("Use the table below to identify the best match Credit Card and Loyalty Card"),
      ),
      
      column(width = 6,
             h4("Loyalty Card"),
             DT::dataTableOutput(ns("cc_loyalty_table"), width = "100%")
      ),
      
      column(width = 6,
             h4("Credit Card and Loyalty Card Match"),
             plotlyOutput(ns("hmcclc_match"), 
                          width = "100%", 
                          height = "800px")
      ),
      
    )
  )
}

poisMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderPlotly({
      req(data_geo())
      
      m <- ggplot(data = data_geo()$data, mapping = aes(x = long, y = lat)) +
        geom_point() +
        xlim(24.82401, 24.90997) +
        ylim(36.04502, 36.09492) +
        theme_void()
      
      obj <- data_geo()$sel
      if(nrow(obj) != 0) {
        m <- m + geom_point(data = obj, color = "red", size = 2)
      }
      
      ax <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      m <- ggplotly(m, source= "master") %>%
        layout(
          xaxis = ax,
          yaxis = ax,
          images = list(
            source = base64enc::dataURI(file = "data/aspatial/MC2-tourist.jpg"),
            opacity = .7,
            x = 24.82401,
            y = 36.09492,
            sizex = 0.08596,
            sizey = 0.0499,
            xref = "x",
            yref = "y",
            sizing = "stretch"
          ),
          margin = list(t = 50)
        )
    })
    
    selected <- reactive({
      event_data("plotly_selected", source = "master")
    })
    
    data_geo <- reactive({
      gps_dots_selected <- df_pois %>%
        filter(ArrivalDate >= input$date[1],
               DepartureDate <= input$date[2],
               CarID %in% input$car_id)
      
      tmp <- gps_dots_selected
      
      sel <- tryCatch(gps_dots_selected[(selected()$pointNumber+1),,drop=FALSE] , error = function(e){NULL})
      
      list(data = tmp, sel = sel)
    })
    
    output$map_data_table <- DT::renderDataTable({
      d <- data_geo()$sel %>%
        filter(!is.na(CarID)) %>%
        select(CarID,
               ArrivalTimestamp,
               DepartureTimestamp,
               FullName,
               Department,
               Title)
      
      datatable(d,
                filter = 'top',
                options = list(pageLength = 10, autoWidth = TRUE),
                colnames = c("Car ID", "Arrival Timestamp",
                             "Departure Timestamp", "Name",
                             "Department", "Title")) %>%
        formatDate(columns = c(2, 3), method = 'toLocaleString', params = list(month = 'numeric',
                                                                               day = 'numeric',
                                                                               year = 'numeric',
                                                                               hour = 'numeric',
                                                                               minute = 'numeric'))
    })
    
    output$cc_data_table <- DT::renderDataTable({
      d <- df_cc %>%
        mutate(last4ccnum = as.character(last4ccnum)) %>%
        select(
          timestamp,
          location,
          last4ccnum,
          price
        )
      
      datatable(d,
                filter = 'top',
                options = list(pageLength = 10, autoWidth = TRUE),
                colnames = c("Timestamp", "Location", "Last 4 Numbers of CC", "Price")) %>%
        formatDate(1, method = 'toLocaleString', params = list(month = 'numeric',
                                                               day = 'numeric',
                                                               year = 'numeric',
                                                               hour = 'numeric',
                                                               minute = 'numeric'))
    })
    
    output$cc_loyalty_table <- DT::renderDataTable({
      d <- df_cc_loyalty %>%
        count(last4ccnum, loyaltynum) %>%
        group_by(last4ccnum) %>%
        mutate(last4ccnum = as.character(last4ccnum),
               pct = n / sum(n)) %>%
        select(
          last4ccnum,
          loyaltynum,
          pct
        )
      
      datatable(d,
                filter = 'top',
                options = list(pageLength = 10, autoWidth = TRUE),
                colnames = c("Last 4 Numbers of CC", "Loyalty Card Number", "Proportion of Transactions")) %>%
        formatPercentage(3, digits = 0)
    })
    
    
    output$hmcc_loc <- renderPlotly({
      hmcc <- df_cc %>%
        group_by(last4ccnum, location) %>%
        summarise(total_price = sum(price)) %>%
        plot_ly(x= ~last4ccnum,
                y= ~reorder(location, desc(location)),
                z = ~total_price,
                type = 'heatmap',
                colors = colorRamp(c(low_color, high_color)),
                hovertemplate = paste('Location: %{y}<br>',
                                      'Credit Card No: %{x}<br>',
                                      'Total Amount Spent: %{z}',
                                      '<extra></extra>')) %>%
        layout(xaxis = list(title = "Last 4 CC Numbers"),
               yaxis = list(title = ""),
               hoverlabel=list(bgcolor=bg_color))
    })
    
    
    output$hmcclc_match <- renderPlotly({
      hmmatch <- df_cclc_match %>%
        plot_ly(x= ~last4ccnum,
                y= ~loyaltynum,
                z = ~percent_match,
                type = 'heatmap',
                colors = colorRamp(c(low_color, high_color)),
                hovertemplate = paste('Loyalty Card No:: %{y}<br>',
                                      'Credit Card No: %{x}<br>',
                                      'Percent Match: %{z}(%)',
                                      '<extra></extra>')) %>%
        layout(xaxis = list(title = "Last 4 CC Numbers"),
               yaxis = list(title = "Loyalty Card Number"),
               hoverlabel=list(bgcolor=bg_color))
    })
  })
}