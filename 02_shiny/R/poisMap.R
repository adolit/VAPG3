poisMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Credit Card and Loyalty Number Owners"),
             h4("Step 1"),
             p("Add instructions")
      )
    ),
    fluidRow(
      column(width = 6,
             h4("Credit Card"),
             DT::dataTableOutput(ns("cc_data_table"), width = "100%")
      )
    ),
    fluidRow(
      column(width = 6,
             h4("Step 2"),
             p("Add instructions")
      )
    ),
    fluidRow(
      column(width = 6,
             h4("Interactive Map"),
             fluidRow(
               column(width = 4,
                      dateRangeInput(ns("date"),
                                     label="Select a date range:",
                                     start  = "2014-01-06",
                                     end    = "2014-01-19",
                                     min    = "2014-01-06",
                                     max    = "2014-01-19",
                                     startview = "month",
                                     separator = " to ",
                                     format = "dd/m/yyyy")
               ),
               column(width = 8,
                      selectInput(ns("car_id"),
                                  "Car ID",
                                  selected = unique(df_gps$CarID),
                                  multiple = TRUE,
                                  width = "90%",
                                  choices = sort(unique(df_gps$CarID)))
               )
             ),
             plotlyOutput(ns("map"), height = 600)
      ),
      column(width = 6,
             h4("Interactive Map Table"),
             DT::dataTableOutput(NS(id, "map_data_table"), width = "100%")
      )
    ),
    fluidRow(
      column(width = 6,
             h4("Step 3")
      )
    ),
    fluidRow(
      column(width = 6,
             p("Add instructions"),
             h4("Loyalty Card"),
             DT::dataTableOutput(ns("cc_loyalty_table"), width = "100%")
      )
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
            source = base64enc::dataURI(file = "data/Geospatial/MC2-tourist.jpg"),
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
      gps_dots_selected <- as.data.frame(gps_dots_selected)
      
      gps_dots_selected <- gps_dots_selected %>%
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
          last4ccnum
        )
      
      datatable(d,
                filter = 'top',
                options = list(pageLength = 10, autoWidth = TRUE),
                colnames = c("Timestamp", "Location", "Last 4 Numbers of CC")) %>%
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
  })
}