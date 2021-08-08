# Functions

poisMapUI <- function(id) {
  tagList(
    fluidRow(
      column(width = 12,
             h3("Credit Card and Loyalty Number Owners")
      )
    ),
    fluidRow(
      column(width = 12,
             h4("Step 1")
      )
    ),
    fluidRow(
      column(width = 6,
             h4("Interactive Map"),
             fluidRow(
               column(width = 8,
                      selectInput(NS(id, "car_id"),
                                  "Car ID",
                                  selected = unique(df_gps$CarID),
                                  multiple = TRUE,
                                  width = "90%",
                                  choices = sort(unique(df_gps$CarID)))
               ),
               column(width = 4,
                      sliderInput(NS(id, "timestamp"),
                                  "Timestamp",
                                  min = as.POSIXct("2014-01-06 00:00:00"),
                                  max = as.POSIXct("2014-01-19 24:00:00"),
                                  value = c(as.POSIXct("2014-01-06 00:00:00"),
                                            as.POSIXct("2014-01-19 24:00:00")))
               )
             ),
             plotlyOutput(NS(id, "map"), height = 600)
      ),
      column(width = 6,
             h4("Interactive Map Table"),
             DT::dataTableOutput(NS(id, "data_table"))
      )
    ),
    fluidRow(
      column(width = 6,
             h4("Step 2")
      ),
      column(width = 6,
             h4("Step 3"))
    ),
    fluidRow(
      column(width = 6,
             h4("Credit Card")
      ),
      column(width = 6,
             h4("Loyalty Card")
      )
    )
  )
}

poisMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderPlotly({
      req(data())
      
      m <- ggplot(data = data()$data, mapping = aes(x = long, y = lat)) +
        geom_point() +
        xlim(24.82401, 24.90997) +
        ylim(36.04502, 36.09492) +
        theme_void()
      
      obj <- data()$sel
      # if(!is.null(obj)) {
      if(nrow(obj)!=0) {
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
    
    data <- reactive({
      gps_dots_selected <- as.data.frame(gps_dots_selected)
      
      gps_dots_selected <- gps_dots_selected %>%
        filter(ArrivalTimestamp >= input$timestamp[1],
               DepartureTimestamp <= input$timestamp[2],
               CarID %in% input$car_id)
      
      tmp <- gps_dots_selected
      
      sel <- tryCatch(gps_dots_selected[(selected()$pointNumber+1),,drop=FALSE] , error = function(e){NULL})
      
      list(data = tmp, sel = sel)
    })
    
    output$data_table <- DT::renderDataTable({
      d <- data()$sel %>%
        select(CarID,
               ArrivalTimestamp,
               DepartureTimestamp,
               FullName,
               Department,
               Title)
      
      datatable(d, filter = 'top', options = list(pageLength = 10, autoWidth = TRUE)) %>%
        formatDate(columns = c(2, 3), method = 'toLocaleString', params = list(month = 'numeric',
                                                                               day = 'numeric',
                                                                               year = 'numeric',
                                                                               hour = 'numeric',
                                                                               minute = 'numeric'))
    })
  })
}