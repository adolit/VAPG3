pathsMapUI <- function(id) {
  tagList(
    fluidRow(
      column(width = 12,
             h3("Vehicle Paths and Stops")
      )
    ),
    fluidRow(
      column(width = 6,
        fluidRow(
          column(width = 4,
                 selectInput(NS(id, "date"),
                             "Date",
                             choices = sort(unique(df_paths$date)))
          ),
          column(width = 4,
                 sliderInput(NS(id, "hour"),
                             "Hour",
                             min = 0,
                             max = 24,
                             value = c(0, 24))
          ),
          column(width = 4,
                 selectInput(NS(id, "car_id"),
                             "Car ID",
                             selected = "1",
                             choices = sort(unique(df_paths$CarID)))
          )
        )
      )
    ),
    fluidRow(),
    fluidRow(
      column(width = 6,
             tmapOutput(NS(id, "map"), height = 600)
      )
    )
  )
}

pathsMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data_paths <- reactive({
      x %>%
        filter(date == input$date,
               hour >= input$hour[1],
               hour <= input$hour[2],
               CarID == input$car_id)
    })
    
    output$map <- renderTmap({
      req(data_paths())
      
      gps_path_selected <- data_paths()
      
      tmap_mode("plot")
      m <- tm_shape(bgmap) +
        tm_rgb(bgmap, r = 1,g = 2,b = 3,
               alpha = NA,
               saturation = 1,
               interpolate = TRUE,
               max.value = 255)
      
      tryCatch({
        if(nrow(gps_path_selected) != 0) {
          m <- m +
            tm_shape(gps_path_selected) +
            tm_lines()
        } else {
          stop()
        }
      }, warning = function(w) {
        showNotification('there was a warning','',type = "error")
        return()
      }, error = function(e) {
        showNotification('there was an error','',type = "error")
        return()
      }, silent=TRUE)
      
      m
    })
  })
}