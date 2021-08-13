pathsMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Vehicle Paths and Points of Interest"),
    sidebarLayout(
      sidebarPanel(width = 2,
                   selectInput(ns("car_id"),
                               "Car ID",
                               selected = "1",
                               choices = sort(unique(df_paths$CarID))),
                   dateRangeInput(ns("date"),
                                  label="Select a date range:",
                                  start  = "2014-01-06",
                                  end    = "2014-01-06",
                                  min    = "2014-01-06",
                                  max    = "2014-01-19",
                                  startview = "month",
                                  separator = " to ",
                                  format = "dd/m/yyyy"),
                   sliderInput(ns("hour"),
                               "Hour",
                               min = 0,
                               max = 24,
                               value = c(0, 24))
      ),
      mainPanel(
        tmapOutput(NS(id, "map"), height = "800px")
      )
    )
  )
}

pathsMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data_paths <- reactive({
      df_paths %>%
        filter(date >= input$date[1],
               date <= input$date[2],
               hour >= input$hour[1],
               hour <= input$hour[2],
               CarID == input$car_id)
    })
    
    data_points <- reactive({
      gps_dots_selected <- df_pois %>%
        filter(date >= input$date[1],
               date <= input$date[2],
               hour >= input$hour[1],
               hour <= input$hour[2],
               CarID == input$car_id) %>%
        mutate(
          Duration = case_when(
            MinutesDuration < 30 ~ "0 to 30 mins",
            MinutesDuration < 60 ~ "30 mins to 1 hour",
            MinutesDuration < 60 * 2 ~ "1 to 2 hrs",
            MinutesDuration < 60 * 4 ~ "2 to 4 hrs",
            MinutesDuration < 60 * 8 ~ "4 to 8 hrs",
            TRUE ~ "8+ hrs"
          )
        ) %>%
        select(CarID,
               FullName,
               Department,
               Title,
               ArrivalTimestamp,
               DepartureTimestamp,
               Duration,
               MinutesDuration)
    })
    
    output$map <- renderTmap({
      req(data_paths())
      
      gps_paths_selected <- data_paths()
      gps_dots_selected <- data_points()
      
      tmap_mode("view")
      m <- tm_shape(bgmap) +
        tm_rgb(bgmap, r = 1,g = 2,b = 3,
               alpha = NA,
               saturation = 1,
               interpolate = TRUE,
               max.value = 255)
      
      tryCatch({
        if(nrow(gps_paths_selected) != 0) {
          m <- m +
            tm_shape(gps_paths_selected) +
            tm_lines(col = "hour",
                     lwd = 2) +
            tm_shape(gps_dots_selected) +
            tm_dots(col = "Duration",
                    palette = "Set1",
                    size = .15,
                    border.col = 'gray',
                    alpha = .5,
                    jitter = .2,
                    popup.vars = c("CarID",
                                   "FullName",
                                   "Department",
                                   "Title",
                                   "ArrivalTimestamp",
                                   "DepartureTimestamp",
                                   "MinutesDuration"))
        } else {
          stop()
        }
      }, warning = function(w) {
        showNotification('No vehicle movement. Please select another date or time range.','',type = "error")
        return()
      }, error = function(e) {
        showNotification('No vehicle movement. Please select another date or time range.','',type = "error")
        return()
      }, silent=TRUE)
      
      m
    })
  })
}