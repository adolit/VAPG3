pathsMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Vehicle Paths and Points of Interest")
      ),
      
      column(width = 3,
             h5("Set Parameters of GPS Movement Data"),
             selectInput(ns("car_id"),
                         "Select Car ID(s):",
                         selected = "1",
                         multiple = TRUE,
                         choices = sort(unique(df_paths$CarID))),
             dateRangeInput(ns("date"),
                            label="Select Date:",
                            start  = "2014-01-06",
                            end    = "2014-01-19",
                            min    = "2014-01-06",
                            max    = "2014-01-19",
                            startview = "month",
                            separator = " to ",
                            format = "dd/m/yyyy"),
             sliderInput(ns("hour"),
                         "Select Hour of Day:",
                         min = 0,
                         max = 24,
                         value = c(0, 24)),
             #br(),
             #actionButton("sel_btn",h6(strong("Generate GPS Movement Map!"),
             #                         icon("hand-point-right")))
      ),
      
      column(width = 9,
             h5("Explore the daily routine of GASTech Employees based on their GPS tracking data"),
             p(em("Note that the interactive map takes some time to load")),
             tmapOutput(NS(id, "map"),
                        width = "100%", 
                        height = "750px")
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
               CarID %in%  input$car_id)
    })

    data_points <- reactive({
      gps_dots_selected <- df_pois %>%
        filter(date >= input$date[1],
               date <= input$date[2],
               hour >= input$hour[1],
               hour <= input$hour[2],
               CarID %in%  input$car_id) %>%
        group_by(date,CarID) %>%
        mutate(
          Duration = case_when(
            HoursDuration < 0.5 ~ "0 to 30 mins",
            HoursDuration < 1 ~ "30 mins to 1 hour",
            HoursDuration < 2 ~ "1 to 2 hrs",
            HoursDuration < 4 ~ "2 to 4 hrs",
            HoursDuration < 8 ~ "4 to 8 hrs",
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
               HoursDuration)
    })

    output$map <- renderTmap({
      req(data_paths())
      req(data_points())
      
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
                     style = "fixed",
                     breaks = c(seq(0, 24, by=4)),
                     palette = "YlOrRd",
                     lwd = 2) +
            tm_shape(gps_dots_selected) +
            tm_dots(col = "Duration",
                    style = "cont",
                    palette = "Blues",
                    size = .25,
                    border.col = 'black',
                    alpha = .5,
                    jitter = .2,
                    popup.vars = c("CarID",
                                   "FullName",
                                   "Department",
                                   "Title",
                                   "ArrivalTimestamp",
                                   "DepartureTimestamp",
                                   "HoursDuration")) 
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