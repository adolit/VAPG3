# UI function

heatmapCCUI <- function(id) {
  tagList(
    plotlyOutput(NS(id, "heatmapcc"))
  )
}

# Server function

heatmapCCServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$heatmapcc <- renderPlotly({
      cc_hour_of_day <- df_cc %>%
        count(location, hour) %>%
        mutate(location = as.factor(location),
               hour = as.factor(hour),
               text = paste("Location: ", location, "\n", 
                             "Hour of the day: ", hour, "\n", 
                             "Count: ", n))
      
      hMap_hour_of_day <- ggplot(cc_hour_of_day, aes(x=hour,
                                                     y=reorder(location, desc(location)),
                                                     fill = n,
                                                     text = text)) +
        geom_tile() +
        scale_fill_gradient(low = low_color, high = high_color) +
        scale_y_discrete() +
        scale_x_discrete() +
        xlab("Hour of the day") +
        theme(panel.grid.major = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 5),
              axis.title.y = element_blank())
      
      ggplotly(hMap_hour_of_day, tooltip = "text")
    })
  })
}