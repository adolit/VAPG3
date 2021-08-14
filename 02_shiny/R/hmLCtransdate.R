# UI function

hmLCtransdateUI <- function(id) {
  tagList(
    plotlyOutput(NS(id, "hmlctransdate"))
  )
}

# Server function

hmLCtransdateServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$hmlctransdate <- renderPlotly({
      lc_trans_date <- df_loyalty %>%
        count(loyaltynum, date) %>%
        mutate(loyaltynum = as.factor(loyaltynum),
               date = as.factor(date),
               text = paste0("Loyalty card no: ", loyaltynum, "\n", 
                             "Date: ", date, "\n", 
                             "Count: ", n))
      
      hMap_trans_by_date <- ggplot(lc_trans_date, aes(x = date,
                                                     y = loyaltynum,
                                                     fill = n,
                                                     text = text)) +
        geom_tile() +
        scale_fill_gradient(low = low_color, high = high_color) +
        scale_y_discrete() +
        scale_x_discrete() +
        xlab("Date") +
        theme(panel.grid.major = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 5),
              axis.title.y = element_blank())
      
      ggplotly(hMap_trans_by_date, tooltip = "text")
    })
  })
}