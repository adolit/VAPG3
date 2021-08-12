# UI function

hmCCtransdateUI <- function(id) {
  tagList(
    plotlyOutput(NS(id, "hmcctransdate"))
  )
}

# Server function

hmCCtransdateServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$hmcctransdate <- renderPlotly({
      cc_trans_date <- df_cc %>%
        count(last4ccnum, date) %>%
        mutate(last4ccnum = as.factor(last4ccnum),
               date = as.factor(date),
               text = paste0("Credit card no: ", last4ccnum, "\n", 
                             "Date: ", date, "\n", 
                             "Count: ", n))
      
      hMap_trans_by_date <- ggplot(cc_trans_date, aes(x = date,
                                                     y = last4ccnum,
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