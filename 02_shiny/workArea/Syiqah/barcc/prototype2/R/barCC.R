# UI function

barCCUI <- function(id) {
  tagList(
    plotOutput(NS(id, "barcc"))
  )
}

# Server function

barCCServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$barcc <- renderPlot({
      df_cc %>%
      mutate(location = fct_rev(fct_infreq(location))) %>%
      ggplot(aes(x = location)) +
      geom_bar(colour = "grey", fill = "lightyellow4") +
      xlab("") +
      ylab("No. of credit card transactions") +
      theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) +
      coord_flip()
    })
  })
}