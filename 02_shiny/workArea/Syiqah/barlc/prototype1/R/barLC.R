# UI function

barLCUI <- function(id) {
  tagList(
    plotOutput(NS(id, "barlc"))
  )
}

# Server function

barLCServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$barlc <- renderPlot({
      df_loyalty %>%
        mutate(location = fct_rev(fct_infreq(location))) %>%
        ggplot(aes(x = location)) +
        geom_bar(colour = "grey", fill = "lightyellow4") +
        xlab("") +
        ylab("No. of loyalty card transactions") +
        theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) +
        coord_flip()
    })
  })
}