edaCreditUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Exploratory Data Analysis based on Credit Card Data")
      ),
      
      column(width = 6,
             h3("Most Popular Locations by Frequency"),
             plotlyOutput(ns("barCC"), 
                          width = "100%", 
                          height = "800px")
             
      ),
      
      column(width = 6,
             h3("Transactions by Locations"),
             p("Click on the bar chart to select location"),
             
      )
      
    )
  ) #tagList
} #edaCreditUI

edaCreditServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      output$barCC <- renderPlotly({
        bar <- df_cc %>%
          count(location) %>%
          plot_ly(x= ~n,
                  y= ~reorder(location,n),
                  type = 'bar',
                  marker = list(color = high_color)) %>% 
          layout(yaxis = list(title = ""),
                 xaxis = list(title = "No. of credit card transactions"))
      })
      
      
      
    }) #moduleServer
}#edaCreditServer