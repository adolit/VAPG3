edaCombineUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Combined Credit Card and Loyalty Card Data")
      )
    )
  )
}

edaCombineServer <- function(id) {
  spendingNetworkServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
    })
  }
}