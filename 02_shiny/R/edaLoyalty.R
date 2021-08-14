edaLoyaltyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Most popular Locations based on Loyalty Card Data")
      )
    )
  )
}

edaLoyaltyServer <- function(id) {
  spendingNetworkServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
    })
  }
}