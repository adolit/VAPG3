spendingNetworkUI <- function(id) {
  tagList(
    fluidRow(
      column(width = 12,
             h3("Unofficial Relationship Networks based on Credit Card Data")
      ),
      
      column(width = 12,
             h5("Text here"),
             collapsibleTreeOutput(NS(id,"co_chart"), 
                                   width = "100%", 
                                   height = "800px")
      )
    )
  )
}


spendingNetworkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}