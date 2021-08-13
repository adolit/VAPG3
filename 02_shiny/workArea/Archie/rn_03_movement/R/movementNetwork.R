movementNetworkUI <- function(id) {
  tagList(
    fluidRow(
      column(width = 12,
             h3("Unofficial Relationship Networks based on GPS Movement Data")
      ),
      
      column(width = 12,
             h5("Text here 2"),
             collapsibleTreeOutput(NS(id,"co_chart"), 
                                   width = "100%", 
                                   height = "800px")
      )
    )
  )
}


movementNetworkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}