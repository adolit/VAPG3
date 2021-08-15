dtMatchUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Matching of Credit Card and Loyalty Card Owners"),
             p("The table below shows the proposed owners based on the analysis of vehicle data together with the credit and loyalty card data."),
             DT::dataTableOutput(ns("dt_match"),width = "100%")
      )
    )
  )
}

dtMatchServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$dt_match <- DT::renderDataTable({
      DT::datatable(df_cclc_owners, 
        colnames = c("Last 4 CC Numbers", "Loyalty Number", "Employee Name", "Deparment", "Title", "CarID"),
        options = list(pageLength = 55),
        rownames = FALSE)
    })
  })
}
