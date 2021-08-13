treeMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Official Relationships of GASTech Employees based on Company Organization Chart")
      ),
      
      column(width = 8,
             h5("Click on the nodes to view"),
             collapsibleTreeOutput(ns("co_chart"), 
                                   width = "100%", 
                                   height = "800px")
      ),
      
      column(width = 4,
             h5("Selected Nodes:"),
             p("Click the nodes to select"),
             verbatimTextOutput(ns("selected_node")),
             
             checkboxInput(ns( "showDetails"),
                           "Check to view Company Details",
                           value = TRUE),
             DT::dataTableOutput(ns("co_table"),width = "100%")
      )
    )
  )
}


treeMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$co_chart <- renderCollapsibleTree({
      collapsibleTree(
        df_cars,
        hierarchy = c("Department", "Title","FullName"),
        root = "GASTech Company",
        inputId = ns("node"),
        width = "auto",
        fill = c(
          #root color
          "tan",

          #company hierarchy color
          rep("navajowhite", length(unique(df_cars$Department))),
          rep("gold", length(unique(paste(df_cars$Department,
                                            df_cars$Title)))),
          rep("yellow", length(unique(paste(df_cars$Title,
                                                 df_cars$FullName))))
        ),
        nodeSize = "leafCount",
        tooltip = TRUE,
        fontSize = 13,
        collapsed = TRUE
      )
    })
    
    node <- reactive({
      input$node
    })
    
    output$selected_node <- renderPrint({
      node()
    })
    
    output$co_table <- DT::renderDataTable({
      if (input$showDetails & !is.null(node())) {
        str <- node()
        DT::datatable(data = df_cars %>%
                        select("CarID", "Department", "Title", "FullName"),
                      options = list(
                        pageLength = 10,
                        search = list(regex = TRUE, caseInsensitive = TRUE, search =str$Department[1])
                      ),
                      rownames = FALSE)
      }
    })
  })
}