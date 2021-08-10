treeMapUI <- function(id) {
  tagList(
    fluidRow(
      column(width = 12,
             h3("Official Relationships of GASTech Employees")
      ),
      
      column(width = 8,
             h5("Click on the nodes to view the company organizational chart"),
             collapsibleTreeOutput(NS(id,"co_chart"), 
                                   width = "100%", 
                                   height = "1000px")
      ),
      
      column(width = 4,
             checkboxInput(NS(id, "showDetails"),
                           "Check to view Company Details",
                           value = TRUE),
             p("You can search the details by CarID, Department, Title or Name"),
             DT::dataTableOutput(NS(id,"co_table"),width = "100%")
      )
    )
  )
}


treeMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$co_chart <- renderCollapsibleTree({
      collapsibleTree(
        df_cars,
        hierarchy = c("Department", "Title","FullName"),
        inputId = "node",
        root = "GASTech Company",
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
    
    output$co_table <- DT::renderDataTable({
      if (input$showDetails) {
        DT::datatable(data = df_cars %>%
                        select("CarID", "Department", "Title", "FullName"),
                      options = list(
                        pageLength = 15,
                        search = list(regex = TRUE, caseInsensitive = TRUE, search = "Engineering")
                      ),
                      rownames = FALSE)
      }
    })
  })
}