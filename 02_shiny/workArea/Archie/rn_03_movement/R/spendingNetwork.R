spendingNetworkUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Unofficial Relationship of GASTech Employees based on Credit Card Data")
      ),
      
      column(width = 2,
             h5("Set Parameters of Credit Card Data"),
             
             dateRangeInput(ns("date"),
                            label="Select Date of Transaction:",
                            start  = "2014-01-06",
                            end    = "2014-01-19",
                            min    = "2014-01-06",
                            max    = "2014-01-19",
                            startview = "month",
                            separator = " to ",
                            format = "dd/m/yyyy"),
             
             sliderInput(ns("hour"), 
                            "Select Hour:", 
                            min = 0, 
                            max = 24, 
                            value = c(0,24), 
                            step = 1),
             
             selectInput(ns("day"),
                            "Select Day of the Week:",
                         choices =  list("Mon",
                                         "Tue",
                                         "Wed",
                                         "Thu",
                                         "Fri",
                                         "Sat",
                                         "Sun"),
                         multiple = TRUE,
                         selected = c("Mon",
                                      "Tue",
                                      "Wed",
                                      "Thu",
                                      "Fri",
                                      "Sat",
                                      "Sun")
                         ),
             
             h5("Select GASTech Employee Deparment"),
             selectInput(ns("deparment"),
                            "Select Deparment:",
                            choices =  list("Executive",
                                            "Engineering",
                                            "Facilities",
                                            "Information Technology",
                                            "Security"),
                            multiple = TRUE,
                            selected = c("Executive",
                                         "Engineering",
                                         "Facilities",
                                         "Information Technology",
                                         "Security"),
                            ),
             
             h5("Select Network Layout"),
             selectInput(ns("layout"),
                         "Select Network Layout:",
                         choices =  list("layout_as_star",
                                         "layout_in_circle",
                                         "layout_nicely",
                                         "layout_on_grid",
                                         "layout_on_sphere",
                                         "layout_with_dh",
                                         "layout_with_drl",
                                         "layout_with_fr",
                                         "layout_with_gem",
                                         "layout_with_graphopt",
                                         "layout_with_kk",
                                         "layout_with_lgl",
                                         "layout_with_mds"),
                         selected = "layout_with_fr"
             ),
      ),
      
      column(width = 7,
             h5("Explore the Relationship of GASTech Employees according to Purchase Location"),
             visNetworkOutput(ns("cc_network"), 
                                   width = "100%", 
                                   height = "800px")
      ),
      
      column(width = 3,
             h5("Selected Nodes:"),
             p("Click the nodes to select"),
             verbatimTextOutput(ns("selected_node")),
             
             checkboxInput(ns( "showDetails"),
                           "Check to view Transaction Details",
                           value = TRUE),
             DT::dataTableOutput(ns("cc_table"),width = "100%")
      )
    )
  )
}


spendingNetworkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    df_cc_network <- reactive({
      df_cc %>% 
      filter(date >= input$date[1] &
               date <= input$date[2]) %>%
      filter(hour >= input$hour[1] &
               hour <= input$hour[2]) %>%
      filter(day %in% input$day)
    })
    
    output$cc_network <- renderVisNetwork({

      cc_edges <- df_cc_network() %>%
        group_by(last4ccnum, location, date, hour) %>%
        mutate(from = last4ccnum) %>%
        mutate(to = location) %>%
        ungroup() %>%
        group_by(from, to) %>%
        summarise(weight = n()) 
      
      sources <- df_cc_network() %>%
        distinct(last4ccnum)  %>%
        rename(label = last4ccnum) %>%
        mutate(group = "GAStech Employee")
      
      destinations <- df_cc_network() %>%
        distinct(location)  %>%
        rename(label = location) %>%
        mutate(group = "Location")
      
      cc_nodes <- full_join(sources,
                            destinations,
                            by = c("label" ="label", 
                                   "group" = "group")) %>%
        rename(id = label) %>%
        arrange(id)
      
      visNetwork(cc_nodes,
                 cc_edges) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE) %>%
        visIgraphLayout(layout = input$layout) %>%
        visEdges(arrows = "to",
                 smooth = list(enabled = TRUE,
                               type = "curvedCW")) %>%
        visGroups(groupname = "GAStech Employee", shape = "icon",
                  icon = list(code = "f007", size = 40, color = "slategray")) %>%
        visGroups(groupname = "Location", shape = "icon",
                  icon = list(code = "f041", size = 100, color = "goldenrod")) %>%
        addFontAwesome() %>%
        visLayout(randomSeed = 123) %>%
        #Use visEvents to turn set input$current_node_selection to list of selected nodes
        visEvents(select=paste0("function(ng_nodes){
                Shiny.onInputChange('",ns('current_node_selection'),"',ng_nodes.nodes[0]);
                             }"))
    })
    
    node <- reactive({
      input$current_node_selection
    })
    
    output$selected_node <- renderPrint({
      node()
    })
    
    output$cc_table <- DT::renderDataTable({
      
      if (input$showDetails & !is.null(node())) {
        DT::datatable(df_cc_network() %>%
                        select(timestamp, day, location, last4ccnum, price),
                      colnames = c("Timestamp", "Day", "Location", "Last 4 CC Numbers", "Price"),
                      options = list(pageLength = 10,
                                     search = list(regex = TRUE, caseInsensitive = TRUE, search = node())), 
                      rownames = FALSE) %>%
          formatDate(1, method = 'toLocaleString', 
                     params = list(month = 'numeric',
                                   day = 'numeric',
                                   year = 'numeric',
                                   hour = 'numeric',
                                   minute = 'numeric'))
    
      }
    })
  })
}
