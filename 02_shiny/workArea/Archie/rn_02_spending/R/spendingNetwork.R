spendingNetworkUI <- function(id) {
  tagList(
    fluidRow(
      column(width = 12,
             h3("Unofficial Relationship Networks based on Credit Card Data")
      ),
      
      column(width = 3,
             h5("Set Parameters of Credit Card Data"),
             
             dateRangeInput(NS(id,"date"),
                            label="Select Date of Transaction:",
                            start  = "2014-01-06",
                            end    = "2014-01-19",
                            min    = "2014-01-06",
                            max    = "2014-01-19",
                            startview = "month",
                            separator = " to ",
                            format = "dd/m/yyyy"),
             
             sliderInput(NS(id,"hour"), 
                            "Select Hour:", 
                            min = 0, 
                            max = 24, 
                            value = c(7,13), 
                            step = 1),
             
             selectInput(NS(id,"day"),
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
                                      "Fri")
                         ),
             
             h5("Select GASTech Employee Deparment"),
             selectInput(NS(id,"deparment"),
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
             selectInput(NS(id,"layout"),
                         "Select Network Layout:",
                         choices =  list("layout_as_star",
                                         "layout_in_circle",
                                         "layout_nicely",
                                         "layout_on_grid",
                                         "layout_on_sphere",
                                         "layout_with_dh",
                                         "layout_with_fr",
                                         "layout_with_gem",
                                         "layout_with_graphopt",
                                         "layout_with_kk",
                                         "layout_with_lgl",
                                         "layout_with_mds"),
                         selected = "layout_nicely"
             ),
      ),
      
      column(width = 9,
             h5("Explore Relationship of GASTech Employees according to Purchase Location"),
             visNetworkOutput(NS(id,"cc_network"), 
                                   width = "100%", 
                                   height = "800px")
      )
    )
  )
}


spendingNetworkServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$cc_network <- renderVisNetwork({
      
      #df_cc$last4ccnum <- as_factor(df_cc$last4ccnum)
      
      df_cc_network <- df_cc %>%
        filter(date >= input$date[1] &
                 date <= input$date[2]) %>%
        filter(hour >= input$hour[1] &
                 hour <= input$hour[2]) %>%
        filter(day %in% input$day)
      
      cc_edges <- df_cc_network %>%
        group_by(last4ccnum, location, date, hour) %>%
        mutate(from = last4ccnum) %>%
        mutate(to = location) %>%
        ungroup() %>%
        group_by(from, to) %>%
        summarise(weight = n()) 
      
      sources <- df_cc_network %>%
        distinct(last4ccnum)  %>%
        rename(label = last4ccnum) %>%
        mutate(group = "GAStech Employee")
      
      destinations <- df_cc_network %>%
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
        visIgraphLayout(layout = input$layout) %>%
        visEdges(arrows = "to",
                 smooth = list(enabled = TRUE,
                               type = "curvedCW")) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE) %>%
        visLegend() %>%
        visLayout(randomSeed = 123) %>%
        visInteraction(hover = TRUE) %>%
        visGroups(groupname = "GAStech Employee", shape = "icon",
                  icon = list(code = "f007", size = 40, color = "slategray")) %>%
        visGroups(groupname = "Location", shape = "icon",
                  icon = list(code = "f041", size = 100, color = "goldenrod")) %>%
        addFontAwesome()
    })
  })
}
