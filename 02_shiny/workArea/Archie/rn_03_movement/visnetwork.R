library(igraph)
library(visNetwork)
library(dplyr)
library(shiny)
library(shinythemes)
library(DT)

#dataset
id<-c("articaine","benzocaine","etho","esli")
label<-c("articaine","benzocaine","etho","esli")
node<-data.frame(id,label)

from<-c("articaine","articaine","articaine",
        "articaine","articaine","articaine",
        "articaine","articaine","articaine")
to<-c("benzocaine","etho","esli","benzocaine","etho","esli","benzocaine","etho","esli")
title<-c("SCN1A","SCN1A","SCN1A","SCN2A","SCN2A","SCN2A","SCN3A","SCN3A","SCN3A")

edge<-data.frame(from,to,title)


#app

ui <- fluidPage(
  
  # Generate Title Panel at the top of the app
  titlePanel("Network Visualization App"),
  
  fluidRow(
    column(width = 6,
           DTOutput('tbl')),
    column(width = 6,
           visNetworkOutput("network")) #note that column widths in a fluidRow should sum to 12
  ),
  fluidRow(column(width = 6), 
           column(width=6, "Click and hold nodes for a second to select additional nodes.")
  )
  
) #end of fluidPage


server <- function (input, output, session){
  
  output$network <- renderVisNetwork({
    visNetwork(nodes = node,edge) %>% 
      visOptions(highlightNearest=TRUE, 
                 nodesIdSelection = TRUE) %>%
      #allow for long click to select additional nodes
      visInteraction(multiselect = TRUE) %>%
      visIgraphLayout() %>% 
      
      #Use visEvents to turn set input$current_node_selection to list of selected nodes
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_selection', nodes.nodes);
                ;}")
    
  })
  
  #render data table restricted to selected nodes
  output$tbl <- renderDT(
    edge %>% 
      filter((to %in% input$current_node_selection)|(from %in% input$current_node_selection)),
    options = list(lengthChange = FALSE)
  )
  
}

shinyApp(ui, server)