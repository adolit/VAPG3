#Reference: https://community.rstudio.com/t/visnetwork-click-event-is-not-working-when-the-code-is-moved-into-a-module/5852/2

library('shiny')
library('dplyr')
library('visNetwork')
library('igraph')

#Ui code
ui <- fluidPage(
  
  tabPanel( "NetworkDiagram", networkdiagramModuleUI("NetworkDiagram"))
  
)
#server code
server <- function(input, output, session) {
  callModule(networkdiagramModule,"NetworkDiagram")
}
#module UI code
networkdiagramModuleUI <- function(id)
{
  ns <- NS(id)
  tagList(
    
    fluidRow(style = "margin-left: 30px;margin-bottom: 30px; width=100%; height=900px"
             ,column(12, uiOutput(ns("click_ui"),  height = 800) )
             ,column(12, visNetworkOutput(ns("Network_plot"),  height = 800) )
             
    )
    
  )
}


#module server code


networkdiagramModule <- function(input,output,session)
{
  nodes <- data.frame(id = 1:10, 
                      label = paste("Node", 1:10),                                 # add labels on nodes
                      group = c("GrA", "GrB"),                                     # add groups on nodes 
                      value = 1:10,                                                # size adding value
                      shape = c("square", "triangle", "box", "circle", "dot", "star",
                                "ellipse", "database", "text", "diamond"),                   # control shape of nodes
                      title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),         # tooltip (html or character)
                      color = c("darkred", "grey", "orange", "darkblue", "purple"),# color
                      shadow = c(FALSE, TRUE, FALSE, TRUE, TRUE))     
  edges <- data.frame(from = sample(1:10, 8), to = sample(1:10, 8),
                      label = paste("Edge", 1:8),                                 # add labels on edges
                      length = c(100,500),                                        # length
                      arrows = c("to", "from", "middle", "middle;to"),            # arrows
                      dashes = c(TRUE, FALSE),                                    # dashes
                      title = paste("Edge", 1:8),                                 # tooltip (html or character)
                      smooth = c(FALSE, TRUE),                                    # smooth
                      shadow = c(FALSE, TRUE, FALSE, TRUE))                       # shadow
  output$Network_plot <- renderVisNetwork({
    visNetwork(nodes, edges, width = "100%") %>%
      visIgraphLayout() %>%
      visEvents(click="function(ng_nodes){
                       Shiny.onInputChange('got_network_current_node_id',ng_nodes);}")
  })
  
  output$click_ui <- renderUI({
    if (is.null(input$got_network_current_node_id) )
    {
      paste0("No node has been clicked, yet", countn, sep='_')
    }
    else
    {
      if (length(input$got_network_current_node_id$node) == 0)
      {
        "You have clicked within the visNetwork but not on a node"
      }
      else
      {
        nodeid <- input$got_network_current_node_id$nodes
        tempnodeid <-unlist(nodeid)
        nodedata = subset(ng_nodes, (ng_nodes$id %in% tempnodeid))
        x<-as.character(nodedata$label)
        
        print(paste0("Selected Nodes:",paste(unlist(x), collapse = ", ")))
      }
    }
    countn = 'test20'
  })
  
}