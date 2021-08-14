#Install and Launch R Packages
#Import and prepare data
source("global.R")

# Define UI ----
ui <- navbarPage("Visual Analytics of Spending & Kinematics (V-ASK)",
                 windowTitle = "V-ASK App", #to do add favicon
                 position = "static-top",
                 collapsible = TRUE,
                 fluid = TRUE, 
                 theme = bslib::bs_theme(
                     bootswatch = "sandstone"),

                 #Syiqah ----
                 tabPanel("Home", icon = icon("home"),
                          fluidPage(
                              h2("Overview"),
                              fluidRow(align = "left",
                                       h3("Context + Overview of the App")
                              )
                          )
                 ), #tabPanel Home
                 
                 tabPanel("Spending EDA", icon = icon("credit-card"),
                          fluidPage(
                              tabsetPanel(
                                  tabPanel("Credit Card Data",
                                           edaCreditUI("se_cc")
                                  ),
                                  
                                  tabPanel("Loyalty Card Data",
                                           edaLoyaltyUI("se_lc")
                                  ),
                                  
                                  tabPanel("Credit Card + Loyalty Data",
                                           edaCombineUI("se_combine")
                                  )
                              )
                          )
                 ), #tabPanel Spending
                 
                 #Kevin ----
                 tabPanel("Patterns of Life Kinematics", icon = icon("route"),
                          fluidPage(
                              tabsetPanel(
                                  tabPanel("GPS Movement",
                                           pathsMapUI("plk_path_map")
                                  ),
                                  
                                  tabPanel("Owners Identification",
                                           poisMapUI("plk_poi_map")
                                  )
                              )                        
                          )
                 ), #tabPanel Kinematics
                 
                 #Archie ----
                 tabPanel("Relationship Networks", icon = icon("people-arrows"),
                          fluidPage(
                              tabsetPanel(
                                  tabPanel("Organizational Chart",
                                           treeMapUI("rn_orgchart")
                                  ),
                                  
                                  tabPanel("Spending Habits",
                                           spendingNetworkUI("rn_spending")
                                  ),
                                  
                                  tabPanel("Geospatial Movement",
                                           movementNetworkUI("rn_movement")
                                  )
                              )
                          )
                 ), #tabPanel Relationship
                 
                 tabPanel("Insights",icon = icon("search-location"),
                          fluidPage(
                              fluidRow(align = "left",
                                       h2("Identification of Suspicious Activities")
                              )
                          )
                 ) #tabPanel Insights
) #ui
    

# Define server logic ----
server <- function(input, output) {
    
    #Syiqah ----
    edaCreditServer("se_cc")
    edaLoyaltyServer("se_lc")
    edaCombineServer("se_combine")
    
    #Kevin ----
    pathsMapServer("plk_path_map")
    poisMapServer("plk_poi_map")
    
    #Archie ----
    treeMapServer("rn_orgchart")
    spendingNetworkServer("rn_spending")
    movementNetworkServer("rn_movement")
}

# Run the application 
shinyApp(ui = ui, server = server)