#Install and Launch R Packages
#Import and prepare data
source("global.R")

# Define UI ----
ui <- navbarPage("Visual Analytics of Spending & Kinematics (V-ASK)",
                 windowTitle = "V-ASK App",
                 position = "static-top",
                 collapsible = TRUE,
                 fluid = TRUE, 
                 theme = bslib::bs_theme(
                     bootswatch = "sandstone"),
                     #bootswatch = "lux"),
                     #bootswatch = "flatly"),
                 
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
                                           fluidRow(
                                               column(width = 12,
                                                      h3("Most popular locations"),
                                                      barCCUI("barcc")
                                               )
                                           ),
                                           fluidRow(
                                               column(width = 12,
                                                      h3("Transactions by location"),
                                                      hmCCtransdateUI("hmcctransdate")
                                               )
                                           ),
                                           fluidRow(
                                               column(width = 12,
                                                      h3("Most popular hours of the day"),
                                                      heatmapCCUI("heatmapcc")
                                               )
                                           ),
                                           
                                           fluidRow(
                                               column(width = 12,
                                                      h3("Credit Card Transactions"),
                                                      boxplotCCUI("boxplotcc")
                                               )
                                           )
                                  ),
                                  
                                  tabPanel("Loyalty Card Data",
                                           fluidRow(
                                               column(width = 12,
                                                      h3("Most popular locations"),
                                                      barLCUI("barlc")
                                               )
                                           ),
                                           fluidRow(
                                               column(width = 12,
                                                      h3("Transactions by location"),
                                                      hmLCtransdateUI("hmlctransdate")
                                               )
                                           ),
                                           fluidRow(
                                               column(width = 12,
                                                      h3("Loyalty card transactions"),
                                                      boxplotLCUI("boxplotlc")
                                               )
                                           )
                                  ),
                                  
                                  #Kevin ----
                                  tabPanel("Credit Card + Loyalty Data",
                                           fluidRow(
                                               column(width = 12,
                                                   h3("Mapping of Credit Card and Loyalty Number")
                                               )
                                           )
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
                 
                 # tabPanel("Anomaly Diagnostics", icon = icon("exclamation-triangle"),
                 #          fluidPage(
                 #              tabsetPanel(                         
                 #                  tabPanel("Spending Anomalies",
                 #                           fluidRow(
                 #                               column(width = 12,
                 #                                      h3("Credit Card Transactions"),
                 #                                      boxplotCCUI("boxplotcc")
                 #                               )
                 #                           ),
                 #                           fluidRow(
                 #                               column(width = 12,
                 #                                      h3("Loyalty Card Transactions"),
                 #                                      boxplotLCUI("boxplotlc")
                 #                               )
                 #                           )
                 #                  ),
                 #                  
                 #                  tabPanel("Movement Anomalies",
                 #                           fluidRow(
                 #                               column(width = 12,
                 #                                   h3("Anomalies based on GPS Data")
                 #                               )
                 #                           )
                 #                  )
                 #              )
                 #          )
                 # ), #tabPanel Anomaly
                 
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
    barCCServer("barcc")
    hmCCtransdateServer("hmcctransdate")
    heatmapCCServer("heatmapcc")
    boxplotCCServer("boxplotcc")
    barLCServer("barlc")
    hmLCtransdateServer("hmlctransdate")
    boxplotLCServer("boxplotlc")
    
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