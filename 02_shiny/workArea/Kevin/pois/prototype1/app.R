# Install and Launch R Packages
# Import and prepare data
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
                 
                 tabPanel("Spending EDA", icon = icon("comment-dollar"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Credit Card Data",
                                       fluidRow(
                                         column(width = 12,
                                                h3("Most Popular Locations")
                                         )
                                       )
                              ),
                              
                              tabPanel("Loyalty Data",
                                       fluidRow(
                                         column(width = 12,
                                                h3("Most Popular Locations")
                                         )
                                       )
                              ),
                              
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
                                       fluidRow(
                                         column(width = 12,
                                                h3("Interactive Map")
                                         )
                                       )
                              ),
                              
                              tabPanel("Owners Identification",
                                       fluidRow(
                                         column(width = 12,
                                                h3("Credit Card and Loyalty Number Owners")
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 12,
                                                h4("Step 1")
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 6,
                                                h4("Interactive Map"),
                                                poisMapUI("map")
                                         ),
                                         column(width = 6,
                                                h4("Interactive Map Table")
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 6,
                                                h4("Step 2")
                                         ),
                                         column(width = 6,
                                                h4("Step 3"))
                                       ),
                                       fluidRow(
                                         column(width = 6,
                                                h4("Credit Card")
                                         ),
                                         column(width = 6,
                                                h4("Loyalty Card")
                                         )
                                       )
                              )
                            )                   
                          )
                 ), #tabPanel Kinematics
                 
                 tabPanel("Anomaly Diagnostics", icon = icon("exclamation-triangle"),
                          fluidPage(
                            tabsetPanel(                         
                              tabPanel("Financial Anomalies",
                                       fluidRow(
                                         column(width = 12,
                                                h3("Anomalies based on Credit Card and Loyalty Data")
                                         )
                                       )
                              ),
                              
                              tabPanel("Movement Anomalies",
                                       fluidRow(
                                         column(width = 12,
                                                h3("Anomalies based on GPS Data")
                                         )
                                       )
                              )
                            )
                          )
                 ), #tabPanel Anomaly
                 
                 #Archie ----
                 tabPanel("Relationship Networks", icon = icon("people-arrows"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Spending Habits",
                                       fluidRow(
                                         column(width = 12,
                                                h3("Relationship based on Credit Card and Loyalty Data")
                                         )
                                       )
                              ),
                              
                              tabPanel("Movement",
                                       fluidRow(
                                         column(width = 12,
                                                h3("Relationship based on GPS Data")
                                         )
                                       )
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
  
  #Kevin ----
  poisMapServer("map")
  
  #Archie ----
  
}

# Run the application 
shinyApp(ui = ui, server = server)