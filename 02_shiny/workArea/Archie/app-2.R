#Install and Lauch R Packages
library(shiny)
#library(shinydashboard)
#library(shinyWidgets)
library(shinythemes)
library(plotly)
library(ggiraph)
library(tmap)
library(sf)
library(raster)
library(igraph)
library(ggraph)
library(visNetwork)
library(clock)
library(lubridate)
library(tidyverse)

#import data
# car_data <- read_csv("data/aspatial/car-assignments.csv")
# cc_data <- read_csv("data/aspatial/cc_data.csv")
# loyalty_data <- read_csv("data/aspatial/loyalty_data.csv")
# gps_data <- read_csv("data/aspatial/gps.csv")
# bgmap <- raster("data/Geospatial/abila_map.tif")
# 

# Define UI ----
ui <- dashboardPage(
    #Header content
    dashboardHeader(title = "V-ASK App"),
    
    #UI/UX style and customization
    #skin = "blue",
    
    #Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Spending", tabName = "spending", icon = icon("comment-dollar")),
            menuItem("Kinematics", tabName = "kinematics", icon = icon("route")),
            menuItem("Anomalies", tabName = "anomalies", icon = icon("exclamation-triangle")),
            menuItem("Relationship", tabName = "relationship", icon = icon("people-arrows")),
            menuItem("Insights", tabName = "insights", icon = icon("search-location"))
        )
    ), #dashboardSidebar
    
    dashboardBody(
        tabItems(
            tabItem(tabName ="home",
                    h1("Overview"),
                    fluidRow(align = "left",
                             h2("Context + Overview of the App")
                    )
            ), #tabItem home
            
            tabItem(tabName ="spending",
                    h1("Credit Card and Loyalty Data Exploratory Data Analysis"),
                    tabsetPanel(
                        tabPanel("Credit Card Data",
                                 fluidRow(
                                     box(
                                         title = "Most Popular Locations", width = 12, status = "primary", solidHeader = TRUE,
                                     )
                                 )
                        ),
                        
                        tabPanel("Loyalty Data",
                                 fluidRow(
                                     box(
                                         title = "Most Popular Locations", width = 12, status = "primary", solidHeader = TRUE,
                                     )
                                 )
                        ),
                        
                        tabPanel("Credit Card + Loyalty Data",
                                 fluidRow(
                                     box(
                                         title = "Mapping of Credit Card and Loyalty Number", width = 12, status = "primary", solidHeader = TRUE,
                                     )
                                 )
                        )
                    )
            ), #tabItem spending
            
            tabItem(tabName ="kinematics",
                    h1("Pattern of Life Kinenamatics"),
                    tabsetPanel(
                        tabPanel("GPS Movement",
                                 fluidRow(
                                     box(
                                         title = "Interactive Map", width = 12, status = "primary", solidHeader = TRUE,
                                     )
                                 )
                        ),
                        
                        tabPanel("Owners Identification",
                                 fluidRow(
                                     box(
                                         title = "Credit Card and Loyalty Number Owners", width = 12, status = "primary", solidHeader = TRUE,
                                     )
                                 )
                        )
                    )
            ), #tabItem kinematics
            
            
            tabItem(tabName ="anomalies",
                    h1("Anomaly Diagnostics"),
                    tabsetPanel(
                        tabPanel("Financial Anomalies",
                                 fluidRow(
                                     box(
                                         title = "Anomalies based on Credit Card and Loyalty Data", width = 12, status = "primary", solidHeader = TRUE,
                                     )
                                 )
                        ),
                        
                        tabPanel("Movement Anomalies",
                                 fluidRow(
                                     box(
                                         title = "Anomalies based on GPS Data", width = 12, status = "primary", solidHeader = TRUE,
                                     )
                                 )
                        )
                    )
            ), #tabItem anomalies
            
            tabItem(tabName ="relationship",
                    h1("GASTech Personnel Relationships"),
                    tabsetPanel(
                        tabPanel("Spending Habits",
                                 fluidRow(
                                     box(
                                         title = "Relationship based on Credit Card and Loyalty Data", width = 12, status = "primary", solidHeader = TRUE,
                                     )
                                 )
                        ),
                        
                        tabPanel("Movement",
                                 fluidRow(
                                     box(
                                         title = "Relationship based on GPS Data", width = 12, status = "primary", solidHeader = TRUE,
                                     )
                                 )
                        )
                    )
            ), #tabItem relationship
            
            
            tabItem(tabName ="insights",
                    h1("Suspicious Activities"),
                    
                    fluidRow(align = "left",
                             h2("Identification of Suspicious Activities")
                    )
            ) #tabItem insights
        ) #tabItems
    ) #dashboardBody
    
) #ui
    

# Define server logic ----
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
