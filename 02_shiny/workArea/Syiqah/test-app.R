library(shiny)

# Define UI
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    navbarPage(
    "V-ASK App",   
    tabPanel("Home", 
             "Introduction + User Guide link"),
    navbarMenu("Spending EDA", 
               tabPanel("Credit card transactions", "Spending EDA - Credit card"),
               tabPanel("Loyalty card transactions", "Spending EDA - Loyalty card"),
               tabPanel("Credit card and loyalty card", "Credit card and loyalty card")
    ),
    navbarMenu("Kinematics Patterns", 
               tabPanel("Vehicle GPS", "Explore vehicle movements"),
               tabPanel("Vehicle owners", "Inferring credit card and loyalty card owners")
    ),
    navbarMenu("Anomalies in Data", 
               tabPanel("Credit card transactions", "Outliers in credit card transactions"),
               tabPanel("Loyalty card transactions", "Outliers in loyalty card transactions"),
               tabPanel("Vehicle movement", "What anomaly to feature here?")
    ),
    tabPanel("Employee Networks", "GASTech personnel relationships"),
    tabPanel("Insights", "Static summary of suspicious activities")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
