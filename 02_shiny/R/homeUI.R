homeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Overview of Visual Analytics of Spending & Kinematics (V-ASK)")
      ),
      
      column(width = 5,
             h4("V-ASK App is arranged using navigation bars and tab panels in 3 main sections:"),
             br(),
             h5("1. Spending EDA – Separate tabs for exploratory data analytics for each transactional data, ‘Credit Card Data’ and ‘Loyalty Card Data’, and a tab for combined dataset."),
             h5("2. Patterns of Life Kinematics – ‘GPS Movement’, ‘Owners Identification’, the ‘Credit card and loyalty card matching’ tab will not be available for use by real life law enforcement"),
             h5("3. Relationship Networks – ‘Organisational chart of GASTech’ and ‘Spending habits’"),
             br(),
             h4("'Insights' section is added to showcase of anomalies and suspicious activities which may be facilitate the investigation of law enforcements."),
             br(),
             # p("For more information on this project, please refer to our "),
             # tags$a(href="https://vapg3.netlify.app/", "Group Project Website")
             HTML("<h5>For more information on this project, please refer to our <a href='https://vapg3.netlify.app/'>Group Project Website</a>!</h5>")
      ),
      
      column(width = 7,
             img(src = "overview.png", width = 1000, height = 337)
      )
    )
  )
}