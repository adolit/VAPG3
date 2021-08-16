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
             h5("1. Spending EDA – Separate tabs for exploratory data analyis of ‘Credit Card Data’,‘Loyalty Card Data’, and Combined dataset."),
             h5("2. Patterns of Life Kinematics – ‘GPS Movement’ and ‘Owners Identification’ to identify the owners which is then summarized in ‘Credit card and loyalty card matching’."),
             h5("3. Relationship Networks – ‘Organisational chart of GASTech’ to show the official relationship of GASTech employees while ‘Spending habits’ to explore the unofficial relationships"),
             br(),
             h4("'Finally, Insights section is added to provide sample anomalies and suspicious activities which were uncovered using the interactive nature of the V-ASK App.
                Users are encouraged to further explore the App and identify suspicious patterns of behavior."),
             br(),
             HTML("<h5>For more information on this project, please refer to our <a href='https://vapg3.netlify.app/'>Group Project Website</a>!</h5>")
      ),
      
      column(width = 7,
             img(src = "overview.png", width = 900, height = 303)
      )
    )
  )
}