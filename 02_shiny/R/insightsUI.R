homeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Key Insights from Exploring V-ASK App")
      ),
    
      column(width = 12,
             h3("Spending EDA"),
             h5("Based on a combination of various visualisations under the ‘Spending EDA’ tab, we found that Katerina’s Café is popular throughout the week with the bulk of their customers transacting during lunch and dinner."),
      ),
      
      column(width = 6,
             img(src = "eda_insights_02.png", width = 1000, height = 670),
      ),
      
      column(width = 6,
             img(src = "eda_insights_01.png", width = 1000, height = 670),
      ),
      
    )
  )
}