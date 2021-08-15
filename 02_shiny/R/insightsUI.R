insightsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             h3("Key Insights from Exploring V-ASK App")
      ),
      
      fluidRow(
        column(width = 12,
               h4("Spending EDA"),
               h5("Based on a combination of various visualisations under the ‘Spending EDA’ tab, we found that Katerina’s Café is popular throughout the week with the bulk of their customers transacting during lunch and dinner."),
        ),
        
        column(width = 6,
               img(src = "eda_insights_02.png", width = 900, height = 600),
        ),
        
        column(width = 6,
               img(src = "eda_insights_01.png", width = 900, height = 600),
        ),
        
        column(width = 12,
               h5("Frydos Autosupply n’ More reported an outlier transaction of $10,000 on 13 Jan 2014, since their transactions typically range between $83 to $236. Further investigation should be performed on this transaction."),
        ),
        
        column(width = 6,
               img(src = "eda_insights_03.png", width = 900, height = 600),
        ),
        
        column(width = 6,
               img(src = "eda_insights_04.png", width = 900, height = 600),
        )
      ),  #EDA
      
      fluidRow(
        column(width = 12,
               h4("Patterns of Life Kinematics")
        ),
        
        column(width = 12,
               h5("From the ‘GPS Movement’ tab, we can generally identify the normal place of residence for employees assigned with a company car if we set the time interval from 9.00pm to 12.00am and refer to the POI location."),
               h5("The image below showcases the daily routine of the executive team including their places of interests and corresponding time of day."),
               img(src = "plk_insights_05.png", width = 1200, height = 600)
        ),
        
        column(width = 12,
               h5("The image below provides evidence of a big dinner party on January 10 attended by GASTech employees from Engineering and Information Technology Deparments."),
               img(src = "plk_insights_06.png", width = 1200, height = 600)
        ),
        
      ), #PLK
      
      fluidRow(
        column(width = 12,
               h4("Relationships Networks")
        ),
        
        column(width = 12,
               h5("Based on the interactive network of transactions graph, we observe that the truck drivers tend to form a community of their own, separate from the rest of the GASTech employees. This could largely be due to the nature of their work that involves driving to far locations such as Abila Airport."),
               img(src = "rn_insights_07.png", width = 1500, height = 600)
        ),
        
        column(width = 12,
               h5("From the same graph, we also found unexpected relations at the Chostus Hotel during office hours, which warrant further investigation."),
               img(src = "rn_insights_08.png", width = 1300, height = 600)
        )
      ), #PN
      
      fluidRow(
        column(width = 12,
               h4("Suspects"),
               h4("Finally, upon exploring all three modules of V-ASK App, we shortlist the following individuals as suspects responsible for the disappearance of their GASTech colleagues: Isia Vann, Hennie Osvaldo, Hideki Cocinaro and Bertrand Ovan. Coincidentally, three of them are from the Security department, except for Bertrand Ovan."),
               br(),
               br()
        ) 
      ) #suspects
    )
  )
}