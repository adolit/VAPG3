# Functions

poisMapUI <- function(id) {
  tagList(
    fluidRow(
      column(width = 4,
             selectInput(NS(id, "date"),
                         "Date",
                         choices = sort(unique(df_gps$date)))
       ),
      column(width = 4,
             sliderInput(NS(id, "hour"),
                         "Hour",
                         min = 0,
                         max = 24,
                         value = c(0, 24))
      ),
      column(width = 4,
             selectInput(NS(id, "CarID"),
                         "Car ID",
                         choices = sort(unique(df_gps$CarID)))
      )
    ),
    tmapOutput(NS(id, "map"), height = 600)
  )
}

poisMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderTmap({
      tmap_mode("view")
      tm_shape(bgmap) +
        tm_rgb(bgmap, r = 1, g = 2, b =3,
               alpha = NA,
               saturation = 1,
               interpolate = TRUE,
               max.value = 255) +
        tm_shape(gps_dots_selected) +
        tm_dots(col = 'red', border.col = 'gray', size = .2, alpha = 0.3, jitter = .8)
    })
  })
}
