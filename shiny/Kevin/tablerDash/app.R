if(interactive()){
    library(shiny)
    library(tablerDash)
    
    shiny::shinyApp(
        ui = tablerDashPage(
            navbar = tablerDashNav(
                id = "mymenu",
                navMenu = tablerNavMenu(
                    tablerNavMenuItem(
                        tabName = "Tab1",
                        "Tab 1",
                        # https://preview.tabler.io/icons.html
                        icon = "bell"
                    ),
                    tablerNavMenuItem(
                        tabName = "Tab2",
                        "Tab 2",
                        icon = "box"
                    ),
                )
            ),
            footer = tablerDashFooter(),
            title = "test",
            body = tablerDashBody(
                tablerTabItems(
                    tablerTabItem(
                        tabName = "Tab1",
                        histogramUI("hist")
                    ),
                    tablerTabItem(
                        tabName = "Tab2",
                        histogramUI("hist2")
                    )
                )
            )
        ),
        server = function(input, output) {
            histogramServer("hist")
            histogramServer("hist2")
        }
    )
}