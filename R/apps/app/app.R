source("global.R", local = TRUE)
source("components/modules/ShinyModule.R")

import::here(HeaderPanel, .from="./components/layout/HeaderPanel.R")
import::here(SidebarPanel, .from="./components/layout/SidebarPanel.R")
import::here(BodyPanel, .from="./components/layout/BodyPanel.R")

App = R6Class(
    "App",
    inherit = ShinyModule,
    public = list(
        # attributes
        headerPanel = NULL,
        sidebarPanel = NULL,
        bodyPanel = NULL,
        
        # initializer
        initialize = function(){
            self$headerPanel = HeaderPanel$new()
            self$sidebarPanel = SidebarPanel$new()
            self$bodyPanel = BodyPanel$new()
        },
        
        # UI
        ui = function(){
            tagList(
                dashboardPage(
                    header = self$headerPanel$ui(),
                    sidebar = self$sidebarPanel$ui(),
                    body = self$bodyPanel$ui()
                )
            )    
        },
        
        # server
        server = function(input, output, session){
            sidebar = self$sidebarPanel$call()
        }
    )
)

app = App$new()

shinyApp(ui = app$ui(), server = app$server)

# ui = bs4DashPage(
#     sidebar_collapsed = FALSE,
#     navbar = bs4DashNavbar(
#         status = "white",
#         "Ghana Study"
#     ),
#     sidebar = sidebar,
#     body = body,
#     controlbar = controlbar,
#     footer = myFooter
# )
# 
# server = function(input, output) {
#     for (file in list.files("server", full.names = TRUE)) {
#         source(file, local = TRUE)
#     }
# }


