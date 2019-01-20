pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase', "shiny", 
         "bs4Dash", "ggsci", "glue", "ggrepel")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load('../../Rdata/lpd_precalc.rda')
source("global.R", local = TRUE)

for (file in list.files("ui", full.names = TRUE)) {
    source(file, local = TRUE)
}

ui = bs4DashPage(
    sidebar_collapsed = FALSE,
    navbar = bs4DashNavbar(
        status = "white",
        "Ghana Study"
    ),
    sidebar = sidebar,
    body = body,
    controlbar = controlbar,
    footer = myFooter
)

server = function(input, output) {
    for (file in list.files("server", full.names = TRUE)) {
        source(file, local = TRUE)
    }
}

shinyApp(ui = ui, server = server)

