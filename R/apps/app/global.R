pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase', "shiny", 
         "shinydashboard", "ggsci", "glue", "ggrepel", "R6", "shinyjs", "ggmetaplots")
for(pkg in pkgs){
    suppressPackageStartupMessages(
        library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
                character.only=TRUE)
    )
}

load('data/data.rda')

theme_set(theme_bw())

