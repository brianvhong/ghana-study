controlbar = bs4DashControlbar(
    skin = "light",
    title = "Parameter Controller",
    selectInput("level", "Select Lipid Level:", 
                choices = names(lpd), selected = "class"),
    selectInput("corr_method", "Select Correlation Method:",
                choices = names(corr_atm$class),
                selected = names(corr_atm$class)[1])
)