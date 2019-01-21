lpdBoxplotTab = bs4TabItem(
    tabName = "lpd_boxplot",
    fluidRow(
        bs4Card(
            title = "T Test Results",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("lpd_statTable")
        ),
        bs4Card(
            width = 6,
            plotlyOutput("lpd_boxPlot")
        )
    )
)

lpdPCATab = bs4TabItem(
    tabName = "lpd_pca",
    fluidRow(
        bs4Card(
            width = 6,
            plotlyOutput("lpd_pca")
        ),
        bs4Card(
            width = 6,
            sliderInput("pca_cutoff", "Select a p value cutoff: ",
                        min = 0, max = 1, value = 0.1, step = 0.05)
        )
    )
)

corrTab = bs4TabItem(
    tabName = "lpd_atm",
    fluidRow(
        column(
            width = 6,
            bs4Card(
                width = 12,
                DT::dataTableOutput("lpd_atm_dt")
            )
        ),
        column(
            width = 6,
            bs4Card(
                width = 12,
                selectInput("lpd_atm", "Select a Anthropometric Variable",
                            choices = names(corr_atm$class$pearson),
                            selected = names(corr_atm$class$pearson)[1]),
                selectInput("lpd_atm_color", "Select a variable to color it",
                            choices = c("flipgroup", "sex_updated")),
                plotlyOutput("lpd_atm_scatter")
            )
        )
    )
)

lpdZscoreTab = bs4TabItem(
    tabName = "lpd_zscore",
    fluidRow(
        column(
            width = 6,
            bs4Card(
                width = 12,
                DT::dataTableOutput("lpd_zscore_dt")
            ),
            bs4Card(
                width = 12,
                uiOutput("zscore_volcano_ui")
            )
        ),
        column(
            width = 6,
            bs4Card(
                width = 12,
                selectInput("zscore_var", "Select a Z score variable:",
                            choices = c("waz18", "laz18", "wlz18", "hcz18"),
                            selected = "waz18"),
                numericInput("zscore_cutoff", "Input a Z score cutoff:",
                             min = -2, max = 2, step = 0.1, value = -1.9),
                checkboxInput("zscore_show_points", "Show Points",
                              value = TRUE)
            ),
            bs4Card(
                width = 12,
                plotlyOutput("lpd_zscore_boxplot")
            ),
            bs4Card(
                width = 12,
                uiOutput("zscore_hist_ui")
            )
        )
    )
)

corrHistTab = bs4TabItem(
    tabName = "p_hist",
    fluidRow(
        bs4Card(
            width = 6,
            plotOutput("hist_waz")
        ),
        bs4Card(
            width = 6,
            plotOutput("hist_laz")
        ),
        bs4Card(
            width = 6,
            plotOutput("hist_wlz")
        ),
        bs4Card(
            width = 6,
            plotOutput("hist_hcz")
        )
    )
    
)

downloadForm = function(zscore){
    type = gsub("(\\D{3})\\d{2}", "\\1", zscore)
    tagList(
        tags$div(
            class = "col-sm-4 d-inline-block",
            numericInput(glue("{type}_ht"), "Height", value = 6, width = "100%")
        ),
        tags$div(
            class = "col-sm-4 d-inline-block",
            numericInput(glue("{type}_wt"), "Width", value = 8, width = "100%")
        ),
        tags$div(
            class = "col-sm-3 d-inline-block",
            downloadButton(glue("{type}_downloader"))
        )
    )
}

volcanoTab = bs4TabItem(
    tabName = "corr_vol",
    fluidRow(
        bs4Card(
            width = 12,
            tags$div(
                class = "col-sm-4 d-inline-block",
                checkboxGroupInput(
                    "var_add_vp", label = "Variables add to the plot (only select one)",
                    choices = names(corr_atm)[names(corr_atm) != "species"],
                    inline = TRUE
                )
            ),
            tags$div(
                class = "col-sm-4 d-inline-block",
                checkboxInput("show_all_av", "Show labels with only |r| > 0.2",
                              value = FALSE)
            )
        ),
        bs4Card(
            width = 6,
            downloadForm("waz"),
            plotOutput("vol_waz")
        ),
        bs4Card(
            width = 6,
            downloadForm("laz"),
            plotOutput("vol_laz")
        ),
        bs4Card(
            width = 6,
            downloadForm("wlz"),
            plotOutput("vol_wlz")
        ),
        bs4Card(
            width = 6,
            downloadForm("hcz"),
            plotOutput("vol_hcz")
        )
    )
)

body = bs4DashBody(
    ## CSS sheet
    tags$head(
        tags$link(rel="stylesheet", type = "text/css", href = "styles.css"),
        tags$script(src="scripts.js")
    ),
    
    ## main body layout
    bs4TabItems(
        ## Boxplot page layout
        lpdBoxplotTab,
        ## PCA page layout
        lpdPCATab,
        ## Correlation page layout
        corrTab,
        ## Categorical Z score test page layout
        lpdZscoreTab,
        corrHistTab,
        volcanoTab
    )
)