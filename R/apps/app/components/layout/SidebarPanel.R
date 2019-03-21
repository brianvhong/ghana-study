SidebarPanel = R6Class(
    "SidebarPanel",
    inherit = ShinyModule,
    public = list(
        # attributes
        
        # initializer
        initialize = function(){
         
        },
        
        # UI
        ui = function(){
            dashboardSidebar(
                sidebarMenu(
                    id = "tabs",
                    menuItem("Lipidome Boxplot", tabName = "lpd-boxplot"),
                    menuItem("Clinical Values", tabName = "cli-boxplot"),
                    menuItem("vs Anthropometric", tabName = "lpd-atm"),
                    menuItem("PCA", tabName = "lpd-pca")
                    #menuItem("Categorial Z scores", tabName = "zscore"),
                    #menuItem("P value Histograms", tabName = "hist"),
                    #menuItem("Correlation Volcano", tabName = "corr-volc")
                ),
                tags$br(),
                tags$br(),
                tags$hr(),
                uiOutput("input-panel")
            )
        },
        
        # server
        #' @emit tabs: string, the current sidbar tabmae
        #' @emit lpd-level: string, the lpd level
        #' @emit corr-method: string, the correlation method
        #' @emit atm: string, the anthropometric variable
        server = function(input, output, session){
            emit = reactiveValues(
                tabs = NULL,
                `lpd-level` = NULL,
                `corr-method` = NULL,
                atm = NULL
            )
            
            output$`input-panel` = renderUI({
                if(input$tabs == "lpd-boxplot") {
                    tagList(
                        selectInput(
                            session$ns("lpd-level"),
                            "Select lipidomics data level",
                            choices = names(lpd)
                        )
                    )
                } else if (input$tabs == "lpd-atm") {
                    tagList(
                        selectInput(
                            session$ns("lpd-level"),
                            "Select lipidomics data level",
                            choices = names(lpd)
                        ),
                        selectInput(
                            session$ns("corr-method"), 
                            "Select Correlation Method:",
                            choices = names(corr_atm$class),
                            selected = names(corr_atm$class)[1]
                        ),
                        selectInput(
                            session$ns("atm"),
                            "Select the athropometric variable",
                            choices = c("waz18", "laz18", "wlz18", "hcz18", "chol_efflux"),
                            selected = "waz18"
                        )
                    )
                }
            })
            
            observe({
                emit$tabs = input$tabs
                emit$`lpd-level` = input$`lpd-level`
                emit$`corr-method` = input$`corr-method`
                emit$atm = input$atm
            })
            return(emit)
        }
    )
)