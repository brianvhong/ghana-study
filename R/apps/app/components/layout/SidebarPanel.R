SidebarPanel = R6Class(
    "SidebarPanel",
    inherit = ShinyModule,
    public = list(
        # attributes
        emit = reactiveValues(
            tab = NULL,
            lpd = reactiveValues(
                level = NULL,
                fct = "chol_efflux",
                cli = NULL,
                sec = NULL
            ),
            glc = reactiveValues(
                level = NULL,
                fct = "chol_efflux",
                cli = NULL,
                sec = NULL
            ),
            cli = reactiveValues(
                fct = "chol_efflux",
                alter = NULL
            ),
            fct = reactiveValues(
                alter = NULL
            ),
            sec = reactiveValues(
                level = NULL
            )
        ),
        
        # initializer
        initialize = function(){
         
        },
        
        # UI
        ui = function(){
            dashboardSidebar(
                sidebarMenu(
                    id = "tab",
                    menuItem(
                        "Lipidome",
                        menuSubItem("Boxplot", tabName = "lpd-lm"),
                        menuSubItem("vs Clinical Values", tabName = "lpd-cli"),
                        menuSubItem("vs HDL Function", tabName = "lpd-fct")
                    ),
                    menuItem(
                        "Glycoproteome",
                        menuSubItem("Boxplot", tabName = "glc-lm"),
                        menuSubItem("vs Clinical Values", tabName = "glc-cli"),
                        menuSubItem("vs HDL Function", tabName = "glc-fct"),
                        menuSubItem("vs SEC", tabName = "glc-sec")
                    ),
                    menuItem(
                        "Clinical Values",
                        menuSubItem("Boxplot", tabName = "cli-lm"),
                        menuSubItem("vs HDL Function", tabName = "cli-fct")
                    ),
                    menuItem(
                        "HDL Function",
                        menuSubItem("Boxplot", tabName = "fct-lm")
                    ),
                    menuItem(
                        "SEC",
                        menuSubItem("SEC Boxplot", tabName = "sec-lm"),
                        menuSubItem("SEC Cromatogram", tabName = "sec-chrom")
                    )
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
            
            output$`input-panel` = renderUI({
                inputs = tagList()
                if(grepl("^lpd", input$tab)) {
                    inputs = tagAppendChild(
                        inputs,
                        selectInput(
                            session$ns("lpd-level"),
                            "Select lipidomics data level",
                            choices = names(data$data$lpd)
                        )
                    )
                } else if(grepl("^glc", input$tab)) {
                    inputs = tagAppendChild(
                        inputs,
                        selectInput(
                            session$ns("glc-level"),
                            "Select lipidomics data level",
                            choices = names(data$data$glc)
                        )
                    )
                    if(input$tab == "glc-sec"){
                        inputs = tagAppendChild(
                            inputs,
                            selectInput(
                                session$ns("sec-var"),
                                "HDL Fraction:",
                                choices = featureNames(data$data$sec$hdl)
                            )
                        )
                    }
                } else if(grepl("^sec", input$tab)) {
                    inputs = tagAppendChild(
                        inputs,
                        selectInput(
                            session$ns("sec-level"),
                            "Chromatogram fractions or HDL fractions:",
                            choices = names(data$data$sec)
                        )
                    )
                }
                
                if(input$tab == "cli-lm"){
                    inputs = tagAppendChild(
                        inputs,
                        radioButtons(
                            inputId = "cli-alter",
                            label = "Alternative",
                            choices = c("two.sided", "less", "greater"),
                            selected = "two.sided"
                        )
                    )
                }
                
                if(input$tab == "fct-lm"){
                    inputs = tagAppendChild(
                        inputs,
                        radioButtons(
                            inputId = "fct-alter",
                            label = "Alternative",
                            choices = c("two.sided", "less", "greater"),
                            selected = "two.sided"
                        )
                    )
                }
                
                if(grepl("-cli$", input$tab)) {
                    inputs = tagAppendChild(
                        inputs,
                        selectInput(
                            session$ns("cli-var"),
                            "Select a Clinical Variable",
                            choices = featureNames(data$data$cli)
                        )
                    )
                }
                inputs
            })
            
            observeEvent(input$tab, {
                self$emit$tab = input$tab
                if(grepl("^lpd", input$tab)){
                    observeEvent(input$`lpd-level`, {
                        self$emit$lpd$level = input$`lpd-level`
                    })
                    observeEvent(input$`cli-var`, {
                        self$emit$lpd$cli = input$`cli-var`  
                    })
                } else if (grepl("^glc", input$tab)) {
                    observeEvent(input$`glc-level`, {
                        self$emit$glc$level = input$`glc-level`
                    })
                    observeEvent(input$`cli-var`, {
                        self$emit$glc$cli = input$`cli-var`  
                    })
                    observeEvent(input$`sec-var`, {
                        self$emit$glc$sec  = input$`sec-var`
                    })
                }
                if(input$tab == "cli-lm"){
                    observeEvent(input$`cli-alter`, {
                        self$emit$cli$alter = input$`cli-alter`
                    })
                }
                if(input$tab == "fct-lm"){
                    observeEvent(input$`fct-alter`, {
                        self$emit$fct$alter = input$`fct-alter`
                    })
                }
                if(input$tab == "sec-lm") {
                    observeEvent(input$`sec-level`, {
                        self$emit$sec$level = input$`sec-level`
                    })
                }
            })
            
            return(self$emit)
        }
    )
)