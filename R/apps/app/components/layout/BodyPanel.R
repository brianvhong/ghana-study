#import::here(PCAPage, .from="../pages/PCAPage.R")
import::here(ChromatogramPage, .from="../pages/ChromatogramPage.R")
import::here(BoxplotPage, .from="../pages/_BoxplotPage.R")
import::here(ScatterplotPage, .from="../pages/_ScatterplotPage.R")

BodyPanel = R6Class(
    "BodyPanel",
    inherit = ShinyModule,
    public = list(
        # attributes
        # pcaPage = NULL,
        lpdBoxplot = NULL,
        lpdFct = NULL,
        lpdCli = NULL,
        glcBoxplot = NULL,
        glcFct = NULL,
        glcCli = NULL,
        cliBoxplot = NULL,
        cliFct = NULL,
        fctBoxplot = NULL,
        secBoxplot = NULL,
        chromPage = NULL,
            
        # initializer
        initialize = function(){
            # self$pcaPage = PCAPage$new()
            # self$lpdAtmPage = LpdAtmPage$new()
            # self$secBoxplotPage = SecBoxplotPage$new()
            
            self$lpdBoxplot = BoxplotPage$new("lpd")
            self$lpdFct = ScatterplotPage$new("lpd-fct")
            self$lpdCli = ScatterplotPage$new("lpd-cli")
            self$glcBoxplot = BoxplotPage$new("glc")
            self$glcFct = ScatterplotPage$new("glc-fct")
            self$glcCli = ScatterplotPage$new("glc-cli")
            self$cliBoxplot = BoxplotPage$new("cli")
            self$cliFct = ScatterplotPage$new("cli-fct")
            self$fctBoxplot = BoxplotPage$new("fct")
            self$secBoxplot = BoxplotPage$new("sec")
            self$chromPage = ChromatogramPage$new()
        },
        
        # UI
        ui = function(){
            dashboardBody(
                tags$link(href="styles.css", rel = "stylesheet"),
                shinyjs::useShinyjs(),
                fluidRow(
                    tabItems(
                        # self$lpdAtmPage$ui(),
                        # self$pcaPage$ui(),
                        tabItem("lpd-lm", self$lpdBoxplot$ui()),
                        tabItem("lpd-cli", self$lpdCli$ui()),
                        tabItem("lpd-fct", self$lpdFct$ui()),
                        tabItem("glc-lm", self$glcBoxplot$ui()),
                        tabItem("glc-cli", self$glcCli$ui()),
                        tabItem("glc-fct", self$glcFct$ui()),
                        tabItem("cli-lm", self$cliBoxplot$ui()),
                        tabItem("cli-fct", self$cliFct$ui()),
                        tabItem("fct-lm", self$fctBoxplot$ui()),
                        tabItem("sec-lm", self$secBoxplot$ui()),
                        self$chromPage$ui()
                    )
                )
            )
        },
        
        # server
        #' @props tabs: string, the current sidbar tabmae
        #' @props lpd-level: string, the lpd level
        #' @props lpd-method: string, the correlation method
        #' @props atm: string, the anthropometric variable
        server = function(input, output, session, props){
            
            observe({
                if(grepl("^lpd", props$tab)){
                    if(!is.null(level <- props$lpd$level)) {
                        self$lpdBoxplot$call(props = reactiveValues(
                            lm = data$lm$lpd[[level]],
                            data = data$data$lpd[[level]]
                        ))
                        if(!is.null(fct <- props$lpd$fct)){
                            self$lpdFct$call(props = reactiveValues(
                                corr = data$corr$lpd$fct[[level]][[fct]],
                                X = data$data$lpd[[level]],
                                Y = data$data$fct,
                                y_var = fct
                            ))
                        }
                        if(!is.null(cli <- props$lpd$cli)){
                            self$lpdCli$call(props = reactiveValues(
                                corr = data$corr$lpd$cli[[level]][[cli]],
                                X = data$data$lpd[[level]],
                                Y = data$data$cli,
                                y_var = cli
                            ))
                        }
                    }
                }else if(grepl("^glc",props$tab)){
                    if(!is.null(level <- props$glc$level)){
                        self$glcBoxplot$call(props = reactiveValues(
                            lm = data$lm$glc[[level]],
                            data = data$data$glc[[level]]
                        ))
                        if(!is.null(fct <- props$glc$fct)){
                            self$glcFct$call(props = reactiveValues(
                                corr = data$corr$glc$fct[[level]][[fct]],
                                X = data$data$glc[[level]],
                                Y = data$data$fct,
                                y_var = fct
                            ))
                        }
                        if(!is.null(cli <- props$glc$cli)){
                            self$glcCli$call(props = reactiveValues(
                                corr = data$corr$glc$cli[[level]][[cli]],
                                X = data$data$glc[[level]],
                                Y = data$data$cli,
                                y_var = cli
                            ))
                        }
                    }
                } else if(props$tab == "fct-lm") {
                    alter = props$fct$alter
                    self$fctBoxplot$call(props = reactiveValues(
                        lm = data$lm$fct[[alter]],
                        data = data$data$fct
                    ))
                } else if(grepl("^cli", props$tab)){
                    alter = props$cli$alter
                    self$cliBoxplot$call(props = reactiveValues(
                        lm = data$lm$cli[[alter]],
                        data = data$data$cli
                    ))
                    if(!is.null(fct <- props$cli$fct)){
                        self$cliFct$call(props = reactiveValues(
                            corr = data$corr$cli$fct[[fct]],
                            X = data$data$cli,
                            Y = data$data$fct,
                            y_var = fct
                        ))
                    }
                } else if(props$tab == "sec-lm") {
                    self$secBoxplot$call(props = reactiveValues(
                        lm = data$lm$sec,
                        data = data$data$sec
                    ))
                } else if (props$tab == "sec-chrom") {
                    self$chromPage$call()
                }
            })
        },
        
        # call
        call = function(input, output, session, props){
            callModule(self$server, self$id, props)
        }
    )
)
