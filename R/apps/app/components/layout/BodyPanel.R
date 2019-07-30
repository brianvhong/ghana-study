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
        glcSec = NULL,
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
            self$glcSec = ScatterplotPage$new("glc-sec")
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
                        tabItem("glc-sec", self$glcSec$ui()),
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
                        if(props$lpd$level %in% c("species_prop", "species_apoa1")) {
                            show_extraplots = TRUE
                        } else {
                            show_extraplots = FALSE
                        }
                        self$lpdBoxplot$call(props = reactiveValues(
                            lm = data$lm$lpd[[level]],
                            data = data$data$lpd[[level]],
                            show_extraplots = show_extraplots
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
                        if(props$glc$level %in% c("glycan_pep", "glycan_apoa1")) {
                            show_extraplots = TRUE
                        } else {
                            show_extraplots = FALSE
                        }
                        self$glcBoxplot$call(props = reactiveValues(
                            lm = data$lm$glc[[level]],
                            data = data$data$glc[[level]],
                            show_extraplots = show_extraplots
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
                        print(props$glc$sec)
                        if(!is.null(sec <- props$glc$sec)) {
                            self$glcSec$call(props = reactiveValues(
                                corr = data$corr$glc$sec[[level]][[sec]],
                                X = data$data$glc[[level]],
                                Y = data$data$sec$hdl,
                                y_var = sec
                            ))
                        }
                    }
                } else if(props$tab == "fct-lm") {
                    if(!is.null(alter <- props$fct$alter)){
                        self$fctBoxplot$call(props = reactiveValues(
                            lm = data$lm$fct[[alter]],
                            data = data$data$fct
                        ))
                    }
                } else if(grepl("^cli", props$tab)){
                    if(!is.null(alter <- props$cli$alter)){
                        self$cliBoxplot$call(props = reactiveValues(
                            lm = data$lm$cli[[alter]],
                            data = data$data$cli
                        ))
                        self$cliFct$call(props = reactiveValues(
                            corr = data$corr$cli$fct[["chol_efflux"]],
                            X = data$data$cli,
                            Y = data$data$fct,
                            y_var = "chol_efflux"
                        ))
                    }
                } else if(props$tab == "sec-lm") {
                    if(!is.null(level <- props$sec$level)){
                        self$secBoxplot$call(props = reactiveValues(
                            lm = data$lm$sec[[level]],
                            data = data$data$sec[[level]]
                        ))
                    }
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
