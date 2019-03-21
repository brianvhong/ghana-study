import::here(LpdBoxplotPage, .from="../pages/LpdBoxplotPage.R")
import::here(CliBoxplotPage, .from="../pages/CliBoxplotPage.R")
import::here(PCAPage, .from="../pages/PCAPage.R")
import::here(LpdAtmPage, .from="../pages/LpdAtmPage.R")

BodyPanel = R6Class(
    "BodyPanel",
    inherit = ShinyModule,
    public = list(
        # attributes
        lpdBoxplotPage = NULL,
        cliBoxplotPage = NULL,
        pcaPage = NULL,
        lpdAtmPage = NULL,
        zscorePage = NULL,
        histogramPage = NULL,
        corrVolcanoPage = NULL,
            
        # initializer
        initialize = function(){
            self$lpdBoxplotPage = LpdBoxplotPage$new()
            self$cliBoxplotPage = CliBoxplotPage$new()
            self$pcaPage = PCAPage$new()
            self$lpdAtmPage = LpdAtmPage$new()
        },
        
        # UI
        ui = function(){
            dashboardBody(
                tags$link(href="styles.css", rel = "stylesheet"),
                shinyjs::useShinyjs(),
                fluidRow(
                    tabItems(
                        self$lpdBoxplotPage$ui(),
                        self$cliBoxplotPage$ui(),
                        self$lpdAtmPage$ui(),
                        self$pcaPage$ui()
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
                if(props$tabs == "lpd-boxplot"){
                    self$lpdBoxplotPage$call(props = props)   
                } else if (props$tabs == "cli-boxplot") {
                    self$cliBoxplotPage$call()
                } else if (props$tabs == "lpd-atm") {
                    self$lpdAtmPage$call(props = props)   
                } else if (props$tabs == "lpd-pca") {
                    self$pcaPage$call()
                }
            })
        },
        
        # call
        call = function(input, output, session, props){
            callModule(self$server, self$id, props)
        }
    )
)
