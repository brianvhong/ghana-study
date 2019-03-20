import::here(LpdBoxplotPage, .from="../pages/LpdBoxplotPage.R")

BodyPanel = R6Class(
    "BodyPanel",
    inherit = ShinyModule,
    public = list(
        # attributes
        lpdBoxplotPage = NULL,
        pcaPage = NULL,
        lpdAtmPage = NULL,
        zscorePage = NULL,
        histogramPage = NULL,
        corrVolcanoPage = NULL,
            
        # initializer
        initialize = function(){
            self$lpdBoxplotPage = LpdBoxplotPage$new()
        },
        
        # UI
        ui = function(){
            dashboardBody(
                
            )
        },
        
        # server
        server = function(input, output, session){
        
        }
    )
)