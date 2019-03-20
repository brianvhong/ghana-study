ShinyModule = R6Class(
    "ShinyModule",
    public = list(
        # attributes
        id = NULL,
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
        
        },
        
        # server
        server = function(input, output, session){
        
        },
        
        call = function(input, ouput, seesion) {
            callModule(self$server, self$id)
        }
    )
)