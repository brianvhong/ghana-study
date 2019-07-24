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
        
        call = function(input, output, session, props){
            if(missing(props)){
                callModule(self$server, self$id)
            } else {
                callModule(self$server, self$id, props)   
            }
        }
    )
)