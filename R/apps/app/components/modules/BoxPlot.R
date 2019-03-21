BoxPlot = R6Class(
    "BoxPlot",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = NULL,
        parent_id = NULL,
        # initializer
        initialize = function(id, parent_id){
            self$id = id
            self$parent_id = parent_id
        },
        
        # UI
        ui = function(){
            ns = NS(NS(self$parent_id)(self$id))
            tagList(
                plotlyOutput(ns("plot"))
            )
        },
        
        # server
        #' @props data: a data frame
        #' @props args: a list of variables to be pasrsed to aes_string
        #' @props labs: a list of lab and plot titles
        server = function(input, output, session, props){
            observeEvent(props$data, {
                if(!is.null(props$data)) {
                    logjs(props$data)
                    output$plot = renderPlotly({
                        p = ggplot(props$data, do.call(aes_string, props$args)) +
                            geom_boxplot() +
                            geom_point()
                        if(!is.null(props$labs$x)) {
                            p = p + labs( x = props$labs$x )
                        }
                        if(!is.null(props$labs$y)) {
                            p = p + labs( y = props$labs$y )
                        }
                        if(!is.null(props$labs$title)) {
                            p = p + labs( title = props$labs$title )
                        }
                        p
                    })
                }
            })
        },
        
        call = function(input, output, session, props){
            callModule(self$server, self$id, props)
        }
    )
)