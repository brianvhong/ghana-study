DataTable = R6Class(
    "DataTable",
    inherit = ShinyModule,
    public = list(
        # attributes
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
                dataTableOutput(ns("data-table"))
            )
        },
        
        # server
        #' @props data: data.frame
        #' @emit rows_selected: numeric
        server = function(input, output, session, props){
            emit = reactiveValues(
                rows_selected = NULL
            )
            observe({
                logjs(props$data)
                output$`data-table` = renderDataTable({
                    datatable(
                        props$data,
                        selection = list(mode = "single", selected = 1),
                        options = list(
                            order = list(4, "asc")
                        )
                    ) %>%
                        formatSignif(columns = 1:5, digits = 4)
                }) 
            })
            
            observeEvent(input$`data-table_rows_selected`, {
                emit$rows_selected = input$`data-table_rows_selected`
            })
            
            return(emit)
        },
        
        # call
        call = function(input, output, session, props){
            callModule(self$server, self$id, props)
        }
    )
)