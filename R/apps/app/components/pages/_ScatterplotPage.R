ScatterplotPage = R6Class(
    "ScatterplotPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = NULL,
        
        # initializer
        initialize = function(id){
            self$id = id
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                column(
                    width = 6,
                    box(
                        width = NULL,
                        DTOutput(ns("table"))
                    )
                ),
                column(
                    width = 6,
                    box(
                        width = NULL,
                        plotlyOutput(ns("plot"))
                    )
                )
            )
        },
        
        # server
        #' @props corr
        server = function(input, output, session, props){
            output$table = renderDT({
                datatable(
                    props$corr,
                    selection = list(mode = "single", selected = 1),
                    options = list(
                        order = list(3, "asc")
                    )
                ) %>%
                    formatSignif(columns = 1:5, digits = 4)
            })
            output$plot = renderPlotly({
                x = featureNames(props$X)[input$table_rows_selected]
                data.frame(
                    x = props$X$conc_table[x,],
                    y = props$Y$conc_table[props$y_var,],
                    flipgroup = props$X$sample_table$flipgroup
                ) %>%
                    ggscatterplot("x", "y", color = "flipgroup", point.size = 1.5) +
                    labs(
                        x = x,
                        y = props$y_var
                    )
            })
        }
    )
)