BoxplotPage = R6Class(
    "BoxplotPage",
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
        server = function(input, output, session, props){
            output$table = renderDT({
                datatable(
                    props$lm,
                    selection = list(mode = "single", selected = 1),
                    options = list(
                        order = list(4, "asc")
                    )
                ) %>%
                    formatSignif(columns = 1:5, digits = 4)
            })
            output$plot = renderPlotly({
                feature = featureNames(props$data)[input$table_rows_selected]
                plot_boxplot(props$data, x = "flipgroup", feature = feature) +
                    labs(y = "")
            })
        }
    )
)