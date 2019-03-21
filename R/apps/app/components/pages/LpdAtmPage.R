import::here(DataTable, .from="../modules/DataTable.R")
import::here(ScatterPlot, .from="../modules/ScatterPlot.R")

LpdAtmPage = R6Class(
    "LpdAtmPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "lpd-atm",
        dataTable = NULL,
        scatterPlot = NULL,
        # initializer
        initialize = function(){
            self$dataTable = DataTable$new("data-table", self$id)
            self$scatterPlot = ScatterPlot$new("scatter", self$id)
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tabItem(
                tabName = "lpd-atm",
                tags$div(
                    class = "col-sm-6",
                    box(
                        width = NULL,
                        self$dataTable$ui()
                    )
                ),
                tags$div(
                    class = "col-sm-6",
                    box(
                        width = NULL,
                        self$scatterPlot$ui()
                    )
                )
            )
        },
        
        # server
        #' @props tabs: string, the current sidbar tabmae
        #' @props lpd-level: string, the lpd level
        #' @props lpd-method: string, the correlation method
        #' @props atm: string the atm variable
        server = function(input, output, session, props){
            states = reactiveValues(
                featureSelected = NULL
            )
            observe({
                if(
                    !is.null(props$`lpd-level`) & 
                    !is.null(props$`corr-method`) &
                    !is.null(props$atm)
                ){
                    data = corr_atm[[props$`lpd-level`]][[props$`corr-method`]][[props$atm]]
                    tableData = self$dataTable$call(props = reactiveValues(
                        data = data
                    ))
                    observeEvent(tableData$rows_selected, {
                        states$featureSelected= rownames(data)[tableData$rows_selected]
                        logjs(states$featureSelected)
                    })
                }
            })
            observeEvent(states$featureSelected, {
                if(!is.null(states$featureSelected)){
                    data = data.frame(
                        x = as.numeric(lpd[[props$`lpd-level`]]$conc_table[states$featureSelected,]),
                        y = lpd$species$sample_table[, props$atm],
                        wid = lpd$species$sample_table$wid,
                        flipgroup = lpd$species$sample_table$flipgroup
                    )
                    args = list(
                        x = "x", y = "y", color = "flipgroup", wid = "wid"
                    )
                    labs = list(
                        x = states$featureSelected,
                        y = props$atm
                    )
                    self$scatterPlot$call(props = reactiveValues(
                        data = data,
                        args = args,
                        labs = labs
                    ))
                }
            })
        },
        
        call = function(input, output, session, props){
            callModule(self$server, self$id, props)
        }
    )
)