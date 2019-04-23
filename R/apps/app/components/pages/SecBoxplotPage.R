import::here(DataTable, .from="../modules/DataTable.R")
import::here(BoxPlot, .from="../modules/BoxPlot.R")

SecBoxplotPage = R6Class(
    "SecBoxplotPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "sec-boxplot",
        dataTable = NULL,
        boxPlot = NULL,
        
        # initializer
        initialize = function(){
            self$dataTable = DataTable$new(id = "table", parent_id = self$id)
            self$boxPlot = BoxPlot$new(id = "boxplot", parent_id = self$id)
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tabItem(
                tabName = "sec-boxplot",
                div(
                    class="col-sm-6",
                    box(
                        width = NULL,
                        title = "Stats Result",
                        self$dataTable$ui()
                    )
                ),
                div(
                    class="col-sm-6",
                    box(
                        width = NULL,
                        title = "Box Plot",
                        self$boxPlot$ui()
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session, props){
            states = reactiveValues(
                rows_selected = NULL
            )
            tableData = self$dataTable$call(props = reactiveValues(
                data = data$lm$sec
            ))
            observeEvent(tableData$rows_selected, {
                states$rows_selected = tableData$rows_selected
            })
            observeEvent(states$rows_selected, {
                if(!is.null(states$rows_selected)){
                    # Boxplot
                    sec = data$data$sec
                    data = data.frame(
                        x = sec$sample_table$flipgroup,
                        y = as.numeric(sec$conc_table[states$rows_selected,]),
                        wid = sec$sample_table$wid,
                        waz = sec$sample_table$waz18,
                        laz = sec$sample_table$laz18,
                        wlz = sec$sample_table$wlz18,
                        hcz = sec$sample_table$hcz18,
                        chole_efflux = sec$sample_table$chol_efflux
                    )
                    args = list(
                        x = "x", y = "y", wid = "wid", waz = "waz",
                        laz = "laz", wlz = "wlz", hcz = "hcz", 
                        chole_efflux = "chole_efflux"
                    )    
                    labs = list(
                        x = "", y = "", 
                        title = featureNames(sec)[states$rows_selected]
                    )
                    self$boxPlot$call(props = reactiveValues(
                        data = data,
                        args = args,
                        labs = labs
                    ))
                }
            })
        },
        
        # call
        call = function(input, output, session, props){
            callModule(self$server, self$id, props)
        }
    )
)