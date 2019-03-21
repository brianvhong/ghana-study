import::here(DataTable, .from = "../modules/DataTable.R")
import::here(BoxPlot, .from = "../modules/BoxPlot.R")

LpdBoxplotPage = R6Class(
    "LpdBoxplotPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "lpd-boxplot",
        dataTable = NULL,
        boxPlot = NULL,
        
        # initializer
        initialize = function(){
            self$dataTable = DataTable$new("table", parent_id = self$id)
            self$boxPlot = BoxPlot$new("boxplot", parent_id = self$id)
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tabItem(
                tabName = "lpd-boxplot",
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
        #' @props lpd-level, string
        server = function(input, output, session, props){
            states = reactiveValues(
                rows_selected = NULL
            )
            observeEvent(props$`lpd-level`, {
                if(!is.null(props$`lpd-level`)) {
                    tableData = self$dataTable$call(props = reactiveValues(
                        data = limma_list[[props$`lpd-level`]]
                    ))
                    observeEvent(tableData$rows_selected, {
                        states$rows_selected = tableData$rows_selected
                    })
                }
            })
                
            observeEvent(states$rows_selected, {
                logjs(states$rows_selected)
                if(!is.null(states$rows_selected)){
                    data = data.frame(
                        x = lpd$class$sample_table$flipgroup,
                        y = as.numeric(lpd[[props$`lpd-level`]]$conc_table[states$rows_selected,]),
                        wid = lpd$class$sample_table$wid,
                        waz = lpd$class$sample_table$waz18,
                        laz = lpd$class$sample_table$laz18,
                        wlz = lpd$class$sample_table$wlz18,
                        hcz = lpd$class$sample_table$hcz18,
                        chole_efflux = lpd$class$sample_table$chol_efflux
                    )
                    args = list(
                        x = "x", y = "y", wid = "wid", waz = "waz",
                        laz = "laz", wlz = "wlz", hcz = "hcz", 
                        chole_efflux = "chole_efflux"
                    )    
                    labs = list(
                        x = "", y = "", 
                        title = featureNames(lpd[[props$`lpd-level`]])[states$rows_selected]
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
        call = function(input, output, session, props) {
            callModule(self$server, self$id, props)
        }
    )
)