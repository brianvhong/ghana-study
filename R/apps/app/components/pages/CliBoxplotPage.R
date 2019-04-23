import::here(DataTable, .from="../modules/DataTable.R")
import::here(BoxPlot, .from="../modules/BoxPlot.R")

CliBoxplotPage = R6Class(
    "CliBoxplotPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "cli-boxplot",
        dataTable = NULL,
        boxPlot = NULL,
        # initializer
        initialize = function(){
            self$dataTable = DataTable$new("data-table", self$id)
            self$boxPlot = BoxPlot$new("boxplot", self$id)
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tabItem(
                tabName = "cli-boxplot",
                tags$div(
                    class="col-sm-6",
                    box(
                        width = NULL,
                        title = "Stats Result",
                        radioButtons(
                            ns("alternative"), label = NULL,
                            choices = c("two.sided", "less", "greater"),
                            inline = TRUE
                        ),
                        tags$hr(),
                        self$dataTable$ui()
                    )
                ),
                tags$div(
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
        server = function(input, output, session){
            states = reactiveValues(
                rows_selected = NULL
            )
            observeEvent(input$alternative, {
                tableData = self$dataTable$call(props = reactiveValues(
                    data = self$runTTest(input$alternative)
                ))
                observeEvent(tableData$rows_selected, {
                    states$rows_selected = tableData$rows_selected
                })
            })
            observeEvent(states$rows_selected, {
                if(!is.null(states$rows_selected)){
                    lpd = data$data$lpd
                    variable = c("waz18", "laz18", "wlz18", "hcz18", "chol_efflux")[states$rows_selected]
                    data = data.frame(
                        x = lpd$species$sample_table$flipgroup,
                        y = lpd$species$sample_table[, variable],
                        wid = lpd$species$sample_table$wid
                    )
                    args = list(x = "x", y = "y", wid = "wid")
                    labs = list(x = "", y = "", title = variable)
                    self$boxPlot$call(props = reactiveValues(
                        data = data,
                        args = args,
                        labs = labs
                    ))
                }
            })
        },
        
        runTTest = function(alternative){
            lpd = data$data$lpd
            df = lpd$species$sample_table[,c("waz18", "laz18", "wlz18", "hcz18", "chol_efflux")]
            flipgroup = lpd$species$sample_table$flipgroup
            
            ttest = lapply(df, function(col){
                res = t.test(
                    col ~ flipgroup + 1, 
                    alternative = alternative
                )
                return(c(
                    statistic = as.numeric(res$statistic),
                    baseMean = as.numeric(res$estimate[1]),
                    logFC = as.numeric(log2(res$estimate[2]/res$estimate[1])),
                    p.value = res$p.value
                ))
            })
            return(do.call(rbind, ttest))
        }
    )
)