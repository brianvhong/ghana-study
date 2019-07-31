import::here(DataTable, .from="../modules/DataTable.R")

ChromatogramPage = R6Class(
    "ChromatogramPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "sec-chrom",
        dataTable = NULL,
        
        # initializer
        initialize = function(){
            self$dataTable = DataTable$new("table", self$id)
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            
            tabItem(
                tabName = "sec-chrom",
                div(
                    class="col-sm-6",
                    box(
                        width = NULL,
                        title = "Sample Info",
                        self$dataTable$ui()
                    )
                ),
                div(
                    class="col-sm-6",
                    box(
                        width = NULL,
                        title = "SEC Chromatogram",
                        plotlyOutput(ns("chrom"))
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            states = reactiveValues(
                sample_selected = NULL
            )
            tableData = self$dataTable$call(props = reactiveValues(
                data = data$data$sec$fractions$sample_table
            ))
            observeEvent(tableData$rows_selected, {
                states$sample_selected = sampleNames(data$data$sec$fractions)[tableData$rows_selected]
                print(states$sample_selected)
            })
            
            output$chrom = renderPlotly({
                chr = data$chr[[states$sample_selected]]
                uv = chr$uv
                fr = chr$fr
                fr = filter(fr, fraction %in% as.character(1:8))
                fr$mAU = sapply(fr$ml, function(x){
                    fit = lm(mAU ~ ml + 1, data = uv[order(abs(uv$ml - x))[1:2],])
                    predict(fit, data.frame(ml = x))
                })

                ggplot(uv) +
                    geom_line(aes(x = ml, y = mAU)) +
                    geom_segment(
                        data = fr, aes(x = ml, xend = ml, y = mAU, yend = 0),
                        linetype = "dashed", color = "#E35F5F", 
                    ) +
                    geom_text(data = fr[1:7, ], aes(x = ml + 0.5, y = 0, label = fraction))
            })
        }
    )
)