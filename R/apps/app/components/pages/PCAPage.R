PCAPage = R6Class(
    "PCAPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "pca",
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tabItem(
                tabName = "lpd-pca",
                tags$div(
                    class = "col-sm-6",
                    box(
                        width = NULL,
                        title = "Settings",
                        # This input defines the scaling method to apply
                        radioButtons(
                            ns("scale"),
                            label = "Scaling method",
                            choices = c(
                                "none",
                                "z-score scale",
                                "absolute scale"
                            ),
                            inline = TRUE
                        ),
                        # Whether to draw the ellipses
                        tags$div(
                            class = "row",
                            column(
                                width = 6,
                                checkboxInput(
                                    ns("ellipse"), "Draw ellipse", value = FALSE
                                )
                            )
                        ),
                        # P value cutoff
                        tags$div(
                            class="col-md-6",
                            numericInput(
                                ns("cutoff"), "P value cutoff",
                                min = 0, max = 1, value = 0.2, step = 0.01
                            )
                        )
                    )
                ),
                
                tags$div(
                    class = "col-sm-6",
                    box(
                        width = NULL,
                        title = "PCA Plot",
                        plotlyOutput(ns("plot"))
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            observe({
                pca = self$runPrcomp(scale = input$scale, cutoff = input$cutoff)
                output$plot = renderPlotly({
                    df = data.frame(
                        PC1 = pca$x[,"PC1"],
                        PC2 = pca$x[,"PC2"],
                        wid = lpd$species$sample_table$wid,
                        flipgroup = lpd$species$sample_table$flipgroup
                    )
                    sdev = pca$sdev ^ 2 / sum(pca$sdev ^ 2)
                    p = ggplot(df, aes(x = PC1, y = PC2, color = flipgroup)) +
                        geom_point()
                    if(input$ellipse) {
                        p = p + stat_ellipse()
                    }
                    p = p + labs(
                        x = glue("PC1 [{round(sdev[1], 4)* 100}%]"),
                        y = glue("PC1 [{round(sdev[2], 4)* 100}%]")
                    )
                })
            })
        },
        
        runPrcomp = function(scale, cutoff){
            scale_fun = switch(
                scale,
                "none" = I,
                "z-score scale" = self$zscoreScale,
                "absolute scale" = self$absoluteScale
            )
            X = lpd$species$conc_table
            X = X[limma_list$species$pvalue <= cutoff,]
            X = scale_fun(t(X))
            pca = prcomp(X)
            return(pca)
        },
        
        zscoreScale = function(mat){
            apply(mat, 2, scale)
        },
        
        absoluteScale = function(mat){
            apply(mat, 2, function(col){
                max = max(col, na.rm = TRUE)
                min = min(col, na.rm = TRUE)
                ((col - min) / (max - min) - 0.5) * 2
            })
        }
    )
)