BoxplotPage = R6Class(
    "BoxplotPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = NULL,
        show_extraplots = FALSE,
        
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
                ),
                uiOutput(ns("extraplots"))
            )
        },
        
        # server
        server = function(input, output, session, props){
            
            observeEvent(props$show_extraplots, {
                self$show_extraplots = props$show_extraplots
            })
            
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
            output$extraplots = renderUI({
                if(self$show_extraplots){
                    tagList(
                        column(
                            width = 6,
                            box(
                                width = NULL,
                                plotlyOutput(session$ns("hist"))
                            ),
                            box(
                                width = NULL,
                                plotlyOutput(session$ns("volcano"))
                            )
                        )
                    )
                }
            })
            output$volcano = renderPlotly({
                props$lm %>%
                    ggplot() +
                    geom_point(aes(x = logFC, y = -log(pvalue), color = pvalue < 0.05, alpha = pvalue < 0.05)) +
                    geom_hline(yintercept = -log(0.05), linetype = "dashed") +
                    scale_color_manual(values = c("gray30", "red")) +
                    scale_alpha_manual(values = c(0.4, 1)) +
                    theme_bw()
            })
            output$hist = renderPlotly({
                props$lm %>%
                    ggplot() +
                    geom_histogram(aes(x = pvalue), color = "white", fill = "gray18", binwidth = 0.025) +
                    theme_bw()
            })
        }
    )
)