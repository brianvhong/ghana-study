GlcEnrichment = R6Class(
    "GlcEnrichment",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "glc-enrich",
        description = strwrap("
        The enrichment analysis is done using the hypergeomatric distribution. 
        N is the total number of glycopeptides; m is the total number of glycopeptides 
        of a particular protein; n is the number of glycopeptides of any proteins
        other than a specifid one; k is the total number of glycopeptides that are
        increased (or decreased) in LNS; x is the number of glycopeptides of the
        particular protein that increased (or decreased) in LNS. The P value can
        be interpreted as the probability that having x number of glycopeptides
        increased (or decreased) is by chance.
        ", width = 10000, simplify = TRUE),
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                column(
                    width = 6,
                    box(
                        width = NULL,
                        title = "Description",
                        collapsible = TRUE,
                        tags$p(self$description)
                    ),
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
                    self$enrichment(props$data, props$direction),
                    selection = list(mode = "single", selected = 1),
                    options = list(
                        order = list(6, "asc")
                    )
                ) %>%
                    formatSignif(columns = 6, digits = 4)
            })
            
            output$plot = renderPlotly({
                protein = unique(props$data$feature_data$Protein)[input$table_rows_selected]
                self$plot(props$data, protein, props$direction)
            })
            
        },
        
        enrichment = function(mset, direction){
            compare = switch(
                direction,
                "increase" = `>`,
                "decrease" = `<`
            )
            res = lapply(unique(mset$feature_data$Protein), function(prot){
                N = nfeatures(mset)
                m = sum(mset$feature_data$Protein == prot)
                n = N - m
                df = mset$conc_table %>% t %>% as.data.frame %>%
                    mutate(
                        flipgroup = mset$sample_table$flipgroup
                    ) %>%
                    melt(id.vars = "flipgroup") %>%
                    group_by(flipgroup, variable) %>%
                    summarise(value = mean(value)) %>%
                    ungroup() %>%
                    dcast(variable ~ flipgroup) %>%
                    mutate(value = cLNS - aIFA) %>%
                    filter(compare(value, 0))
                k = nrow(df)
                x = df %>%
                    mutate(protein = mset$feature_data[variable,"Protein"]) %>%
                    filter(protein == prot) %>%
                    nrow()
                p = phyper(q=x-1, m=m, n=n, k=k, lower.tail=FALSE)
                res = c(N, m, n, k, x, p)
                names(res) = c("N", "m", "n", "k", "x", "p.value")
                return(res)
            })
            res = do.call(rbind, res)
            rownames(res) = unique(mset$feature_data$Protein)
            return(res)
        },
        
        plot = function(mset, prot, direction){
            compare = switch(
                direction,
                "increase" = `>`,
                "decrease" = `<`
            )
            mset$conc_table %>%
                t %>% as.data.frame %>%
                mutate(flipgroup = mset$sample_table$flipgroup) %>%
                mutate(flipgroup = factor(flipgroup, levels = c("cLNS", "aIFA"))) %>%
                melt(id.vars = "flipgroup") %>%
                mutate(protein = mset$feature_data[variable, "Protein"]) %>%
                filter(protein == prot) %>%
                mutate(variable = gsub(" Results$", "", variable)) %>%
                group_by(variable, flipgroup) %>%
                summarize(mean = mean(value), stderr = sd(value)/log(length(value))) %>%
                ungroup() %>%
                melt(id.vars = c("variable", "flipgroup"), variable.name = "valuevar") %>%
                dcast(variable ~ flipgroup  + valuevar) %>%
                arrange(cLNS_mean) %>%
                mutate(change = cLNS_mean - aIFA_mean) %>%
                mutate(variable = factor(variable, levels = variable)) %>%
                melt(id.vars = c("variable", "change"), variable.name = "mixed_variable") %>%
                tidyr::separate(col = "mixed_variable", sep = "_", into = c("flipgroup", "valuevar")) %>%
                dcast(variable + flipgroup + change ~ valuevar) %>%
                ggplot(aes(x = variable, y = mean, fill = flipgroup, alpha = compare(change, 0))) +
                geom_col(position = "dodge") +
                geom_errorbar(aes(ymin = mean - stderr, ymax = mean + stderr), position = position_dodge(0.9)) +
                #scale_y_log10() +
                scale_alpha_manual(values = c(1, 0.3)) +
                guides(alpha = guide_legend(title = direction)) +
                coord_flip()
        }
    )
)