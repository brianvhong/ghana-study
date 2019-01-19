output$lpd_boxPlot <- renderPlotly({
    
    df = data.frame(
        conc  = lpd[[input$level]]$conc_table[lpd_boxplot_selector(),], 
        group = lpd[[input$level]]$sample_table$flipgroup,
        wid = lpd[[input$level]]$sample_table$wid
    )
    
    p = ggplot(df, aes(x = group, y = conc, wid = wid)) +
        geom_boxplot(outlier.color = NA) +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.7, 
                   shape = 21, color = "white", fill = "black", size = 2.5) +
        xlab("group") +
        ylab("conc") +
        ggtitle("boxplot") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
    
})

lpd_limma = reactive({
    limma_list[[input$level]] %>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        mutate(
            baseMean = round(baseMean, digits = 3),
            logFC = round(logFC, digits = 3),
            stat = round(stat, digits = 3),
            pvalue = round(pvalue, digits = 3),
            padj = round(padj, digits = 3)
        ) %>%
        column_to_rownames("Feature")
})

output$lpd_statTable = DT::renderDataTable(
    lpd_limma(),
    selection = list(mode = "single", selected = 1),
    server=T
)

