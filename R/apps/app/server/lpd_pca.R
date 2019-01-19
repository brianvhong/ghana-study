lpd_boxplot_selector = reactive({
    rownames(lpd_limma())[input$lpd_statTable_rows_selected]
})

output$lpd_pca = renderPlotly({
    
    data = t(lpd$species$conc_table[limma_list$species$pvalue <= input$pca_cutoff,])
    log.data = log(data)
    data.pca = prcomp(log.data,
                      center = TRUE,
                      scale. = TRUE)
    
    g = data.frame(
        PC1 = data.pca$x[, 1],
        PC2 = data.pca$x[, 2],
        treatment = lpd$species$sample_table$flipgroup,
        wid = lpd$species$sample_table$wid
    ) %>%
        ggplot(aes(x = PC1, y = PC2)) +
        geom_point(aes(color = treatment,wid = wid)) +
        theme_bw()
    
    ggplotly(g)
    
})