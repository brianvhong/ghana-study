zscore_mset = reactive({
    mset = lpd[[input$level]]
    zscore = mset$sample_table[,input$zscore_var]
    zscore = ifelse(zscore <= input$zscore_cutoff, "Low", "High")  
    zscore = factor(zscore, levels = c("Low", "High"))
    mset$sample_table$zscore = zscore
    mset
})

zscore_limma = reactive({
    mset = zscore_mset()
    design = model.matrix(~zscore + 1, data = as(mset$sample_table, "data.frame"))
    mSet_limma(mset, design, coef = 2, p.value = 2) 
})

output$lpd_zscore_dt = DT::renderDataTable(
    
    zscore_limma() %>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        mutate(baseMean = round(baseMean, 3),
               logFC    = round(logFC, 3),
               stat     = round(stat, 3),
               pvalue   = round(pvalue, 3),
               padj     = round(padj, 3)) %>%
        column_to_rownames("Feature"),
    
    selection = list(mode = "single", selected = 1),
    server=T
)

zscore_selector = reactive({
    rownames(zscore_limma())[input$lpd_zscore_dt_rows_selected]
})

output$lpd_zscore_boxplot = renderPlotly({
    mset = zscore_mset()
    df = data.frame(
        feature = mset$conc_table[zscore_selector(),],
        zscore = mset$sample_table$zscore
    )
    p = ggplot(df, aes(x=zscore, y=feature))
    if (input$zscore_show_points) {
        p = p + geom_boxplot() +
            geom_point(position = position_jitter(width = 0.2), alpha = 0.7, 
                       shape = 21, color = "white", fill = "black", size = 2.5)
    } else {
        p = p + geom_boxplot(aes(fill=zscore)) +
            scale_fill_lancet()
    }
    p = p + theme_bw() +
        theme(legend.position = "none")
})

## Histograme
output$zscore_hist = renderPlotly({
    if(input$level != "species") {
        return()
    }
    zscore_limma() %>%
        ggplot(aes(pvalue)) +
        geom_histogram(bins = 40, color = "white") +
        theme_bw()
})

output$zscore_hist_ui = renderUI({
    if(input$level != "species") return()
    box(width=NULL,
        plotlyOutput("zscore_hist"))
})

## Volcano plot
output$zscore_volcano = renderPlotly({
    if(input$level != "species") return()
    zscore_limma() %>%
        rownames_to_column("feature") %>%
        ggplot(aes(logFC, -log(pvalue))) +
        geom_point(aes(feature = feature), color = "gray19", alpha=0.5) +
        geom_hline(yintercept = 4, linetype="dashed", color="firebrick") +
        theme_bw()
})

output$zscore_volcano_ui = renderUI({
    if(input$level != "species") return()
    box(width=NULL,
        plotlyOutput("zscore_volcano"))
})