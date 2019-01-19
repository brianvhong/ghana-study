lpd_atm_table = reactive({
    corr_atm[[input$level]][[input$corr_method]][[input$lpd_atm]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        mutate(
            stat = round(stat, digits = 3),
            estimate = round(estimate, digits = 3),
            pval = round(pval, digits = 3),
            padj = round(padj, digits = 3)
        ) %>%
        column_to_rownames("Feature")
})

output$lpd_atm_dt = DT::renderDataTable(
    lpd_atm_table(),
    selection = list(mode = "single", selected = 1),
    server=T
)

lpd_atm_selector = reactive({
    rownames(lpd_atm_table())[input$lpd_atm_dt_rows_selected]
})

output$lpd_atm_scatter = renderPlotly({
    data.frame(
        atm = lpd[[input$level]]$sample_table[,input$lpd_atm],
        lpd = lpd[[input$level]]$conc_table[lpd_atm_selector(),],
        flipgroup = lpd[[input$level]]$sample_table$flipgroup,
        sex_updated = lpd[[input$level]]$sample_table$sex_updated
    ) %>%
        ggplot(aes(x=lpd, y=atm)) +
        geom_point(aes_string(color=input$lpd_atm_color)) +
        stat_smooth(method = "lm") +
        labs(
            x = lpd_atm_selector(),
            y = input$lpd_atm
        ) +
        theme_bw()
})