output$hist_waz = renderPlot({
    plotHistogram(
        data = corr_atm$species[[input$corr_method]]$waz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs waz")
    )
})

output$hist_laz = renderPlot({
    plotHistogram(
        data = corr_atm$species[[input$corr_method]]$laz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs laz")
    )
})

output$hist_wlz = renderPlot({
    plotHistogram(
        data = corr_atm$species[[input$corr_method]]$wlz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs wlz")
    )
})

output$hist_hcz = renderPlot({
    plotHistogram(
        data = corr_atm$species[[input$corr_method]]$hcz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs hcz")
    )
})