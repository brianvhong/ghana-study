output$vol_waz = renderPlot({
    plotVolcano(
        data = corr_atm$species[[input$corr_method]]$waz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs waz")
    )
})

output$vol_laz = renderPlot({
    plotVolcano(
        data = corr_atm$species[[input$corr_method]]$laz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs laz")
    )
})

output$vol_wlz = renderPlot({
    plotVolcano(
        data = corr_atm$species[[input$corr_method]]$wlz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs wlz")
    )
})

output$vol_hcz = renderPlot({
    plotVolcano(
        data = corr_atm$species[[input$corr_method]]$hcz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs hcz")
    )
})
