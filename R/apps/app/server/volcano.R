output$vol_waz = renderPlot({
    p = plotVolcano(
        data = corr_atm$species[[input$corr_method]]$waz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs waz")
    )
    if (!is.null(input$var_add_vp)) {
        p = addVar2Volcano(p, input$var_add_vp, input$corr_method, "waz18")
    }
    p
})

output$vol_laz = renderPlot({
    p = plotVolcano(
        data = corr_atm$species[[input$corr_method]]$laz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs laz")
    )
    if (!is.null(input$var_add_vp)) {
        p = addVar2Volcano(p, input$var_add_vp, input$corr_method, "laz18")
    }
    p
})

output$vol_wlz = renderPlot({
    p = plotVolcano(
        data = corr_atm$species[[input$corr_method]]$wlz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs wlz")
    )
    if (!is.null(input$var_add_vp)) {
        p = addVar2Volcano(p, input$var_add_vp, input$corr_method, "wlz18")
    }
    p
})

output$vol_hcz = renderPlot({
    p = plotVolcano(
        data = corr_atm$species[[input$corr_method]]$hcz18,
        title = glue("Lipid species {str_to_title(input$corr_method)}'s correlation vs hcz")
    )
    if (!is.null(input$var_add_vp)) {
        p = addVar2Volcano(p, input$var_add_vp, input$corr_method, "hcz18")
    }
    p
})
