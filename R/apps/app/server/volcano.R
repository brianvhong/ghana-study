## waz
p_vol_waz = reactive({
    plotVolcano("waz18", input$var_add_vp, input$corr_method, input$show_all_av)
})

output$vol_waz = renderPlot({
    p_vol_waz()
})

output$waz_downloader = downloadHandler(
    filename = "waz18.png",
    content = function(file) {
        ggsave(
            file, p_vol_waz(), height = input$waz_ht, width = input$waz_wt, 
            dpi = 300, device = "png", units = "in"
        )
    }
)

## laz
p_vol_laz = reactive({
    plotVolcano("laz18", input$var_add_vp, input$corr_method, input$show_all_av)
})

output$vol_laz = renderPlot({
    p_vol_laz()
})

output$laz_downloader = downloadHandler(
    filename = "laz18.png",
    content = function(file) {
        ggsave(
            file, p_vol_laz(), height = input$laz_ht, width = input$laz_wt, 
            dpi = 300, device = "png", units = "in"
        )
    }
)

## wlz
p_vol_wlz = reactive({
    plotVolcano("wlz18", input$var_add_vp, input$corr_method, input$show_all_av)
})

output$vol_wlz = renderPlot({
    p_vol_wlz()
})

output$wlz_downloader = downloadHandler(
    filename = "wlz18.png",
    content = function(file) {
        ggsave(
            file, p_vol_wlz(), height = input$wlz_ht, width = input$wlz_wt, 
            dpi = 300, device = "png", units = "in"
        )
    }
)

## hcz
p_vol_hcz = reactive({
    plotVolcano("hcz18", input$var_add_vp, input$corr_method, input$show_all_av)
})

output$vol_hcz = renderPlot({
    p_vol_hcz()
})

output$hcz_downloader = downloadHandler(
    filename = "hcz18.png",
    content = function(file) {
        ggsave(
            file, p_vol_hcz(), height = input$hcz_ht, width = input$hcz_wt, 
            dpi = 300, device = "png", units = "in"
        )
    }
)
