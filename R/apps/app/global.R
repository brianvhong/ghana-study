bs4SidebarMenuItemWithOrder = function (..., tabName = NULL, tabOrder = NULL) {
    shiny::tags$li(
        class = "nav-item", 
        
        shiny::tags$a(
            class = "nav-link", 
            id = paste0("tab-", tabName), 
            href = paste0("#shiny-tab-", tabName), 
            `data-toggle` = "tab", 
            `data-value` = tabName, 
            
            shiny::tags$span(
                class = "nav-icon fa-stack",
                shiny::tags$strong(class = "fa-stack-1x", tabOrder)
            ), 
            shiny::tags$p(...)
        )
    )    
}

myFooter = shiny::tags$footer(
    id = "main-footer", 
    class = "text-center text-white bg-dark p-3",
    shiny::tags$p(
        shiny::tags$a(
            href = "http://zivkoviclab.ucdavis.edu",
            class = "",
            paste("Zivkovic Lab ©", format(Sys.Date(), "%Y"))
        )
    )
)

plotHistogram = function(data, title){
    ggplot(data, aes(pval)) +
        geom_histogram(binwidth = 0.025, color = "white", fill = "navyblue") +
        geom_vline(xintercept = 0.05, color = "brown3", 
                   linetype = "dashed", size = 1) +
        scale_x_continuous(breaks = seq(0, 1, 0.2)) +
        labs(x = "p value", title = title) +
        theme_bw() +
        theme(axis.text = element_text(color = "black", size = rel(1.25)),
              axis.title = element_text(size = rel(1.25)),
              plot.title = element_text(hjust = 0.5, size = rel(1.5)))
}

plotVolcano = function(zscore, var_add_vp, corr_method, vp_label_type){
    data = corr_atm$species[[corr_method]][[zscore]]
    data$avgExp = rowMeans(lpd$species$conc_table)
    data$feature_type = "species"
    data = rownames_to_column(data, "featureid")
    
    if(!is.null(var_add_vp)){
        add_data = corr_atm[[var_add_vp]][[corr_method]][[zscore]]
        add_data$avgExp = rowMeans(lpd[[var_add_vp]]$conc_table)
        add_data$feature_type = var_add_vp
        add_data = rownames_to_column(add_data, "featureid")
        data = rbind(data, add_data)
    }
    
    data$feature_type = factor(data$feature_type)
    data$feature_type = relevel(data$feature_type, ref = "species")
    
    p = ggplot(data, aes(x = estimate, y = -log10(pval))) +
        geom_vline(xintercept = 0.2, linetype = "dashed",
                   color = "grey") +
        geom_vline(xintercept = -0.2, linetype = "dashed",
                   color = "grey") +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed",
                   color = "grey")
    
    if(vp_label_type != "no") {
        if(vp_label_type == "r") {
            features_2_label = !between(data$estimate, -0.2, 0.2)
        } else if(vp_label_type == "p") {
            features_2_label = data$pval < 0.05
        } else if (vp_label_type == "both") {
            features_2_label = !between(data$estimate, -0.2, 0.2) & data$pval < 0.05
        }
        p = p + 
            geom_text_repel(data = subset(data, features_2_label),
                            aes(label = featureid), size = 4)       
    }
    
    if(is.null(var_add_vp)){
        p = p + geom_point(aes(alpha = avgExp), color = "navyblue")
    }else {
        p = p + geom_point(aes(alpha = avgExp, color = feature_type)) +
            scale_color_manual(values = c("navyblue", "firebrick"))
    }
    p = p +
        labs(x = "Correlation Coefficient",
             title = glue("Lipid species {str_to_title(corr_method)}'s correlation vs {zscore}")) +
        guides(alpha = guide_legend(title = "Relative\nAbundance")) +
        theme_bw() +
        theme(
            axis.text = element_text(color = "black", size = rel(1.25)),
            axis.title = element_text(size = rel(1.25)),
            plot.title = element_text(hjust = 0.5, size = rel(1.5))
        )
    p
}

