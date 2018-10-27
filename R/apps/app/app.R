pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase', "shiny", 
         "shinydashboard", "ggsci")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load('../../Rdata/lpd_precalc.rda')

ui <- dashboardPage(
    
    header = dashboardHeader(title = "Ghana Study"),
    sidebar = dashboardSidebar(
        ## A home button
        sidebarMenu(
            menuItem("Home", icon = icon("home"), newtab = FALSE,
                     href="http://www.chenghaozhu.net/studies/ghana/")
        ),
        ## input
        selectInput("level", "Select Lipid Level:", 
                    choices = names(lpd), selected = "class"),
        selectInput("corr_method", "Select Correlation Method:",
                    choices = names(corr_atm$class),
                    selected = names(corr_atm$class)[1]),
        ## side bar menue
        sidebarMenu(
            id = "sidebar",
            menuItem("Boxplot", tabName = "lpd_boxplot"),
            menuItem("PCA", tabName = "lpd_pca"),
            menuItem("vs Anthropometric", tabName = "lpd_atm"),
            menuItem("Categorial Z scores", tabName = "lpd_zscore")
        )
    ),
    
    body = dashboardBody(
        
        ## CSS sheet
        tags$head(
            tags$link(rel="stylesheet", type = "text/css", href = "styles.css")
        ),
        
        ## main body layout
        tabItems(
            ## Boxplot page layout
            tabItem(
                tabName = "lpd_boxplot",
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL,
                            DT::dataTableOutput("lpd_statTable"), height = "100%")
                    ),
                    column(
                        width = 6,
                        box(width = NULL,
                            plotlyOutput("lpd_boxPlot"))
                    )
                )
            ),
            ## PCA page layout
            tabItem(
                tabName = "lpd_pca",
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL,
                            plotlyOutput("lpd_pca"))
                    ),
                    column(
                        width = 6,
                        box(width = NULL,
                            sliderInput("pca_cutoff", "Select a p value cutoff: ",
                                        min = 0, max = 1, value = 0.1, step = 0.05))
                    )
                )
            ),
            ## Correlation page layout
            tabItem(
                tabName = "lpd_atm",
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL,
                            DT::dataTableOutput("lpd_atm_dt"), height = "100%")
                    ),
                    column(
                        width = 6,
                        box(width = NULL,
                            selectInput("lpd_atm", "Select a Anthropometric Variable",
                                        choices = names(corr_atm$class$pearson),
                                        selected = names(corr_atm$class$pearson)[1]),
                            selectInput("lpd_atm_color", "Select a variable to color it",
                                        choices = c("flipgroup", "sex_updated"))),
                        box(width = NULL,
                            plotlyOutput("lpd_atm_scatter"))
                    )
                )
            ),
            ## Categorical Z score test page layout
            tabItem(
                tabName = "lpd_zscore",
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL,
                            DT::dataTableOutput("lpd_zscore_dt"), height = "100%"),
                        uiOutput("zscore_volcano_ui")
                    ),
                    column(
                        width = 6,
                        box(width = NULL,
                            column(
                                width = 5,
                                selectInput("zscore_var", "Select a Z score variable:",
                                            choices = c("waz18", "laz18", "wlz18", "hcz18"),
                                            selected = "waz18")),
                            column(
                                width = 5,
                                numericInput("zscore_cutoff", "Input a Z score cutoff:",
                                             min = -2, max = 2, step = 0.1, value = -1.9)),
                            column(
                                width = 2,
                                checkboxInput("zscore_show_points", "Show Points",
                                              value = TRUE)
                            )),
                        box(width = NULL,
                            plotlyOutput("lpd_zscore_boxplot")),
                        uiOutput("zscore_hist_ui")
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
   
## -------- boxplot ------------------------------------------------------------
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

## -------- PCA ----------------------------------------------------------------    
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

## -------- vs anthropometric data ---------------------------------------------
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

## -------- Categorical Z Scores -----------------------------------------------
    
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
}

shinyApp(ui = ui, server = server)

