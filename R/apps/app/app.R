pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase', "shiny", 
         "shinydashboard")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load('../../Rdata/lpd_precalc.rda')

ui <- dashboardPage(
    
    header = dashboardHeader(title = "Ghana Study"),
    sidebar = dashboardSidebar(
        
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
            menuItem("vs Anthropometric", tabName = "lpd_atm")
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
    
    lpd_boxplot_selector = reactive({
       rownames(lpd_limma())[input$lpd_statTable_rows_selected]
       
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

}

shinyApp(ui = ui, server = server)

