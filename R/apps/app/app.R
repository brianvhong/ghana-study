pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase', "shiny", 
         "shinydashboard", "stats")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load('../../Rdata/lpd_precalc.Rdata')

ui <- dashboardPage(
    
    header = dashboardHeader(title = "Ghana Study"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            id = "sidebar",
            menuItem(
                icon = icon("caret-right"), "Lipidome", 
                menuSubItem("Boxplot", tabName = "lpd_boxplot"),
                menuSubItem("PCA", tabName = "lpd_hist")
                selectInput("level", "Select from here", 
                    choices = names(lpd), selected = "class"),
                sliderInput("p value", "Select",
                    min = 0, max = 1, value = 0.1, step = 0.5)
            )
        )
    ),
    
    body = dashboardBody(
        tags$head(
            tags$link(rel="stylesheet", type = "text/css", href = "styles.css")
        ),
        fluidRow(
            column(
                width = 6,
                box(width = NULL,
                    DT::dataTableOutput("statTable"), height = "100%")
            ),
            column(
                width = 6,
                box(width = NULL,
                    plotlyOutput("boxPlot"))
            )
        )
    )
)

server <- function(input, output) {
   
   output$boxPlot <- renderPlotly({
       
       df = data.frame(
           conc  = lpd[[input$level]]$conc_table[feature_selector(),], 
           group = lpd[[input$level]]$sample_table$flipgroup,
           wid = lpd[[input$level]]$sample_table$wid
       )
       
       p = ggplot(df, aes(x = group, y = conc, wid = wid)) +
           geom_boxplot(outlier.color = NA) +
           geom_point(position = position_jitter(width = 0.2), alpha = 0.7, shape = 21, color = "white", fill = "black", size = 2.5) +
           xlab("group") +
           ylab("conc") +
           ggtitle("boxplot") +
           theme_bw() +
           theme(plot.title = element_text(hjust = 0.5))
       
       ggplotly(p)
       
   })
   
   stats_table = reactive({
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
   
   output$statTable = DT::renderDataTable(
       stats_table(),
       selection = list(mode = "single", selected = 1),
       server=T
   )
   
   feature_selector = reactive({
       rownames(stats_table())[input$statTable_rows_selected]
       
   })
   
   data = t(lpd$species$conc_table)
   
   log.data = log(data)
   data.pca = prcomp(log.data,
                     center = TRUE,
                     scale. = TRUE)
   g = data.frame(
       PC1 = data.pca$x[, 1],
       PC2 = data.pca$x[, 2],
       Treatment = lpd$species$sample_table$flipgroup,
       wid = lpd$species$sample_table$wid
   ) %>% 
       ggplot(aes(x = PC1, y = PC2)) +
       geom_point(aes(color = Treatment))
   g
   
   
   
}

shinyApp(ui = ui, server = server)

