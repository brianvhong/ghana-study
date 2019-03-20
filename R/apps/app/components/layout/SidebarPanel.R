SidebarPanel = R6Class(
    "SidebarPanel",
    inherit = ShinyModule,
    public = list(
        # attributes
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            dashboardSidebar(
                sidebarMenu(
                    id = "tabs",
                    menuItem("Lipidome Boxplot", tabName = "lpd-boxplot"),
                    menuItem("PCA", tabName = "lpd-pca"),
                    menuItem("vs Anthropometric", tabName = "lpd-atm"),
                    menuItem("Categorial Z scores", tabName = "zscore"),
                    menuItem("P value Histograms", tabName = "hist"),
                    menuItem("Correlation Volcano", tabName = "corr-volc")
                )
            )
        },
        
        # server
        server = function(input, output, session){
            observeEvent(input$tabs, {
                return(input$tabs)
            })
        }
    )
)