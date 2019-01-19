sidebar = bs4DashSidebar(
    skin = "dark",
    class = "text-white",
    title = "Table of Content",
    brandColor = "white",
    bs4SidebarMenu(
        id = "tabName",
        bs4SidebarMenuItem("Boxplot", tabName = "lpd_boxplot", icon = "caret-square-right"),
        bs4SidebarMenuItem("PCA", tabName = "lpd_pca", tabOrder = 2, icon = "caret-square-right"),
        bs4SidebarMenuItem("vs Anthropometric", tabName = "lpd_atm", icon = "caret-square-right"),
        bs4SidebarMenuItem("Categorial Z scores", tabName = "lpd_zscore", icon = "caret-square-right"),
        bs4SidebarMenuItem("P value Histograms", tabName = "p_hist", icon = "caret-square-right"),
        bs4SidebarMenuItem("Correlation Volcano", tabName = "corr_vol", icon = "caret-square-right")
    )
)
