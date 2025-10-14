function(request) {
  header <- dashboardHeaderPlus(
    title = div(img(src = "logo-icm-white.png", height = "30px"), "QUBY - RSQ"),
    titleWidth = 250,
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "info"
  )

  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "sbmenu",
      menuItem("Data Selection", tabName = "dataselect", icon = icon("layer-group")),
      menuItem("Expression data",
        icon = icon("braille"),
        menuSubItem("Global overview", tabName = "expression_overview"),
        menuSubItem("Feature-specific", tabName = "expression_features")
      ),
      menuItem("Sample stratification", tabName = "pca", icon = icon("sitemap")),
      menuItem("Differential analysis", tabName = "detags", icon = icon("chart-area")),
      menuItem("Set intersections", tabName = "genesets", icon = icon("bezier-curve")),
      menuItem("Enrichment analysis", tabName = "enrich", icon = icon("project-diagram")),
      menuItem("Report -BETA", tabName = "report", icon = icon("book")) # ,
    )
  )

  body <- dashboardBody(
    tagList(
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "customColors.css")),
      tags$head(tags$script(src = "custom.js")),
      tags$head(tags$script(src = "bootbox.js")) 
    ),
    tabItems(
      tabItem(
        tabName = "dataselect",
        fluidRow(
          HTML("<div class='HeaderSection'> Select/Upload your data</div>"),
          tagList(
            box(
              width = 3,
              solidHeader = TRUE,
              status = "warning",
              title = div(icon("cog"), div(style = "display: inline-block; padding-left: 5px", "Data selection")),
              fileInput("countfile", "Upload raw counts", multiple = F) %>%
                helper(type = "markdown", content = "upload", colour = "#269dc0", size = "l"),
              fileInput("spfile", "Upload sample plan", multiple = F),
              selectInput("project_type", "Project type", choices = c("mRNA", "smallRNA","other")),
              selectInput("organism", "Organism", choices = c("human", "mouse", "rat", "pig", "dog", "other")),
              actionButton("load", "Upload and Normalize"),
              actionButton("example", "Use example")
            ),
            moduleSampleplanUI("sampleplan")
          )
        )
      ),
      tabItem(tabName = "expression_overview", fluidRow(HTML("<div class='HeaderSection'>Expression data &raquo; Global overview</div>"), moduleEOUI("expression_overview"))),
      tabItem(tabName = "expression_features", fluidRow(HTML("<div class='HeaderSection'>Expression data &raquo; Feature-specific</div>"), moduleFSUI("expression_features"))),
      tabItem(tabName = "pca", fluidRow(HTML("<div class='HeaderSection'>Sample stratification &raquo; PCA</div>"), modulePCAUI("pca_plot"))),
      tabItem(tabName = "detags", fluidRow(HTML("<div class='HeaderSection'>Differential analysis</div>"), moduleDEUI("DE"))),
      tabItem(tabName = "genesets", fluidRow(HTML("<div class='HeaderSection'>Set intersections</div>"), moduleSETUI("gene_sets"))),
      tabItem(tabName = "enrich", fluidRow(HTML("<div class='HeaderSection'>Enrichment analysis </div>"), moduleENRICHMENTAnalysisUI("enrich"))),
      tabItem(tabName = "report", fluidRow(HTML("<div class='HeaderSection'>Report</div>"), moduleReportUI("Report")))
    )
  )

  dashboardPagePlus(
    title = "QUBY - RNAseq",
    header,
    sidebar,
    body,
    rightsidebar <- rightSidebar(
      tags$style( # https://stackoverflow.com/questions/62964056/r-shiny-scrolling-sidebar-overflow
        "#control-sidebar-1-tab {
            overflow: auto;
            max-height: 80vh;
        }"
      ),
      tags$style( # https://stackoverflow.com/questions/39603574/setting-a-grayscale-background-image-shiny-r
        "body.skin-blue.sidebar-mini.control-sidebar-open { 
             content: ''; 
             height: 100%; width: 100%; 
             position: fixed; 
             z-index: -1; 
             background: black; 
             background-size: cover; 
             filter: grayscale(100%); 
             -webkit-filter: grayscale(100%); }"
      ),
      background = "light",
      width = 1000,
      rightSidebarTabContent(
        id = 1,
        active = TRUE
      )
    )
  )
}
