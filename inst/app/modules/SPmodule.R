# JS intro steps on this page: 1 -> 4

moduleSampleplanUI <- function(id, label = "module Sample Plan") {
  ns <- NS(id)
  tagList(
    box(
      width = 9,
      solidHeader = TRUE,
      status = "warning",
      title = div(icon("cog"), div(style = "display: inline-block; padding-left: 5px", "Data selection")),
      introjsUI(),
      # launch tour guide
      introBox(
        actionButton(ns("help"), "Take the tour"),
        data.step = 1,
        data.intro = "This intro will guide you through the main functionalities of the application. Use your arrow keys or the buttons to move to the next step, and press Esc or click anywhere to close the help screen. You will find a button like this on each page."
      ),
      br(),
      column(
        width = 6,
        uiOutput(ns("group_choices"))
      ),
      column(
        width = 12,
        introBox(
          downloadButton(ns("downloadSP"), "Download table"),
          data.step = 3,
          data.intro = "You can download the sample plan and modify the groups or add new variables. Remember to always keep column SampleID!"
        ),
        p(),
        withSpinner(dataTableOutput(ns("sptable")), color = "#2093bf")
      ),
      column(
        width = 3,
        tagList(
          introBox(
            radioButtons(ns("upload_sp"), "Upload a new Sample Plan", choices = c("no", "yes"), selected = "no"),
            data.step = 4,
            data.intro = "You can upload a different sample plan with other groups, but remember to always keep column SampleID. The new sample plan will only be active available during your current session <div style='color: #FF0000'> <br> END OF INTRO ON THIS PAGE. CLICK ANYWHERE TO END IT OR CONTINUE TO NEXT PAGE </div>"
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("upload_sp"), "'] == 'yes'"),
          fileInput(ns("spfile"), "Upload a new Sample Plan", multiple = F) %>%
            shinyInput_label_embed(icon("info") %>%
              bs_embed_popover(title = "Sample Plan must be in text format (.txt or .csv) with 2 columns named: \"SampleID\" and \"SampleGroup\". You can download current Sample Plan and modify the column SampleGroup. Do NOT change SampleIDs!")),
          actionButton(ns("usefile"), "Use Uploaded File"),
          actionButton(ns("resetSP"), "Reset Sample Plan")
        )
      )
    )
  )
}


moduleSampleplan <- function(input, output, session, dataset) {
  ns <- session$ns
  
  report_save <- reactiveValues()

  # initiate intro.js on click
  observeEvent(
    input$help,
    introjs(session,
      options = list(
        "nextLabel" = "Next",
        "prevLabel" = "Previous",
        "skipLabel" = "Skip intro"
      ),
      events = list(onbeforechange = readCallback("switchTabs"))
    )
  )

  ## user choses which sample plan to use (default or uploaded)
  chosefile <- reactiveValues(usedfile = "default")

  observeEvent(input$usefile, {
    chosefile$usedfile <- "uploaded"
  })

  observeEvent(input$resetSP, {
    chosefile$usedfile <- "default"
  })

  sptable_pre <- reactive({
    if (chosefile$usedfile == "default") {
      req(dataset()$SamplePlan)
      df <- dataset()$SamplePlan
    } else {
      df <- fread(input$spfile$datapath, data.table = F)
    }
    all_groups <- names(df)[-which(names(df) %in% c("SampleID"))]
    df <- df[, c("SampleID", all_groups)]
    for (i in 2:dim(df)[2]) {
      df[, i] <- as.factor(df[, i])
    }
    return(
      list(
        df = df,
        group_names = all_groups
      )
    )
  })

  group_choices <- reactive({
    introBox(
      selectInput(ns("use_group"), strong("Select the column containing the experimental conditions"), choices = sptable_pre()$group_names, selected = 1),
      data.step = 2,
      data.intro = "This is where you select the variable containing your groups"
    )
  })

  output$group_choices <- renderUI({
    group_choices()
  })

  use_group <- reactive({
    input$use_group
  })

  output$sptable <- DT::renderDataTable(
    datatable(sptable_pre()$df) %>% formatStyle(use_group(), backgroundColor = "#f4f4f4", color = "#00404D"),
    rownames = FALSE,
    # style='bootstrap',
    class = "table-bordered",
    options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#00404D', 'color': '#fff'});",
        "}"
      )
    )
  )

  output$downloadSP <- downloadHandler(
    filename = function() {
      "SamplePlan.txt"
    },
    content = function(file) {
      write.table(x = as.data.frame(sptable_pre()$df), file = file, sep = "\t", quote = FALSE, row.names = FALSE)
    }
  )

  rlist <- list(
    sptable = sptable_pre,
    use_group = use_group
  )

  return(rlist)
}
