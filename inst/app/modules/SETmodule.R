moduleSETUI <- function(id, label = "module gene-set") {
  ns <- NS(id)

  tagList(
    box(
      width = 3,
      solidHeader = TRUE,
      status = "warning",
      title = div(icon("cog"), div(style = "display: inline-block; padding-left: 5px", "Data selection")),
      HTML("<div style='color: #9A9FA1'><b>SELECT DATA TYPE:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
      uiOutput(ns("feature_type")),
      HTML("<div style='color: #9A9FA1'><b>DATASETS:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
      uiOutput(ns("sSelectDatasets")),
      uiOutput(ns("cPanel"))
    ),
    boxPlus(
      width = 9,
      solidHeader = TRUE,
      status = "primary",
      closable = FALSE,
      title = div(icon("eye"), div(style = "display: inline-block; padding-left: 5px", "Data viewer")),
      div(downloadButton(ns("download_plot"), "Download plot"), style = "display: inline-block; "),
      div(downloadButton(ns("download_table"), "Download table"), style = "display: inline-block; "),
      p(),
      uiOutput(ns("setPlotout"))
    )
  )
}


moduleSET <- function(input, output, session, dataset, de, gobject, newsp) {
  ns <- session$ns

  fType <- reactive({
    validate(need(length(de$de_data_save$data) > 0, "First you need to run the differential analysis and save the results"))
    selectInput(ns("feature_type"), label = NULL, choices = as.character(names(de$de_data_save$data)), selected = 1)
  })

  output$feature_type <- renderUI({
    fType()
  })

  # Select datasets ----
  sdatasets <- reactive({
    validate(need(length(de$de_data_save$data) > 0, "No dataset"))
    validate(need(!is.null((input$feature_type)), "No feature type"))
    validate(need(length(de$de_data_save$data[[input$feature_type]]) > 1, "You need at least 2 sets of results to intersect"))
    choices <- as.character(names(de$de_data_save$data[[input$feature_type]]))
    pickerInput(
      inputId = ns("sSelectDatasets"),
      label = NULL,
      choices = choices,
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE
    )
  })

  output$sSelectDatasets <- renderUI({
    sdatasets()
  })

  output$cPanel <- renderUI({
    if (length(input$sSelectDatasets) > 0) {
      column(
        width = 12,
        selectInput(ns("adjust_P"), label = "Adjust p-values:", choices = list("yes (FDR)" = 1, "no" = 2), selected = 1),
        sliderInput(inputId = ns("results_P_cutoff"), label = "Select FDR (p-value if no adjustment) threshold:", min = .001, max = .1, value = .05, step = .01),
        sliderInput(inputId = ns("results_FC_cutoff"), label = "Select log2 fold-change threshold:", min = 0, max = 10, value = 1, step = .5),
        selectInput(ns("regulated"), label = "Include", choices = list("All regulated features" = "all", "Upregulated only" = "up", "Downregulated only" = "down"), selected = 1),
        div(actionButton(ns("data_run"), label = "Run", icon = icon("caret-square-o-right")), style = "display: inline-block; margin-right: 5px"),
        div(actionButton(ns("rmd_plot"), "Save this plot to the report", icon = icon("floppy-o")), style = "display: inline-block; margin-right: 5px")
      )
    }
  })

  data_filter <- eventReactive(input$data_run, {
    validate(need(length(input$sSelectDatasets) > 0, "Please select at least two datasets"))
    P_select <- ifelse(input$adjust_P == "1", "FDR", "PValue") # select the apropriate pvalue (adjusted or not)
    P_cutoff <- input$results_P_cutoff
    F_cutoff <- input$results_FC_cutoff
    genes <- lapply(input$sSelectDatasets, function(dtset) {
      d <- de$de_data_save$data[[input$feature_type]][[dtset]]$res
      g <- list()
      g[["all"]] <- row.names(d)[which(d[, P_select] < P_cutoff & abs(d$logFC) > F_cutoff)]
      g[["up"]] <- row.names(d)[which(d[, P_select] < P_cutoff & d$logFC > F_cutoff)]
      g[["down"]] <- row.names(d)[which(d[, P_select] < P_cutoff & d$logFC < (-F_cutoff))]
      return(g)
    })
    names(genes) <- input$sSelectDatasets
    return(genes)
  })

  # Create plot ----
  setPlot <- eventReactive(input$data_run, {
    validate(need(length((dtsets <- input$sSelectDatasets)) > 1, "Please select at least two datasets"))
    filtered_data <- data_filter()
    genes <- lapply(dtsets, function(i) {
      g <- filtered_data[[i]][[input$regulated]]
      print(head(g))
      return(g)
    })
    names(genes) <- set_names()
    gInt <- fromList(genes)
    x1 <- unlist(genes, use.names = FALSE)
    x1 <- x1[!duplicated(x1)]
    row.names(gInt) <- as.character(x1)
    if (length(genes)<=3) {
      splot <- ggvenn(genes, stroke_size = 0.5, set_name_size = 4)
    } else {
      splot <- upset(gInt)
    }
    return(
      list(
        splot = splot,
        gInt = gInt
      )
    )
  })

  output$set_names <- renderUI({
    dtsets <- input$sSelectDatasets
    lapply(1:length(dtsets), function(i) {
      textInput(ns(paste0("set_name",i)), label = "", value = dtsets[i])
    })
  })    
  
  set_names <- reactive({
    dtsets <- input$sSelectDatasets
    new_names <- character()
    for (i in 1:length(dtsets)) {
      new_names <- c(new_names, session$input[[paste0("set_name", i)]])
    }
    return(new_names)
  })
  
  

  output$setPlotout <- renderUI({
    dtsets <- input$sSelectDatasets
    plot_width <- ifelse(length(dtsets) <= 3, "50%", "100%")
    output$setPlot <- renderPlot({
       setPlot()$splot
    })
    tagList(
      fluidRow(
        column(
          HTML("<div><b>Change set labels:</b></div>"),
          width = 3,
          uiOutput(ns("set_names"))
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "center",
          plotOutput(ns("setPlot"), width = plot_width)      
        )
      )
    )
  })

  # Download intersection ----
  output$download_table <- downloadHandler(
    filename <- function() {
      "intersection_table.xlsx"
    },
    content <- function(file) {
#      if (class(setPlot()$gInt) == "matrix") {
        write.xlsx(setPlot()$gInt, file = file, sheetName = "Intersections", append = T)
#      } else {
#        for (i in 1:length(setPlot()$gInt)) {
#          write.xlsx(setPlot()$gInt[i], file = file, sheetName = as.character(i), append = T)
#        }
#      }
    }
  )

  output$download_plot <- downloadHandler(
    filename <- function() {
      "intersection_plot.jpeg"
    },
    content <- function(file) {
      width <- 600
      height <- 500
      jpeg(file, width = width, height = height)
      print(setPlot()$splot) # without the grid.draw the plot won't show in pdf
      dev.off()
    }
  )

  ### save plots for report ----
  report_save <- reactiveValues()
  observeEvent(input$rmd_plot, {
    report_save$status <- ifelse(input$rmd_plot != 0, 1, 0)
    report_save$header <- c("Oups...", "Done!")[report_save$status + 1]
    report_save$message <- c(
      "First you need to generate the plot!",
      paste(
        "Successfully saved as:<b>",
        (Id <- paste0("intersection_", as.character(length(report_save$plot1) + 1)))
      )
    )[report_save$status + 1]
    validate(need(length((dtsets <- input$sSelectDatasets)) > 1, "Please select at least two datasets"))
    data_filter <- data_filter()
    genelists_all <- setdata(data_filter, dtsets, "all")
    genelists_up <- setdata(data_filter, dtsets, "up")
    genelists_down <- setdata(data_filter, dtsets, "down")
    if (report_save$status != 0) {
      report_save$plot1[[Id]] <- setplot(genelists_all, dtsets)
      report_save$plot2[[Id]] <- setplot(genelists_up, dtsets)
      report_save$plot3[[Id]] <- setplot(genelists_down, dtsets)
    }
  })

  observeEvent(input$rmd_plot, {
    session$sendCustomMessage(
      type = "message",
      message = list(
        status = report_save$status,
        header = report_save$header,
        message = report_save$message
      )
    )
  })

  rlist <- list(report_save = report_save)
  
  return(rlist)
}
