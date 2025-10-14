# JS intro steps on this page: 11 -> 14

moduleFSUI <- function(id, label = "module feature-specific") {
  ns <- NS(id)

  tagList(
    box(
      width = 3,
      solidHeader = TRUE,
      status = "warning",
      title = div(icon("cog"), div(style = "display: inline-block; padding-left: 5px", "Data selection")),
      actionButton(
        inputId = "starthelp",
        label = "Show help",
        `data-intro` = "Visualize the expressions of a specific gene either by sample or by groups",
        `data-step` = "11",
        onclick = "introJs().goToStepNumber(11).start();"
      ),
      introBox(
        uiOutput(ns("dataset_type")),
        uiOutput(ns("count_type")) %>%
          helper(type = "markdown", content = "tpm_cpm_fpkm", colour = "#269dc0", size = "l"),
        data.step = 12,
        data.intro = "Select genes or isoforms (when available) and the gene expression unit"
      ),
      selectInput(ns("plot_geom"), label = "See expressions:", choices = list("by samples" = 1, "by groups" = 2), selected = 1),
      introBox(
        selectizeInput(ns("gSelect"), label = "Search gene", choices = NULL, selected = NULL),
        data.step = 13,
        data.intro = "Type in the gene name"
      ),
      conditionalPanel(
        condition = paste0("input['", ns("dataset_type"), "'] == 'isoforms'"),
        uiOutput(ns("iSelect"))
      ),
      introBox(
        actionButton(ns("rmd_plot"), "Save this plot to the report", icon = icon("floppy-o")),
        data.step = 14,
        data.intro = "Click save for each gene you want to include into the report <div style='color: #FF0000'> <br> END OF INTRO ON THIS PAGE. CLICK ANYWHERE TO END IT. </div>"
      )
    ),
    box(
      width = 9,
      solidHeader = TRUE,
      status = "primary",
      title = div(icon("eye"), div(style = "display: inline-block; padding-left: 5px", "Data viewer")),
      tabBox(
        width = 12, selected = "Plot",
        tabPanel(
          "Plot",
          icon = icon("file-image-o"),
          div(awesomeRadio(ns("plot_extension"), label = "Download plot as:", choices = list("pdf", "png"), status = "warning", inline = TRUE), style = "display: inline-block;"),
          div(downloadButton(ns("downloadplot"), "Download plot"), style = "display: inline-block;"),
          plotOutput(outputId = ns("renderplot"), width = "auto", height = "500px")
        ),
        tabPanel(
          "Data",
          icon = icon("file-text-o"),
          div(awesomeRadio(ns("table_extension"), label = "Download data as:", choices = list("csv", "tsv"), status = "warning", inline = TRUE), style = "display: inline-block;"),
          div(downloadButton(ns("downloadcsv"), "Download table"), style = "display: inline-block;"),
          p(),
          uiOutput(ns("datatable_content")),
          p(),
          DT::dataTableOutput(outputId = ns("rendercsv"))
        )
      )
    )
  )
}


moduleFS <- function(input, output, session, dataset, gobject, newsp) {
  ns <- session$ns

  fType <- reactive({
    selectInput(ns("dataset_type"), label = "Select data view:", choices = names(dataset()$ExpressionNormCounts), selected = 1)
  })

  output$dataset_type <- renderUI({
    fType()
  })

  cType <- reactive({
    selectInput(ns("count_type"), label = "Expression unit:", choices = c("cpm", "tpm")[which(c("cpm", "tpm") %in% names(dataset()$ExpressionNormCounts[[1]]))], selected = 1)
  })

  output$count_type <- renderUI({
    cType()
  })

  datatype <- reactive({
    input$dataset_type
  })
  
  counttype <- reactive({
    input$count_type
  })

  observe({
    req(dataset()$ExpressionNormCounts$genes$cpm$raw)
    updateSelectizeInput(session, "gSelect", choices = rownames(dataset()$ExpressionNormCounts$genes$cpm$raw), server = TRUE, selected = NULL)
  })

  gQuantCPM <- reactive({
    apply(dataset()$ExpressionNormCounts$genes$cpm$raw, 2, function(x) quantile(x, probs = seq(0.01, 1, .01)))
  })

  gQuantTPM <- reactive({
    validate(need("tpm" %in% names(ExpressionNormCounts$genes), "No TPM value in your dataset"))
    apply(dataset()$ExpressionNormCounts$genes$tpm, 2, function(x) quantile(x, probs = seq(0.01, 1, .01)))
  })

  iQuantCPM <- reactive({
    apply(dataset()$ExpressionNormCounts$isoforms$cpm$raw, 2, function(x) quantile(x, probs = seq(0.01, 1, .01)))
  })

  iQuantTPM <- reactive({
    validate(need("tpm" %in% names(ExpressionNormCounts$isoforms), "No TPM value in your dataset"))
    apply(dataset()$ExpressionNormCounts$isoforms$tpm, 2, function(x) quantile(x, probs = seq(0.01, 1, .01)))
  })

  iName <- reactive({
    x <- NULL
    validate(need(!is.null((datatype())), "Loading data"))
    if (datatype() == "isoforms") {
      if (counttype() == "cpm") {
        x <- grep(paste0("^", input$gSelect, "_"), rownames(dataset()$ExpressionNormCounts$isoforms$cpm$raw), ignore.case = FALSE, perl = TRUE, value = TRUE)
        names(x) <- gsub("^[^_]+_", "", x)
      } else {
        x <- grep(paste0("^", input$gSelect, "_"), rownames(dataset()$ExpressionNormCounts$isoforms$tpm), ignore.case = FALSE, perl = TRUE, value = TRUE)
        names(x) <- gsub("^[^_]+_", "", x)
      }
    }
    return(x)
  })

  output$iSelect <- renderUI({
    validate(need(!is.null(iName()), ""))
    selectInput(ns("iSelect"), strong(paste("Select", paste0(input$gSelect, "-specific isoform:"))), choices = iName(), selectize = FALSE)
  })

  output$datatable_content <- renderUI({
    ctype <- counttype()
    choices_text <- c(paste("sample-based", ctype, "and quantile rank"), paste("group-based", ctype, "summary statistic"), paste("group-based quantile rank", ctype, "summary statistic"))
    selectInput(
      ns("datatable_content"),
      label = "Select type of data:",
      choices = choices_text,
      selected = 1
    )
  })

  sptable <- reactive({
    sp <- newsp$sptable()$df
    use_group <- newsp$use_group()
    sp <- sp[, c("SampleID", use_group)]
    names(sp)[2] <- "SampleGroup"
    return(sp)
  })

  loadfs <- reactive({
    validate(need(!is.null((fType <- datatype())), "Loading data"))
    fPref <- substring(datatype(), 1, 1)
    fName <- input[[paste0(fPref, "Select")]]
    fUnit <- counttype()
    if (fUnit == "cpm") {
      validate(need(!is.null((fName)), "Loading data"))
      i <- match(fName, rownames(dataset()$ExpressionNormCounts[[fType]]$cpm$raw))
      fData <- dataset()$ExpressionNormCounts[[fType]]$cpm$raw[i, ]
      Quant <- eval(parse(text = paste0(fPref, "QuantCPM()")))
      Quant <- colSums(t(t(Quant) <= fData))
      list(
        L = list(fData = fData, Quant = Quant),
        m = data.frame(
          value = c(fData, Quant),
          Item = rep(c("CPM estimates", "Quantile rank"), each = nrow(sptable())),
          SampleGroup = rep(sptable()$SampleGroup, 2),
          SampleID = factor(rep(sptable()$SampleID, 2), levels = sptable()$SampleID)
        ),
        fName = fName,
        fType = substring(fType, 1, nchar(fType) - 1),
        fUnit = fUnit
      )
    } else {
      validate(need(!is.null((fName)), "Loading data"))
      i <- match(fName, rownames(dataset()$ExpressionNormCounts[[fType]]$tpm))
      fData <- dataset()$ExpressionNormCounts[[fType]]$tpm[i, ]
      Quant <- eval(parse(text = paste0(fPref, "QuantTPM()")))
      Quant <- colSums(t(t(Quant) <= fData))
      list(
        L = list(fData = fData, Quant = Quant),
        m = data.frame(
          value = c(fData, Quant),
          Item = rep(c("TPM estimates", "Quantile rank"), each = nrow(sptable())),
          SampleGroup = rep(sptable()$SampleGroup, 2),
          SampleID = factor(rep(sptable()$SampleID, 2), levels = sptable()$SampleID)
        ),
        fName = fName,
        fType = substring(fType, 1, nchar(fType) - 1),
        fUnit = fUnit
      )
    }
  })

  plot <- reactive({
    i <- loadfs()
    validate(need(!is.null(i), ""))
    if (i$fUnit == "tpm") {
      j <- subset(i$m, Item == "TPM estimates")
    }
    if (i$fUnit == "cpm") {
      j <- subset(i$m, Item == "CPM estimates")
    }
    p <- ggplot(data = j, aes(y = value, fill = SampleGroup))
    if (input$plot_geom == 1) {
      p <- p +
        geom_bar(aes(x = SampleID), stat = "identity") +
        geom_text(
          aes(x = SampleID, y = value, colour = SampleGroup, label = paste0(round(value), "\n")),
          size = rel(4),
          lineheight = .3,
          hjust = .5,
          vjust = 0,
          show.legend = FALSE,
          fontface = "bold"
        )
    } else {
      p <- p +
        geom_boxplot(aes(x = SampleGroup, outlier.colour = NULL, colour = SampleGroup), alpha = .5) +
        theme(axis.text.x = element_blank())
    }
    p <- p +
      theme_bw() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = rel(1.5), lineheight = 1, vjust = 1, face = "bold"),
        panel.border = element_rect(colour = "grey50", fill = NA),
        strip.background = element_rect(colour = "grey50"),
        strip.text = element_text(size = rel(1)),
        axis.text = element_text(size = rel(.9)),
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_fill_manual(values = gobject()$default.colors) +
      scale_color_manual(values = gobject()$default.colors) +
      ggtitle(paste(i$fName, i$fType, i$fUnit, "expression levels")) #+ facet_grid(Item~., scales='free')
  })

  output$renderplot <- renderPlot({
    print(plot())
  })

  datacsv <- reactive({
    validate(need(!is.null((i <- loadfs())), "Loading data"))
    validate(need(!is.null((ctype <- counttype())), "Loading data"))
    validate(need(!is.null((select_dttable <- input$datatable_content)), "Loading data"))
    if (select_dttable == paste("sample-based", ctype, "and quantile rank")) {
      m <- data.frame(SampleID = sptable()$SampleID, "Estimates" = c(i$L$fData), "Quantile.rank" = c(i$L$Quant))
    } else if (select_dttable == paste("group-based", ctype, "summary statistic")) {
      m <- data.frame(Item = rownames((m <- sapply(with(sptable(), split(SampleID, SampleGroup)), function(x) summary(i$L[["fData"]][x])))), m)
    } else {
      m <- data.frame(Item = rownames((m <- sapply(with(sptable(), split(SampleID, SampleGroup)), function(x) summary(i$L[["Quant"]][x])))), m)
    }
    return(m)
  })

  geneName <- reactive({
    i <- loadfs()
    i$fName
  })
  
  type <- reactive({
    i <- loadfs()
    i$fType
  })

  plot_extension <- reactive({
    input$plot_extension
  })

  output$downloadplot <- downloadHandler(
    filename = function() {
      if (!is.null(type()) && !is.null(geneName())) {
        paste0(paste("expression", type(), geneName(), sep = "_"), ".", plot_extension())
      } else if (!is.null(type())) {
        paste0(paste("expression", type(), sep = "_"), ".", plot_extension())
      } else {
        paste0("expression", ".", plot_extension())
      }
    },
    content = function(file) {
      width <- 12
      height <- 8
      if (plot_extension() == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
        print(plot())
        dev.off()
      } else {
        pdf(file, width = width, height = height)
        print(plot())
        dev.off()
      }
    }
  )

  output$rendercsv <- DT::renderDataTable(
    {
      datacsv()
    },
    rownames = FALSE,
    style = "bootstrap",
    class = "table-bordered",
    options = list(
      pagingType = "full",
      searching = TRUE,
      lengthMenu = list(c(10, 20, -1), list("10", "20", "All")),
      scrollX = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#00404D', 'color': '#fff'});",
        "}"
      )
    )
  )

  table_extension <- reactive({
    input$table_extension
  })

  output$downloadcsv <- downloadHandler(
    filename = function() {
      if (!is.null(type()) && !is.null(geneName())) {
        paste0(paste("expression", type(), geneName(), sep = "_"), ".", table_extension())
      } else if (!is.null(type())) {
        paste0(paste("expression", type(), sep = "_"), ".", table_extension())
      } else {
        paste0("expression", ".", table_extension())
      }
    },
    content = function(file) {
      if (table_extension() == "csv") {
        write.table(datacsv(), file = file, sep = ";", quote = FALSE, row.names = FALSE)
      } else {
        write.table(datacsv(), file = file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    }
  )

  ### save plots for report
  report_save <- reactiveValues()
  
  observeEvent(input$rmd_plot, {
    report_save$status <- ifelse(input$rmd_plot != 0, 1, 0)
    report_save$header <- c("Oups...", "Done!")[report_save$status + 1]
    plot_geom <- ifelse(input$plot_geom == 1, "barplot", "boxplot")
    x <- loadfs()
    gene <- strsplit(x$fName, "_")[[1]][1]
    report_save$message <- c("There is nothing to save down here!", paste("Successfully saved as:<b>", (Id <- paste(x$fName, x$fType, plot_geom, sep = "_"))))[report_save$status + 1]
    if (report_save$status != 0) {
      report_save$plots[[gene]][[Id]] <- plot()
    }
  })

  observeEvent(input$rmd_plot, {
    session$sendCustomMessage(type = "message", message = list(status = report_save$status, header = report_save$header, message = report_save$message))
  })

  rlist <- list(report_save = report_save)
  
  return(rlist)
}
