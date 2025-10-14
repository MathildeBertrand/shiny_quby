# JS intro steps on this page: 8 -> 10

moduleEOUI <- function(id, label = "module expression overview") {
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
        `data-intro` = "Take an overview of the global expression levels across your groups by genes or by isoforms(transcripts), when the latter is available ",
        `data-step` = "8",
        onclick = "introJs().goToStepNumber(8).start();"
      ),
      uiOutput(ns("feature_type")), # flexible, depending on data: genes/isoforms
      introBox(
        uiOutput(ns("count_type")) %>%
          helper(type = "markdown", content = "tpm_cpm_fpkm", colour = "#269dc0", size = "l"), # flexible, depending on data: cpm, tpm, vst, raw etc
        data.step = 9,
        data.intro = "Select the gene expression unit to plot and to display in the table. The table with the expression values can be downloaded for further analysis"
      ),
      introBox(
        materialSwitch(ns("exclude_zero_expressions"), label = "Remove features with no expression", value = TRUE, status = "warning", right = TRUE),
        data.step = 10,
        data.intro = "Genes with no expression at all have been excluded by default. <div style='color: #FF0000'> <br> END OF INTRO ON THIS PAGE. CLICK ANYWHERE TO END IT. </div>"
      ),
      actionButton(ns("rmd_plot"), "Save plot to report", icon = icon("floppy-o"))
    ),
    moduleUI(ns("expression_overview_render"))
  )
}


moduleEO <- function(input, output, session, dataset, newsp, project) {
  ns <- session$ns

  # feature type
  fType <- reactive({
    selectInput(ns("feature_type"), label = "Select data view:", choices = names(dataset()$ExpressionNormCounts), selected = 1)
  })

  output$feature_type <- renderUI({
    fType()
  })

  # count type
  cType <- reactive({
    selectInput(ns("count_type"), label = "Expression unit:", choices = c(names(dataset()$ExpressionNormCounts[[1]]), "raw"), selected = 1)
  })

  output$count_type <- renderUI({
    cType()
  })

  datatype <- reactive({
    input$feature_type
  })

  counttype <- reactive({
    input$count_type
  })

  # this will be used if we filter out 0 expression genes (recommended)
  keep_genes <- reactive({
    data <- datatype()
    keep <- rowSums(dataset()$ExpressionNormCounts[[data]]$cpm$raw > 0) > 0
  })

  # select data table based on datatype and counttype, and filter no expressions
  data <- reactive({
    data <- datatype()
    validate(need(!is.null((cType <- counttype())), "Loading data"))
    if (cType == "cpm") {
      m <- dataset()$ExpressionNormCounts[[data]][["cpm"]][["raw"]]
    } else if (cType == "raw") {
      m <- dataset()$ExpressionCounts[[data]]
    } else {
      m <- dataset()$ExpressionNormCounts[[data]][[cType]]
    }
    if (input$exclude_zero_expressions) {
      m <- m[keep_genes(), ]
    }
    return(m)
  })

  # this is the gene expression table that can be downloaded by the user
  datadisplay <- reactive({
    dat <- as.data.frame(data())
    sptable <- newsp$sptable()$df
    dat$Gene <- row.names(dat)
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering data.", detail = "Please wait...", value = 1)
    if (project$project_type=="mRNA") {
      dat$FeatureID_simple <- sapply(as.character(dat$Gene), function(x) {
        strsplit(x, "\\_|\\:")[[1]][1]
      })
      if (datatype() == "isoforms") {
        dat$TXID <- sapply(as.character(dat$Gene), function(x) {
          strsplit(x, "\\_|\\:")[[1]][2]
        })
        dat$TXID <- sapply(as.character(dat$TXID), function(x) {
          strsplit(x, "\\.")[[1]][1]
        })
      }
      dat_annot_entrez <- AnnotationDbi::mapIds(
        x = eval(parse(text = project$slibrary)), keys = as.vector(dat$FeatureID_simple), column = c("ENTREZID"),
        keytype = project$keytype, multiVals = "first"
      )
      dat_annot_genename <- AnnotationDbi::mapIds(
        x = eval(parse(text = project$slibrary)), keys = as.vector(dat$FeatureID_simple), column = c("GENENAME"),
        keytype = project$keytype, multiVals = "first"
      )
      if ((project$organism %in% c("human", "mouse", "rat")) & (project$keytype == "SYMBOL")) {
        if (datatype() == "genes") {
          dat_annot_transcript_type <- AnnotationDbi::mapIds(
            x = eval(parse(text = project$enslibrary)), keys = as.vector(dat$FeatureID_simple), column = c("TXBIOTYPE"),
            keytype = "SYMBOL", multiVals = "first"
          )
        } else {
          dat_annot_transcript_type <- AnnotationDbi::mapIds(
            x = eval(parse(text = project$enslibrary)), keys = as.vector(dat$TXID), column = c("TXBIOTYPE"),
            keytype = "TXID", multiVals = "first"
          )
        }
        dat_annot_gene_type <- AnnotationDbi::mapIds(
          x = eval(parse(text = project$enslibrary)), keys = as.vector(dat$FeatureID_simple), column = c("GENEBIOTYPE"),
          keytype = "SYMBOL", multiVals = "first"
        )
        if (all(names(dat_annot_entrez) == names(dat_annot_genename))) {
          if (datatype() == "genes") {
            dat_annot <- cbind(dat_annot_entrez, dat_annot_genename, dat_annot_gene_type, dat_annot_transcript_type)
            datFinal <- merge(x = dat_annot, y = dat, by.x = "row.names", by.y = "FeatureID_simple")
          } else {
            dat_annot <- cbind(dat_annot_entrez, dat_annot_genename, dat_annot_gene_type)
            datFinal <- merge(x = dat_annot, y = dat, by.x = "row.names", by.y = "FeatureID_simple")
            dat_annot_transcript_type <- data.frame(dat_annot_transcript_type)
            datFinal <- merge(x = datFinal, y = dat_annot_transcript_type, by.x = "TXID", by.y = "row.names")
          }
          if (datatype() == "isoforms") {
            colrem <- "TXID"
            datFinal <- datFinal[, !names(datFinal) %in% colrem]
          }
          datFinal <- datFinal[, c("Gene", "dat_annot_entrez", "dat_annot_genename", "dat_annot_transcript_type", "dat_annot_gene_type", as.character(sptable$SampleID))]
          colnames(datFinal)[1:5] <- c("Gene", "EntrezID", "Gene Name", "TranscriptType", "GeneBiotype")
          df <- datFinal
        }
      } else {
        if (all(names(dat_annot_entrez) == names(dat_annot_genename))) {
          dat_annot <- cbind(dat_annot_entrez, dat_annot_genename)
          datFinal <- merge(x = dat_annot, y = dat, by.x = "row.names", by.y = "FeatureID_simple")
          if (datatype() == "isoforms") {
            colrem <- "TXID"
            datFinal <- datFinal[, !names(datFinal) %in% colrem]
          }
          datFinal <- datFinal[, c("Gene", "dat_annot_entrez", "dat_annot_genename", as.character(sptable$SampleID))]
          colnames(datFinal)[1:3] <- c("Gene", "EntrezID", "Gene Name")
          df <- datFinal
        }
      } 
    } else {
      df <- dat[,c("Gene", as.character(sptable$SampleID))]
    } 
    return(df)
  })

  plot <- reactive({
    mydata <- data()
    use_group <- newsp$use_group()
    sptable <- newsp$sptable()$df
    sptable <- sptable[order(sptable[, use_group]), ]
    x <- data.frame(CountData = c(mydata, recursive = T), SampleID = factor(rep(colnames(mydata), each = nrow(mydata)), levels = sptable$SampleID))
    x$SampleGroup <- sptable[, use_group][match(x$SampleID, sptable$SampleID)]
    ctype <- counttype()
    dtype <- datatype()
    p <- ggplot(data = x, aes(x = SampleID, y = CountData + .1)) +
      geom_boxplot(aes(outlier.colour = NULL, colour = SampleGroup, fill = SampleGroup), alpha = .5) +
      scale_y_continuous(trans = log10_trans()) +
      theme_bw() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = rel(1.2), lineheight = 1, vjust = 1, face = "bold"),
        panel.border = element_rect(colour = "grey50", fill = NA),
        axis.text = element_text(size = rel(.9)),
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      ggtitle(paste0(dtype, ": Global expression levels")) +
      ylab(paste0(ctype, " expression")) +
      scale_fill_manual(values = brewer.pal(12, "Paired")[1:length(levels(sptable[, use_group]))]) +
      scale_color_manual(values = brewer.pal(12, "Paired")[1:length(levels(sptable[, use_group]))])
    return(p)
  })

  ### save plots for report ----
  report_save <- reactiveValues()
  observeEvent(input$rmd_plot, {
    report_save$plot$status <- ifelse(input$rmd_plot != 0, 1, 0)
    report_save$plot$header <- c("Oups...", "Done!")[report_save$plot$status + 1]
    plot_view <- datatype()
    data_type <- counttype()
    report_save$plot$message <- c("There is nothing to save down here!", paste("Successfully saved as:<b>", (Id <- paste("Plot", plot_view, data_type, sep = "_"))))[report_save$plot$status + 1]
    if (report_save$plot$status != 0) {
      report_save$plot$report[[Id]] <- plot()
    }
  })

  observeEvent(input$rmd_plot, {
    session$sendCustomMessage(type = "message", message = list(status = report_save$plot$status, header = report_save$plot$header, message = report_save$plot$message))
  })

  callModule(module, "expression_overview_render", datadisplay, plot, "global_expression", datatype, counttype)

  rlist <- list(report_save = report_save)

  return(rlist)
}
