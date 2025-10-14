# JS intro steps on this page: 15 -> 19

############################### MAIN PCA module    ###############################
modulePCAUI <- function(id, label = "PCA module") {
  ns <- NS(id)

  tagList(
    column(
      width = 3,
      # Data Selection box ----
      box(
        width = NULL,
        collapsed = FALSE,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "warning",
        title = div(icon("cog"), div(style = "display: inline-block; padding-left: 5px", "Data selection")),
        actionButton(
          inputId = "starthelp",
          label = "Show help",
          `data-intro` = "The main purpose of the Principal Component Analysis is visualizing the main source of variation in the data and to identify outliers. If the experiment is well controlled and all worked well, the greatest source of variation should be treatments/groups",
          `data-step` = "10",
          onclick = "introJs().goToStepNumber(15).start();"
        ),
        uiOutput(ns("ftype")),
        # uiOutput(ns("savedDataset")),
        uiOutput(ns("selectDataset")),
        materialSwitch(ns("scaleunit"), label = "Scale to unit variance", value = TRUE, status = "warning", right = TRUE),
        introBox(
          uiOutput(ns("exclude_outliers")),
          data.step = 12,
          data.intro = "If you identify any outliers, you can exclude them here, then rerun the analysis"
        ),
        introBox(
          uiOutput(ns("exclude_groups")),
          data.step = 13,
          data.intro = "If you have multiple groups you can choose to inspect only a subset of them. At any change, remember to rerun the analysis"
        ),
        uiOutput(ns("shape_by")),
        uiOutput(ns("correctbatch_by")),
        materialSwitch(ns("displayLabel"), label = "Display sample names on the plots", value = FALSE, status = "warning", right = TRUE),
        awesomeRadio(ns("plot_extension"), label = "Download plots as:", choices = list("pdf", "png"), status = "warning", inline = TRUE),
        introBox(
          div(actionButton(ns("pca_data_run"), label = "Run PCA", icon = icon("caret-square-o-right")), style = "display: inline-block"),
          data.step = 11,
          data.intro = "Start by launching the Principal Component Analysis"
        ),
        introBox(
          div(actionButton(ns("rmd_plot"), "Save to report", icon = icon("floppy-o")), style = "display: inline-block"),
          data.step = 14,
          data.intro = "Click save to include the PCA into your final report <div style='color: #FF0000'> <br> END OF INTRO ON THIS PAGE. CLICK ANYWHERE TO END IT </div>"
        )
      ),
      box(
        width = NULL,
        collapsed = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "warning",
        title = div(icon("eye"), div(style = "display: inline-block; padding-left: 5px", "Selected data recap")),
        div(style = "color: #9A9FA1", strong("FEATURES:"), textOutput(ns("pca_features_size"), inline = TRUE), "feature(s)"),
        hr(style = "margin-top: 5px; margin-bottom: 5px"),
        "data view:", strong(textOutput(ns("ftype_recap"), inline = TRUE)), br(),
        "scale counts:", strong(textOutput(ns("scaleunit_recap"), inline = TRUE)), br(),
        "use dataset:", strong(textOutput(ns("pca_features_dataset_recap"), inline = TRUE)), br(),
        "rendered plot format:", strong(textOutput(ns("plot_extension_recap"), inline = TRUE)), br(),
        "excluded samples:", strong(textOutput(ns("exclude_outliers_recap"), inline = TRUE)), br(),
        "excluded groups:", strong(textOutput(ns("exclude_groups_recap"), inline = TRUE)), br()
      )
    ),
    column(
      width = 4,
      # PC1 vs PC2 ----
      boxPlus(
        width = NULL,
        collapsed = FALSE,
        collapsible = TRUE,
        solidHeader = TRUE,
        closable = FALSE,
        status = "primary",
        title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "PC#1", em("vs."), "PC#2")),
        plotDownloadUI(ns("pca_plot_1_export")) %>%
          helper(type = "markdown", content = "pca", colour = "#269dc0", size = "l"),
        plotPCAUI(ns("pca_plot_1")) # ,
      ),
      # PC1 vs PC3 ----
      boxPlus(
        width = NULL,
        collapsed = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        closable = FALSE,
        status = "primary",
        title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "PC#1", em("vs."), "PC#3")),
        plotDownloadUI(ns("pca_plot_3_export")),
        plotPCAUI(ns("pca_plot_3"))
      )
    ),
    column(
      width = 4,
      # PC2 vs PC3 ----
      boxPlus(
        width = NULL,
        collapsed = FALSE,
        collapsible = TRUE,
        solidHeader = TRUE,
        closable = FALSE,
        status = "primary",
        title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "PC#2", em("vs."), "PC#3")),
        plotDownloadUI(ns("pca_plot_2_export")),
        plotPCAUI(ns("pca_plot_2"))
      ),
      # 3D plot ----
      box(
        width = NULL,
        collapsed = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "primary",
        title = div(icon("cube"), div(style = "display: inline-block; padding-left: 5px", "3D-view")),
        plotlyOutput(ns("render3d"))
      )
    ),
    column(
      width = 8,
      offset = 3,
      # PCA Table ----
      boxPlus(
        width = NULL,
        collapsed = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        closable = FALSE,
        status = "primary",
        title = div(icon("file-text-o"), div(style = "display: inline-block; padding-left: 5px", "PCA Table")),
        downloadButton(ns("contribfile"), "Download"),
        DT::dataTableOutput(ns("contrib"))
      )
    ),
    column(
      width = 8,
      offset = 3,
      # Heatmap sample/sample
      boxPlus(
        width = NULL,
        collapsed = FALSE,
        collapsible = TRUE,
        solidHeader = TRUE,
        closable = FALSE,
        status = "primary",
        title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "Sample Distance Heatmap")),
        plotDownloadUI(ns("downloadsampleheatmap")),
        plotOutput(outputId = ns("render_sample_heatmap"), width = "auto", height = "500px")
      )
    )
  )
}


modulePCA <- function(input, output, session, gobject, dataset, de, newsp) {
  ns <- session$ns

  pca_data_save <- reactiveValues()

  pca_excludeSamples <- reactiveValues(ID = NULL) # will contain selected IDs

  # gene or isoform  ----
  fType <- reactive({
    selectInput(ns("ftype"), label = "Select data view:", choices = names(dataset()$ExpressionNormCounts), selected = 1)
  })

  output$ftype <- renderUI({
    fType()
  })

  selected_fType <- reactive({
    validate(need(!is.null((input$ftype)), ""))
    return(input$ftype)
  })

  # define samples ----
  sptable <- reactive({
    sp <- newsp$sptable()$df
    use_group <- newsp$use_group()
    othercols <- names(sp)[-which(names(sp) %in% c("SampleID", use_group))]
    if (length(othercols) > 0) {
      sp <- sp[, c("SampleID", use_group, othercols)]
    } else {
      sp <- sp[, c("SampleID", use_group)]
    }
    names(sp)[2] <- "SampleGroup"
    sp <- sp[order(sp$SampleGroup), ]
    return(sp)
  })

  # exclude outliers by sample or by group ----
  eoutliers <- reactive({
    m <- data.frame(as.matrix(sptable()[, c("SampleID", "SampleGroup")]), stringsAsFactors = FALSE)
    SamplePlan <- sptable()
    SI_ordered <- as.character(SamplePlan$SampleID[order(match(SamplePlan$SampleGroup, sort(levels(SamplePlan$SampleGroup))))])
    SG_ordered <- SamplePlan$SampleGroup[order(match(SamplePlan$SampleGroup, sort(levels(SamplePlan$SampleGroup))))]
    names(SI_ordered) <- paste(SI_ordered, SG_ordered, sep = " : ")
    pickerInput(
      inputId = ns("exclude_outliers"),
      label = "Select samples to exclude",
      choices = SI_ordered,
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE
    )
  })

  output$exclude_outliers <- renderUI({
    eoutliers()
  })

  egroups <- reactive({
    m <- data.frame(as.matrix(sptable()[, c("SampleID", "SampleGroup")]), stringsAsFactors = FALSE)
    SamplePlan <- sptable()
    SG_levels <- sort(levels(as.factor(SamplePlan$SampleGroup)))
    pickerInput(
      inputId = ns("exclude_groups"),
      label = "Select groups to exclude",
      choices = as.character(SG_levels),
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE
    )
  })

  output$exclude_groups <- renderUI({
    egroups()
  })

  shape_by <- reactive({
     sampleplan <- newsp$sptable()$df
     othercols <- names(sampleplan)[-which(names(sampleplan) %in% c("SampleID", "SampleGroup", "GroupLabels"))]
     if (length(othercols) > 0) {
       selectInput(ns("shape_by"), "Add shapes by:", choices = c("none", othercols))
     } else {
       NULL
     }
  })
 
   output$shape_by <- renderUI({
     shape_by()
   })
 
  correctbatch_by <- reactive({
    sampleplan <- newsp$sptable()$df
    othercols <- names(sampleplan)[-which(names(sampleplan) %in% c("SampleID", "SampleGroup", "GroupLabels"))]
    if (length(othercols) > 0) {
      selectInput(ns("correctbatch_by"), "Correct batch effect by:", choices = c("none", othercols))
    } else {
      NULL
    }
  })
 
  output$correctbatch_by <- renderUI({
    correctbatch_by()
  })

  # fList: gene-list or isoform-list ----
  pca_features_list <- reactive({
    validate(need(!is.null((selected_fType())), ""))
    fList <- rownames((m <- dataset()$ExpressionNormCounts[[selected_fType()]])$vst)
    fList <- fList[rowSums(m$cpm$raw >= 1) >= min(table(sptable()$SampleGroup))]
    return(fList)
  })

  # display label names
  labelNames <- reactive({
    input$displayLabel
  })

  # PCA data ----
  # data is created when user clicks the <RUN> button
  # used in pca_plot(), pca_coord(), callModule(plotlyPCA), output$contrib, output$contribfile
  # i: PCA calculation is always based on normalized data: __vst__
  pca_data_out <- eventReactive(input$pca_data_run, {
    fType <- selected_fType() # genes, isoforms
    fList <- pca_features_list() # list of genes or isoforms
    sList <- rep(TRUE, nrow(sptable())) # n=samplesize
    sIDs <- as.character(sptable()$SampleID)
    excl <- c(input$exclude_outliers, as.character(sptable()$SampleID[which(as.character(sptable()$SampleGroup) %in% input$exclude_groups)]))
    if (!is.null(excl)) {
      sList[which(sIDs %in% excl)] <- FALSE
      names(sList) <- sIDs
    }
#    m <- data.frame(SampleID = as.character(sptable()$SampleID), SampleGroup = as.character(sptable()$SampleGroup), stringsAsFactors = FALSE)[sList, ] # keep only IDs that are TRUE in sList
    m <- sptable()[sList, ]
    m$SampleGroup <- factor(m$SampleGroup, levels = names(gobject()$default.colors))
    m$SampleID <- as.character(m$SampleID)
    correctbatch_by <- input$correctbatch_by
    exists_correctbatch_by <- ifelse(is.null(correctbatch_by),FALSE,ifelse(correctbatch_by=="none",FALSE,TRUE))
    if (!exists_correctbatch_by) {
      x <- PCA(
        t(dataset()$ExpressionNormCounts[[fType]]$vst[fList, m$SampleID]),
        scale.unit = input$scaleunit,
        graph = FALSE
      )
    } else {
      batch_var <- m[, correctbatch_by]
      count_correct <- removeBatchEffect(dataset()$ExpressionNormCounts[[fType]]$vst[fList, m$SampleID], batch = batch_var)
      x <- PCA(
        t(count_correct), 
        scale.unit = input$scaleunit,
        graph = FALSE
      )
    }
    e <- format(x$eig[1:3, 2], digits = 2, nsmall = 2, trim = TRUE)
    contrib <- x$var$cor[, 1:3]
    x <- data.frame(x$ind$coord[as.character(m$SampleID), 1:3], subset(m, select = -SampleID))
    #x <- data.frame(x$ind$coord[as.character(m$SampleID), 1:3], m)
    print(head(x))
    names(x)[1:3] <- colnames(contrib) <- paste0("PC", 1:3)
    hm_dist <- dist(t(dataset()$ExpressionNormCounts[[fType]]$vst[fList, m$SampleID]))
    annot <- data.frame(group = m$SampleGroup)
    rownames(annot) <- m$SampleID
    list(
      variance = e,
      coord = x,
      contrib = contrib,
      hm_dist = hm_dist,
      annot = annot,
      correctbatch_by = correctbatch_by
    )
  })

  # PCA plot 2D----
  # (based on pca_data_out)
  pca_plot <- reactive({
    validate(need(!is.null((pca <- pca_data_out())), ""))
    M <- pca$coord
    if (is.null((ID <- pca_excludeSamples$ID))) {
      ID <- rep(FALSE, nrow(M))
    } else {
      ID <- ID[rownames(M)]
    }
    sampleIDs <- rownames(M)
    shape_by <- input$shape_by
    exists_shape_by <- ifelse(is.null(input$shape_by),FALSE,ifelse(input$shape_by=="none",FALSE,TRUE))
    correctbatch_by <- pca$correctbatch_by
    exists_correctbatch_by <- ifelse(is.null(correctbatch_by),FALSE,ifelse(correctbatch_by=="none",FALSE,TRUE))
    if (exists_shape_by) {
      M[, shape_by] <- as.character(M[, shape_by])
    }
    o <- lapply(
      1:length((L <- list(c(1, 2), c(2, 3), c(1, 3)))),
      function(i) {
        m <- M[, -setdiff(1:3, L[[i]])]
        m$snames <- rownames(m)
        names(m)[1:2] <- c("x", "y")
        include <- m[!ID, , drop = FALSE]
        exclude <- m[ID, , drop = FALSE]
        snames <- rownames(include)
        nshapeby <- 0
        if (!exists_shape_by) {
          p <- ggplot(data = include, aes(x = x, y = y, colour = SampleGroup, label = snames))
        } else {
          nshapeby <- length(levels(as.factor(include[, shape_by])))
          p <- ggplot(data = include, aes(x = x, y = y, colour = SampleGroup, label = snames, shape = get(shape_by    ))) +
            labs(shape = shape_by, col = "SampleGroup")
        }
        p <- p + theme_bw() # background color: white
        if (labelNames() == TRUE) {
          p <- p + geom_point(size = 2, alpha = .5) +
            geom_point(data = exclude, size = 2, color = "#000000", alpha = 0.2) +
            ggrepel::geom_text_repel()
        } else {
          p <- p + geom_point(size = 7, alpha = .5) +
            geom_point(data = exclude, size = 7, color = "#000000", alpha = 0.2)
            if (nshapeby < 4) {
              p <- p + geom_point_interactive(aes(tooltip = snames), size = 2)
            }
        }
        # it is necessary to <isolate> the following part, to avoid error msg when changing SamplePlan
        ftext <- ifelse(selected_fType() == "genes", "Gene", "Isoform")
        isolate({
          p <- p +
            theme(plot.title = element_text(size = rel(1.2), lineheight = 2, vjust = 1, face = "bold"), panel.border = element_rect(colour = "grey50", fill = NA), axis.text = element_text(size = rel(.7))) +
            scale_color_manual(values = gobject()$default.colors, drop = T) +
            scale_shape_discrete(drop = FALSE) +
            xlab(paste(paste0("PC#", L[[i]][1]), paste0("(", pca$variance[L[[i]][1]], "%)"))) +
            ylab(paste(paste0("PC#", L[[i]][2]), paste0("(", pca$variance[L[[i]][2]], "%)")))
          if (!exists_correctbatch_by) {
            p <- p + ggtitle(paste(paste0(ftext, "-based"), "PCA (vst data)"))
          } else {
            p <- p + ggtitle(paste(paste0(ftext, "-based"), "PCA (vst data, batch correction by", correctbatch_by,     ")"))
          }
        })
      }
    )
    return(o)
  })

  # PCA coordinates using pca_data_out ----
  pca_coord <- reactive({
    validate(need(!is.null((pca <- pca_data_out())), "")) # renamed function name from m to pca_coord
    m <- pca$coord
    return(m)
  })

  plot3d <- reactive({
    x <- pca_data_out()$coord
    if (length((ID <- pca_excludeSamples$ID))) {
      if (length(ID[ID])) {
        x$SampleGroup[match(names(ID)[ID], rownames(x))] <- "brushed"
      }
    }
    scene <- list(camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25)))
    colors <- gobject()$default.colors[levels((x$SampleGroup <- factor(as.character(x$SampleGroup))))]
    p <- plotly::plot_ly(x,
      x = ~PC1, y = ~PC2, z = ~PC3,
      type = "scatter3d",
      mode = "markers",
      opacity = 1, # points not showing in safari, workaround:
      # https://github.com/plotly/plotly.js/issues/5158#issuecomment-696557773
      marker = list(color = colors[x$SampleGroup]),
#      text = ~SampleID,
      hoverinfo = "text"
    )
    return(p)
  })

  output$render3d <- renderPlotly({
    plot3d()
  })

  plottype <- reactive({
    input$plot_extension
  })

  plot1 <- callModule(plotPCA, "pca_plot_1", pca_coord, reactive({
    pca_plot()[[1]]
  }), "P1", 3)

  plot2 <- callModule(plotPCA, "pca_plot_2", pca_coord, reactive({
    pca_plot()[[2]]
  }), "P2", 1)

  plot3 <- callModule(plotPCA, "pca_plot_3", pca_coord, reactive({
    pca_plot()[[3]]
  }), "P3", 2)

  callModule(plotDownload, "pca_plot_1_export", plot1, "PCA_pc1vs2", plottype)

  callModule(plotDownload, "pca_plot_2_export", plot2, "PCA_pc2vs3", plottype)

  callModule(plotDownload, "pca_plot_3_export", plot3, "PCA_pc2vs2", plottype)

  callModule(plotDownload, "downloadsampleheatmap", heatmapplot, "heatmap_samples", plottype)

  # PCA table ----
  output$contrib <- DT::renderDataTable(
    {
      pca_data_out()$contrib
    },
    options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#00404D', 'color': '#fff'});",
        "}"
      )
    )
  )

  output$contribfile <- downloadHandler(
    filename = function() {
      "PCA_contribution.csv"
    },
    content = function(file) {
      df <- data.frame(gene = rownames(pca_data_out()$contrib), pca_data_out()$contrib)
      write.table(df, file = file, sep = ";", quote = FALSE, row.names = FALSE)
    }
  )

  # Heatmap ----
  heatmapplot <- reactive({
    mycolors <- gobject()$default.colors # [1:length(unique(annot$group))]
    mycolors <- list(group = mycolors)
    validate(need(!is.null((pca_data_out())), ""))
    annot <- pca_data_out()$annot
    pheatmap::pheatmap(
      mat = as.matrix(pca_data_out()$hm_dist),
      color = colorRampPalette(c("#E9EBEC", "#168EE9", "#004D89"))(100), # blue to red
      clustering_distance_rows = pca_data_out()$hm_dist,
      clustering_distance_cols = pca_data_out()$hm_dist,
      annotation_col = annot,
      annotation_row = annot,
      annotation_colors = mycolors
    )
  })

  output$render_sample_heatmap <- renderPlot({
    heatmapplot()
  })

  # render various recap info ----
  output$pca_features_size <- renderText({
    validate(need(!is.null(pca_features_list()), ""))
    length(pca_features_list())
  })

  output$ftype_recap <- renderText({
    ifelse(selected_fType() == "genes", "gene-scaled", "isoform-scaled")
  })
  
  output$scaleunit_recap <- renderText({
    c("yes", "no")[as.numeric(input$scaleunit)]
  })
  
  output$plot_extension_recap <- renderText({
    input$plot_extension
  })
  
  output$pca_features_dataset_recap <- renderText({
    if (!is.null(input$selectedDataset)) {
      dt <- input$selectedDataset
    } else {
      dt <- "all features with cpm > 1"
    }
  })
  
  output$exclude_outliers_recap <- renderText({
    if (!is.null(input$exclude_outliers)) {
      eo <- input$exclude_outliers
    } else {
      eo <- "none"
    }
  })
  
  output$exclude_groups_recap <- renderText({
    if (!is.null(input$exclude_groups)) {
      eo <- input$exclude_groups
    } else {
      eo <- "none"
    }
  })

  ### save plots for report ----
  report_save <- reactiveValues()
  observeEvent(input$rmd_plot, {
    report_save$plot$status <- ifelse(input$rmd_plot != 0, 1, 0)
    report_save$plot$header <- c("Oups...", "Done!")[report_save$plot$status + 1]
    report_save$plot$message <- c("First run the PCA analysis!", paste("Successfully saved as:<b>", (Id <- "PCA_plots")))[report_save$plot$status + 1]
    if (report_save$plot$status != 0) {
      report_save$plot$report$plot1[[Id]] <- pca_plot()[[1]]
      report_save$plot$report$plot2[[Id]] <- pca_plot()[[2]]
      report_save$plot$report$plot3[[Id]] <- pca_plot()[[3]]
      report_save$plot$report$heatmap[[Id]] <- heatmapplot()
      report_save$summary$ftype <- input$ftype
      report_save$summary$scaleunit <- input$scaleunit
      report_save$summary$pca_features_filterout <- input$pca_features_filterout
      report_save$summary$exclude_outliers <- input$exclude_outliers
      report_save$summary$exclude_groups <- input$exclude_groups
    }
  })

  observeEvent(input$rmd_plot, {
    session$sendCustomMessage(type = "message", message = list(status = report_save$plot$status, header = report_save$plot$header, message = report_save$plot$message))
  })

  rlist <- list(report_save = report_save)
  
  return(rlist)
}


############################### PCA  2D  module  ###############################


plotPCAUI <- function(id, label = "Render Plot") {
  ns <- NS(id)

  tagList(
    ggiraphOutput(ns("render"))
  )
}

plotPCA <- function(input, output, session, pca_coord, data, P, nb) {
  output$render <- renderggiraph({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering data.", detail = "Please wait...", value = 1)
    ggiraph(code = print(data()))
  })

  return(data)
}

############################### PCA  3D module   ###############################

plotlyPCAUI <- function(id, label = "Render Plot") {
  ns <- NS(id)
  plotlyOutput(ns("render"))
}

plotlyPCA <- function(input, output, session, pcadata, pca_excludeSamples, gobject) {
  plot3d <- reactive({
    x <- pcadata()
    if (length((ID <- pca_excludeSamples$ID))) {
      if (length(ID[ID])) {
        x$SampleGroup[match(names(ID)[ID], rownames(x))] <- "brushed"
      }
    }
    scene <- list(camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25)))
    colors <- gobject()$default.colors[levels((x$SampleGroup <- factor(as.character(x$SampleGroup))))]
    p <- plotly::plot_ly(x,
      x = ~PC1, y = ~PC2, z = ~PC3,
      hoverinfo = "none",
      type = "scatter3d",
      mode = "markers",
      opacity = .5,
      marker = list(color = colors[x$SampleGroup])
    )
    p <- p %>% config(displayModeBar = FALSE, showLink = FALSE) # %>% layout(title = "3D Scatter plot", scene = scene)

    return(p)
  })

  output$render <- renderPlotly({
    #  progress = shiny::Progress$new()
    #  on.exit(progress$close())
    #  progress$set(message='Rendering data.', detail='Please wait...', value=1)
    plot3d()
  })
}
