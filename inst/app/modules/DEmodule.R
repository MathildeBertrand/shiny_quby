# JS intro steps on this page: 20 -> 30

moduleDEUI <- function(id, label = "DE module") {
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
        title = div(icon("cog"), div(style = "display: inline-block; padding-left: 5px", "Parameters")),
        tabBox(
          width = 12,
          tabPanel(
            "Data Selection",
            actionButton(
              inputId = "starthelp1",
              label = "Show Help on Parameters",
              `data-intro` = 'You are on the "Data selection" tab, here you define and filter the data you will use in the differential analysis.',
              `data-step` = "20",
              onclick = "introJs().goToStepNumber(20).start();"
            ),
            introBox(
              br(),
              HTML("<div style='color: #2B6587'><b>SELECT YOUR GROUPS:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
              column(
                width = 5,
                uiOutput(ns("sSelectgroupB"))
              ),
              column(
                width = 2,
                HTML("<b>VS.</b>")
              ),
              column(
                width = 5,
                uiOutput(ns("sSelectgroupA"))
              ),
              data.step = 21,
              data.intro = "Start by selecting the two groups that you want to compare."
            ),
            introBox(
              HTML("<div style='color: #2B6587'><b>REMOVE OUTLIERS:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
              uiOutput(ns("sSelectoutliers")),
              data.step = 22,
              data.intro = "Outliers identified on the PCA can and sometimes should be excluded from the analysis"
            ),
            HTML("<div style='color: #2B6587'><b>SELECT DATA TYPE:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
            uiOutput(ns("de_feature_type")),
            introBox(
              HTML("<div style='color: #2B6587'><b>PRE-FILTER EXPRESSION DATA:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
              HTML("<div style='color: #269dc0'><i>Choose the number of samples with cpm >= threshold. By default, 1 CPM in at least 1 sample</i></div>"),
              sliderInput(inputId = ns("de_features_select_CPM_cutoff"), label = "Select CPM threshold:", min = 1, max = 10, value = 1, step = 0.5),
              uiOutput(ns("min_samples_with_CPM")),
              data.step = 23,
              data.intro = "It is recommended to filter for lowly expressed genes before running the analysis. Genes with very low counts across all samples provide little evidence for differential expression. <div style='color: #FF0000'> <br> CLICK ANYWHERE TO EXIT, THEN GO TO THE SECOND TAB IN PARAMETERS CALLED 'Analysis' </div>"
            )
          ),
          tabPanel(
            "Analysis",
            actionButton(
              inputId = "starthelp2",
              label = "Show Help on Analysis",
              `data-intro` = "This is where you will actually run the analysis",
              `data-step` = "24",
              onclick = "introJs().goToStepNumber(24).start();"
            ),
            introBox(
              br(),
              HTML("<div style='color: #2B6587'><b>METHOD SELECTION:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
              selectInput(ns("de_method"), label = "Select differential analysis method:", choices = list("DESeq2" = 1, "edgeR" = 2), selected = 1),
              data.step = 25,
              data.intro = "DESeq2 and edgeR are two different methods for differential analysis. For robust results we recommend running both and keeping genes detected by both methods. On a practical note: edgeR runs faster then DESeq2."
            ),
            introBox(
              uiOutput(ns("select_covariates")),
              data.step = 26,
              data.intro = "Any additional variables (if provided) will appear here. You can include them in the model as covariates. "
            ),
            introBox(
              HTML("<div style='color: #2B6587'><b>FILTER RESULTS:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
              selectInput(ns("de_adjust_P"), label = "Adjust p-values:", choices = list("yes (FDR)" = 1, "no" = 2), selected = 1),
              sliderInput(inputId = ns("de_results_P_cutoff"), label = "Select FDR (p-value if no adjustment) threshold:", min = .001, max = .1, value = .05, step = .01),
              sliderInput(inputId = ns("de_results_FC_cutoff"), label = "Select log2 fold-change threshold:", min = 0, max = 10, value = 1, step = .5) %>%
                shinyInput_label_embed(icon("info") %>%
                  bs_embed_popover(title = "This is a log2 fold change. Log2(1) means that your feature is expressed twice as much in group2 compared to group1")),
              data.step = 27,
              data.intro = "Set thresholds for the differential analysis results"
            ),
            div(
              style = "position: relative; border-style: dotted none none none; border-width: 2px; border-color: rgba(243, 156, 18, .8); width: 100%; height: 45px",
              div(
                style = "position: absolute; right: 0; top: 10px",
                introBox(
                  div(actionButton(ns("de_data_run"), label = "Run analysis", icon = icon("caret-square-o-right")), style = "display: inline-block; margin-right: 5px"),
                  data.step = 28,
                  data.intro = "Click here to launch the analysis every time you make any changes in the parameters"
                ),
                introBox(
                  div(actionButton(ns("de_data_save"), label = "Save results", icon = icon("floppy-o")), style = "display: inline-block"),
                  data.step = 29,
                  data.intro = "Save the results to use them in the Enrichment analysis, Set intersections (Venn-diagrams), and to be included in the final report"
                )
              )
            ),
            tags$br(),
            column(
              width = 6,
              tags$br(),
              awesomeRadio(ns("de_export_plot_type"), label = "Download plots as:", choices = list("pdf", "png"), status = "warning", inline = TRUE)
            )
          )
        )
      ),
      # Selection data recap box ----
      box(
        width = NULL,
        collapsed = FALSE,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "warning",
        title = div(icon("eye"), div(style = "display: inline-block; padding-left: 5px", "Saved Datasets")),
        introBox(
          DT::dataTableOutput(ns("recap_saved")),
          data.step = 30,
          data.intro = "You will find here a list with the names of the results you have saved"
        )
      )
    ),
    column(
      width = 9,
      fluidRow(
        # Volcano plot ----
        boxPlus(
          width = 6,
          collapsed = FALSE,
          collapsible = TRUE,
          solidHeader = TRUE,
          closable = FALSE,
          status = "primary",
          title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "Volcano plot")),
          downloadButton(ns("de_features_volcano_plot_export"), label = "Download"),
          withSpinner(plotOutput(ns("de_features_volcano_plot"),
            brush = brushOpts(id = ns("de_features_volcano_plot_brush"), resetOnNew = TRUE)
          ), color = "#2093bf") %>%
            helper(type = "markdown", content = "volcanoplot", colour = "#269dc0", size = "l")
        ),
        # DE plot ----
        boxPlus(
          width = 6,
          collapsed = FALSE,
          collapsible = TRUE,
          solidHeader = TRUE,
          closable = FALSE,
          status = "primary",
          title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "Differentially expressed features - Summary")),
          downloadButton(ns("de_features_volcano_plot_summary_export"), label = "Download"),
          plotOutput(ns("de_features_volcano_plot_summary"))
        ),
        # Feature selection plot ----
        boxPlus(
          width = 6,
          collapsed = TRUE,
          collapsible = TRUE,
          solidHeader = TRUE,
          closable = FALSE,
          status = "primary",
          title = div(icon("hand-o-up"), div(style = "display: inline-block; padding-left: 5px", "Zoom on the volcano plot")),
          HTML("<div style='color: #269dc0'><i> Select a region on the above volcano plot. The details will appear below. <br> Click on the dot in front of the gene name and see the expression plot on the righ (Selected feature) </i></div>"),
          downloadButton(ns("de_features_volcano_plot_zoom_export"), label = "Download"),
          plotOutput(ns("de_features_volcano_plot_zoom"), click = ns("de_features_volcano_plot_click"))
        ),
        # Selected feature plot ----
        boxPlus(
          width = 6,
          collapsed = TRUE,
          collapsible = TRUE,
          solidHeader = TRUE,
          closable = FALSE,
          status = "primary",
          title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "Selected feature")),
          HTML("<div style='color: #269dc0'><i> Click on the dot in front of the gene in 'Feature selection' box. The expression plot will be displayed here </i></div>"),
          downloadButton(ns("de_features_volcano_plot_focus_export"), label = "Download"),
          plotOutput(ns("de_features_volcano_plot_focus"))
        ),
        # MAplot ----
        box(
          width = 12,
          collapsed = TRUE,
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "primary",
          title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "MA plot")),
          plotlyOutput(ns("de_features_ma_plot")) %>% helper(type = "markdown", content = "maplot", colour = "#269dc0", size = "l")
        ),
        # Diff analysis table ----
        boxPlus(
          width = 12,
          collapsed = TRUE,
          collapsible = TRUE,
          solidHeader = TRUE,
          closable = FALSE,
          status = "primary",
          title = div(icon("file-text-o"), div(style = "display: inline-block; padding-left: 5px", "Differential analysis summary")),
          div(
            style = "display: inline-block;vertical-align:top; width: 200px;",
            selectInput(ns("de_export_list"),
              label = "Select data table:",
              choices = list("Full data table" = 1, "Regulated features only" = 2),
              selected = 1
            )
          ),
          div(style = "display: inline-block;vertical-align:top; width: 200px;", uiOutput(ns("de_features_select_counts"))),
          downloadButton(ns("de_data_out_export"), label = "Download"),
          p(),
          DT::dataTableOutput(outputId = ns("de_data_out"))
        ),
        # Heatmap ----
        boxPlus(
          width = 12,
          collapsed = TRUE,
          collapsible = TRUE,
          solidHeader = TRUE,
          closable = FALSE,
          status = "primary",
          title = div(icon("hand-o-up"), div(style = "display: inline-block; padding-left: 5px", "Heatmap")),
          div(dropdownButton(
            circle = TRUE,
            status = "primary",
            icon = icon("gear"),
            color = "black",
            size = "sm",
            tooltip = tooltipOptions(title = "Click to change parameters !"),
            tags$h3("Heatmap Parameters"),
            selectInput(ns("gene_list"), label = "Display up-/down- regulated genes:", choices = c("both", "up", "down")),
            materialSwitch(ns("heatmap_select_showgenename"), label = "Display sample names on the plots", value = FALSE, status = "primary", right = TRUE),
            selectInput(ns("hclustFun"), "Clustering method", c("complete", "single", "average", "mcquitty", "median", "centroid", "ward.D", "ward.D2"), selected = "complete"),
            selectInput(ns("dendrogram"), "Apply Clustering:", c("none", "genes", "samples", "both"), selected = "both"),
            selectInput(ns("heatmap_select_rowdist"), label = "Feature-based distance metric:", choices = c("correlation", "euclidean", "manhattan", "maximum"), selected = "euclidean"),
            selectInput(ns("heatmap_select_coldist"), label = "Sample-based distance metric:", choices = c("correlation", "euclidean", "manhattan", "maximum"), selected = "euclidean")
          ), style = "display: inline-block"),
          div(downloadButton(ns("de_features_heatmap_plot_export"), label = "Download"), style = "display: inline-block"),
          uiOutput(ns("heatmap_out"))
        )
      )
    )
  )
}


moduleDE <- function(input, output, session, dataset, newsp, project) {
  ns <- session$ns

  # select genes or isoforms
  fType <- reactive({
    selectInput(ns("de_feature_type"), label = NULL, choices = names(dataset()$ExpressionCounts), selected = 1)
  })

  output$de_feature_type <- renderUI({
    fType()
  })

  # select count type to display in expression data table
  cType <- reactive({
    count_types <- names(dataset()$ExpressionNormCounts[[1]])
    count_types <- c(count_types, "raw")
    selectInput(ns("de_features_select_counts"), label = "Select datatype:", choices = count_types, selected = "cpm")
  })

  output$de_features_select_counts <- renderUI({
    cType()
  })

  # initialize variables (from input) ----
  de_data_save <- reactiveValues()
  
  de_features_volcano_plot_range <- reactiveValues(x = NULL, y = NULL)
  
  de_feature_type_recap <- reactive({
    input$de_feature_type
  })
  
  de_method <- reactive({
    c("DESeq2", "edgeR")[as.numeric(input$de_method)]
  })
  
  de_features_select_counts_recap <- reactive({
    input$de_features_select_counts
  })
  
  de_features_select_CPM_cutoff_recap <- reactive({
    as.numeric(input$de_features_select_CPM_cutoff)
  })
  
  de_min_samples_with_CPM <- reactive({
    input$min_samples_with_CPM
  })
  
  de_adjust_P_recap <- reactive({
    input$de_adjust_P
  })
  
  de_export_list <- reactive({
    as.numeric(input$de_export_list)
  })
  
  de_results_P_cutoff_recap <- reactive({
    as.numeric(input$de_results_P_cutoff)
  })
  
  de_results_FC_cutoff_recap <- reactive({
    as.numeric(input$de_results_FC_cutoff)
  })
  
  de_export_plot_type_recap <- reactive({
    input$de_export_plot_type
  })
  
  de_data_run <- reactive({
    input$de_data_run
  })

  use_group <- reactive({
    newsp$use_group()
  })

  sptable <- reactive({
    newsp$sptable()$df
  })

  # Select groups ----
  groupA <- reactive({
    div(
      id = "expr-containerA",
      selectInput(
        ns("sSelectgroupA"),
        label = NULL,
        choices = levels(as.factor(sptable()[, use_group()]))
      )
    )
  })

  output$sSelectgroupA <- renderUI({
    groupA()
  })

  groupB <- reactive({
    div(
      id = "expr-containerB",
      selectInput(
        ns("sSelectgroupB"),
        label = NULL,
        choices = levels(as.factor(sptable()[, use_group()]))
      )
    )
  })

  output$sSelectgroupB <- renderUI({
    groupB()
  })

  # Outliers ----
  soutliers <- reactive({
    use_group <- use_group()
    sp_subset <- sptable()[which(sptable()[, use_group] %in% c(input$sSelectgroupA, input$sSelectgroupB)), ]
    choices <- as.character(sp_subset$SampleID)
    names(choices) <- paste(sp_subset$SampleID, sp_subset[, use_group], sep = ":")
    pickerInput(
      inputId = ns("sSelectoutliers"),
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

  output$sSelectoutliers <- renderUI({
    soutliers()
  })

  # SampleInfo ----
  de_sampleplan <- reactive({
    validate(need(!is.null((sptable <- sptable())), "Loading samples"))
    x <- list(NULL, NULL)
    othercolumns <- names(sptable)[-which(names(sptable) %in% c("SampleID", use_group()))]
    if (length(othercolumns) > 0) {
      m <- data.frame(as.matrix(sptable()[, c("SampleID", use_group(), othercolumns)]), stringsAsFactors = FALSE)
    } else {
      m <- data.frame(as.matrix(sptable()[, c("SampleID", use_group())]), stringsAsFactors = FALSE)
    }
    m <- m[which(m[, use_group()] %in% c(input$sSelectgroupA, input$sSelectgroupB)), ]
    if (length(input$sSelectoutliers) > 0) {
      m <- m[-which(as.character(m$SampleID) %in% input$sSelectoutliers), ]
    }
    m$SampleID <- as.character(m$SampleID)
    names(m)[2] <- "GroupLabels"
    m$SampleGroup <- ifelse(m$GroupLabels == input$sSelectgroupB, "Group#2", "Group#1")
    return(m)
  })

  # Pre-filter expression data ----
  output$min_samples_with_CPM <- renderUI({
    sliderInput(
      inputId = ns("min_samples_with_CPM"),
      label = "Number of samples respecting CPM threshold:",
      min = 1,
      max = min(table(de_sampleplan()$SampleGroup)),
      value = 1,
      step = 1
    )
  })

  data_filterCPM <- eventReactive(de_data_run(), {
    validate(need(!is.null((sampleplan <- de_sampleplan())), "No Sampleplan"))
    validate(need(!is.null((fType <- input$de_feature_type)), "Please select a dataset"))
    # subset CPM table (keep only samples that will be included in the analysis)
    sampleids <- trimws(as.character(sampleplan$SampleID))
    dat_cpm <- dataset()$ExpressionNormCounts[[fType]]$cpm$raw
    dat_cpm <- dat_cpm[ ,sampleids]
    # define list of genes to keep that satisfy the CPM conditions
    keep <- rowSums(dat_cpm >= input$de_features_select_CPM_cutoff) >= input$min_samples_with_CPM
    # subset (by genes and samples) all the tables that will be displayed in the table section
    list(
      raw = dataset()$ExpressionCounts[[fType]][keep, sampleids],
      cpm = dataset()$ExpressionNormCounts[[fType]]$cpm$raw[keep, sampleids],
      tpm = dataset()$ExpressionNormCounts[[fType]]$tpm[keep, sampleids],
      vst = dataset()$ExpressionNormCounts[[fType]]$vst[keep, sampleids]
    )
  })

  # Differential analysis ----
  select_covariates <- reactive({
    validate(need(!is.null((sampleplan <- de_sampleplan())), ""))
    othercols <- names(sampleplan)[-which(names(sampleplan) %in% c("SampleID", "SampleGroup", "GroupLabels"))]
    if (length(othercols) > 0) {
      awesomeCheckboxGroup(ns("select_covariates"), "Add covariates", choices = othercols)
    } else {
      NULL
    }
  })

  output$select_covariates <- renderUI({
    select_covariates()
  })

  design_formula <- reactive({
    if (!is.null(input$select_covariates) & length(input$select_covariates) > 0) {
      f <- as.formula(paste0("~", paste(c(input$select_covariates, "SampleGroup"), collapse = "+")))
    } else {
      f <- as.formula("~SampleGroup")
    }
    return(f)
  })

  design_formula_pretty <- eventReactive(de_data_run(), {
    f <- design_formula()
    f_pretty <- paste(as.character(f), collapse = " ")
    f_pretty <- gsub("SampleGroup", use_group(), f_pretty)
    return(f_pretty)
  })

  de_analysis <- eventReactive(de_data_run(), {
    validate(need(input$sSelectgroupA != input$sSelectgroupB, "Please select two different groups"))
    validate(need(!is.null((dat <- data_filterCPM()$raw)), "No data"))
    validate(need(!is.null((sampleinfo <- de_sampleplan())), "No sample info"))
    validate(need(!is.null((f <- design_formula())), ""))
    sampleinfo <- sampleinfo %>% mutate(across(c("SampleGroup", input$select_covariates), factor))
    s <- sampleinfo %>% summarise(across(input$select_covariates, nlevels)) # for higher versions of dplyr
    validate(need(!(1 %in% s), "One or more covariates have a single element"))
    f_pretty <- design_formula_pretty()
#    ncoef <- 2 + length(input$select_covariates)
    if (de_method() == "edgeR") {
      mm <- model.matrix(f, sampleinfo)
      dge <- DGEList(counts = dat, group = mm[, 2] + 1)
      dge <- calcNormFactors(dge)
      dge <- estimateGLMTrendedDisp(dge, design = mm)
      dge <- estimateGLMTagwiseDisp(dge, design = mm)
      fit <- glmFit(dge, design = mm)
      lrt <- glmLRT(fit)
      res <- subset(topTags(lrt, n = nrow(dge), sort.by = "none")$table, select = c(logFC, PValue, FDR))
    } else if (de_method() == "DESeq2") {
      dds <- DESeqDataSetFromMatrix(countData = round(dat), colData = sampleinfo, design = f)
      dds <- DESeq(dds)
      # The genes with adjusted p-value of NA have less mean normalized counts than the optimal threshold.
      # https://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#why-are-some-p-values-set-to-na
      res <- results(dds, cooksCutoff = TRUE, independentFiltering = TRUE)[, c("log2FoldChange", "pvalue", "padj")]
      names(res) <- c("logFC", "PValue", "FDR")
    }
    # we need to freeze the different parameters, as these will be used for the plot titles and labels
    # by doing so we ensure that the labels will only refresh on click
    list(
      res = res,
      grA = input$sSelectgroupA,
      grB = input$sSelectgroupB,
      fType = input$de_feature_type,
      de_method = de_method(),
      design_formula = f,
      desing_formula_pretty = f_pretty
    )
  })

  # Mark results as regulated or not regulated
  de_mark_results <- reactive({
    validate(need(!is.null((res <- data.frame(de_analysis()$res))), "No results"))
    grA <- de_analysis()$grA
    grB <- de_analysis()$grB
    P_select <- ifelse(input$de_adjust_P == "1", "FDR", "PValue") # select the apropriate pvalue (adjusted or not)
    P_cutoff <- input$de_results_P_cutoff
    F_cutoff <- input$de_results_FC_cutoff
    Status <- abs(res$logFC) > F_cutoff & res[, P_select] < P_cutoff
    Status[Status > 0 & res$logFC > 0] <- 2
    res$Status <- factor(Status, levels = 0:2, labels = c("Not regulated", paste0("Down in ", grB), paste0("Up in ", grB)))
    res$FC <- ifelse(res$logFC > 0, 2^res$logFC, (-1 / (2^res$logFC)))
    res$log10P <- -log10(res[, P_select])
    res <- res[, c("logFC", "FC", "PValue", "FDR", "Status", "log10P")]
    return(res)
  })

  # Volcano plot ----
  de_features_volcano_plot <- reactive({
    validate(need(!is.null((dat <- de_mark_results())), "No data"))
    grA <- de_analysis()$grA
    grB <- de_analysis()$grB
    fType <- de_analysis()$fType
    de_method <- de_analysis()$de_method
    form <- design_formula_pretty()
    P_select <- ifelse(input$de_adjust_P == "1", "FDR", "PValue")
    dat <- dat[which(!is.na(dat$PValue) & !is.na(dat$FDR)), ] # keep lines with no NA
    gtitle <- paste0(grB, " vs. ", grA, " : ", substr(fType, 1, nchar(fType) - 1), "-scaled ")
    p <- ggplot(data = data.frame(FeatureID = rownames(dat), dat), aes(x = logFC, y = log10P, colour = Status))
    p +
      geom_point(size = 4, alpha = .5) +
      scale_color_manual(values = c("#808080", "#1E90FF", "#FF8C00"), name = paste("Regulated", fType), drop = FALSE) +
      theme_bw() +
      theme(plot.title = element_text(size = rel(1.2), lineheight = 2, vjust = 1, face = "bold"), panel.border = element_rect(colour = "grey50", fill = NA)) +
      ggtitle(gtitle) +
      xlab("log2FC") +
      ylab(paste("-log10", P_select)) +
      labs(subtitle = paste0(de_method, " : ", form))
  })

  # Render Volcano plot ----
  output$de_features_volcano_plot <- renderPlot({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering data.", detail = "Please wait...", value = 1)
    de_features_volcano_plot()
  })

  # Download Volcano plot ----
  output$de_features_volcano_plot_export <- downloadHandler(
    filename <- function() {
      fType <- de_analysis()$fType
      paste(paste0("de-P1-", fType), input$de_export_plot_type, sep = ".")
    },
    content <- function(file) {
      width <- 10
      height <- 8
      if (input$de_export_plot_type == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
      } else {
        pdf(file, width = width, height = height)
      }
      print(de_features_volcano_plot())
      dev.off()
    }
  )

  # Differentially expressed features: summary plot
  de_features_volcano_plot_summary <- reactive({
    validate(need(!is.null((dat <- de_mark_results())), ""))
    dat <- dat[which(!is.na(dat$PValue) & !is.na(dat$FDR)), ] # keep lines with no NA
    fType <- de_analysis()$fType
    x <- data.frame(Status = levels(dat$Status), value = as.numeric(table(dat$Status)))
    h <- x$value
    h[!h] <- 1
    ggplot(
      data = data.frame(x, height = h + .001)
    ) +
      geom_bar(aes(x = factor(Status, levels = levels(Status)), y = height, fill = Status), stat = "identity", width = .98, position = "dodge") +
      scale_fill_manual(values = c("#808080", "#1E90FF", "#FF8C00")[c(2, 1, 3)], name = paste("Regulated", fType)) +
      scale_color_manual(values = c("#808080", "#1E90FF", "#FF8C00")[c(2, 1, 3)], guide = "none") +
      geom_text(
        aes(x = factor(Status, levels = levels(Status)), y = height, colour = Status, label = paste0(value, "\n")),
        position = position_dodge(width = 0.9),
        size = rel(4),
        lineheight = .3,
        hjust = .5,
        vjust = 0,
        show.legend = FALSE
      ) +
      theme_bw() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = rel(1.2), lineheight = 1, vjust = 1, face = "bold"),
        panel.border = element_rect(colour = "grey50", fill = NA),
        axis.text = element_text(size = rel(.7)),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
      ) +
      ggtitle(paste("Differentially expressed", fType)) +
      ylab(paste(paste0("#", fType), "(log-scaled counts)")) +
      scale_y_log10()
  })

  # Render Differentially expressed features: summary plot ----
  output$de_features_volcano_plot_summary <- renderPlot({
    print(de_features_volcano_plot_summary())
  })

  # Download DE plot ----
  output$de_features_volcano_plot_summary_export <- downloadHandler(
    filename <- function() {
      fType <- de_analysis()$fType
      paste(paste0("de-P4-", fType), input$de_export_plot_type, sep = ".")
    },
    content <- function(file) {
      width <- 10
      height <- 8
      if (input$de_export_plot_type == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
      } else {
        pdf(file, width = width, height = height)
      }
      print(de_features_volcano_plot_summary())
      dev.off()
    }
  )

  # brush activated on volcano plot ----
  observe({
    if (!is.null((brush <- input$de_features_volcano_plot_brush))) {
      de_features_volcano_plot_range$x <- c(brush$xmin, brush$xmax)
      de_features_volcano_plot_range$y <- c(brush$ymin, brush$ymax)
    } else {
      de_features_volcano_plot_range$x <- NULL
      de_features_volcano_plot_range$y <- NULL
    }
  })

  # Feature selection plot ----
  de_features_volcano_plot_zoom <- reactive({
    de_features_volcano_plot() +
      coord_cartesian(xlim = de_features_volcano_plot_range$x, ylim = de_features_volcano_plot_range$y) +
      geom_text(aes(x = logFC, y = log10P, color = Status, label = paste0("  ", FeatureID), hjust = 0, vjust = .5), size = rel(5), show.legend = FALSE)
  })

  # Volcano plot click ----
  de_features_volcano_plot_click <- reactive({
    validate(need(!is.null((dat <- de_mark_results())), ""))
    x <- data.frame(FeatureID = rownames(dat), x = dat$logFC, y = dat$log10P, stringsAsFactors = FALSE)
    x <- nearPoints(df = x, coordinfo = input$de_features_volcano_plot_click, xvar = "x", yvar = "y", maxpoints = 1)
    if (!nrow(x)) {
      NULL
    }
    else {
      return(x$FeatureID)
    }
  })

  # Selected feature plot ----
  de_features_volcano_plot_focus <- reactive({
    validate(need(!is.null((FeatureID <- de_features_volcano_plot_click())), ""))
    validate(need(!is.null((sampleinfo <- de_sampleplan())), ""))
    fType <- de_analysis()$fType
    x <- data.frame(
      value = dataset()$ExpressionNormCounts[[fType]]$cpm$raw[FeatureID, sampleinfo$SampleID],
      SampleGroup = sampleinfo$SampleGroup,
      GroupLabels = factor(sampleinfo$GroupLabels)
    )
    man_col <- c("#1E90FF", "#FF8C00")
    if (levels(x$GroupLabels)[1] != de_analysis()$grA) {
      x$SampleGroup <- factor(x$SampleGroup, levels(x$SampleGroup)[c(2, 1)])
      man_col <- c("#FF8C00", "#1E90FF")
    }
    ggplot(data = data.frame(SampleID = factor(rownames(x), levels = rownames(x)), x)) +
      geom_bar(aes(x = SampleID, y = value, fill = GroupLabels), stat = "identity", width = .98) +
      scale_fill_manual(values = man_col) +
      scale_color_manual(values = man_col, guide = "none") +
      geom_text(aes(x = SampleID, colour = SampleGroup, y = value, label = paste0(round(value), "\n")),
        size = rel(2.5),
        lineheight = .3,
        hjust = .5,
        vjust = 0,
        show.legend = FALSE
      ) +
      theme_bw() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = rel(1.2), lineheight = 1, vjust = 1, face = "bold"),
        panel.border = element_rect(colour = "grey50", fill = NA),
        axis.text = element_text(size = rel(.7)),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)
      ) +
      ggtitle(paste(FeatureID, gsub("s$", "", fType, perl = TRUE), "expression levels")) +
      ylab("cpm normalized expression")
  })

  # Render Selected feature plot ----
  output$de_features_volcano_plot_focus <- renderPlot({
    update_geom_defaults("point", list(colour = NULL))
    print(de_features_volcano_plot_focus())
    update_geom_defaults("point", list(colour = "#000000"))
  })

  # Download Selected feature plot ----
  output$de_features_volcano_plot_focus_export <- downloadHandler(
    filename <- function() {
      fType <- de_analysis()$fType
      paste(paste0("de-P3-", fType), input$de_export_plot_type, sep = ".")
    },
    content <- function(file) {
      width <- 10
      height <- 8
      if (input$de_export_plot_type == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
      } else {
        pdf(file, width = width, height = height)
      }
      print(de_features_volcano_plot_focus())
      dev.off()
    }
  )

  # Render Feature selection plot ----
  output$de_features_volcano_plot_zoom <- renderPlot({
    de_features_volcano_plot_zoom()
  })

  # Download Feature selection plot ----
  output$de_features_volcano_plot_zoom_export <- downloadHandler(
    filename <- function() {
      fType <- de_analysis()$fType
      paste(paste0("de-P2-", fType), input$de_export_plot_type, sep = ".")
    },
    content <- function(file) {
      width <- 10
      height <- 8
      if (input$de_export_plot_type == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
      } else {
        pdf(file, width = width, height = height)
      }
      print(de_features_volcano_plot_zoom())
      dev.off()
    }
  )

  # MAplot ----
  de_features_ma_plot <- reactive({
    validate(need(!is.null((res <- data.frame(de_analysis()$res))), "No results"))
    validate(need(!is.null((dat <- de_mark_results())), ""))
    validate(need(!is.null((sampleinfo <- de_sampleplan())), "No sample info"))
    dat <- dat[which(!is.na(dat$PValue) & !is.na(dat$FDR)), ] # keep lines with no NA
    fType <- de_analysis()$fType
    cpm <- dataset()$ExpressionNormCounts[[fType]]$cpm$raw[rownames(dat), sampleinfo$SampleID]
    dat$M <- apply(cpm, 1, mean)
    p <- ggplot(data = data.frame(FeatureID = rownames(dat), dat), aes(x = M, y = logFC, colour = Status, text = FeatureID))
    p +
      scale_x_continuous(trans = log10_trans()) +
      geom_point(size = 2, alpha = .5) +
      scale_color_manual(values = c("#808080", "#1E90FF", "#FF8C00"), name = paste("Regulated", fType), drop = FALSE) +
      theme_bw() +
      theme(
        plot.title = element_text(size = rel(1.2), lineheight = 2, vjust = 1, face = "bold"),
        panel.border = element_rect(colour = "grey50", fill = NA)
      ) +
      ggtitle(paste0(substr(fType, 1, nchar(fType) - 1), "-scaled MA plot")) +
      xlab("mean expression") +
      ylab("log2FC")
  })

  # Render MA plot ----
  output$de_features_ma_plot <- renderPlotly({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering data.", detail = "Please wait...", value = 1)
    ggplotly(de_features_ma_plot())
  })

  # Result table ----
  de_results_and_counts <- reactive({
    validate(need(!is.null((res <- de_mark_results())), ""))
    validate(need(!is.null((ctype <- input$de_features_select_counts)), "No counts recap"))
    validate(need(!is.null((ttype <- de_export_list())), ""))
    validate(need(!is.null((count_data <- data_filterCPM())), ""))
    dat <- count_data[[ctype]]
    dat <- merge(res[, -which(names(res) %in% "log10P")], dat, by = "row.names", sort = F)
    names(dat)[1] <- "FeatureID"
    if (project$project_type == "mRNA") {
      dat$FeatureID_simple <- sapply(as.character(dat$FeatureID), function(x) {
        strsplit(x, "\\_|\\:")[[1]][1]
      })
      if (input$de_feature_type == "isoforms") {
        dat$TXID <- sapply(as.character(dat$FeatureID), function(x) {
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
        if (de_analysis()$fType == "genes") {
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
          if (de_analysis()$fType == "genes") {
            dat_annot <- cbind(dat_annot_entrez, dat_annot_genename, dat_annot_gene_type, dat_annot_transcript_type)
            datFinal <- merge(x = dat_annot, y = dat, by.x = "row.names", by.y = "FeatureID_simple")
          } else {
            dat_annot <- cbind(dat_annot_entrez, dat_annot_genename, dat_annot_gene_type)
            datFinal <- merge(x = dat_annot, y = dat, by.x = "row.names", by.y = "FeatureID_simple")
            dat_annot_transcript_type <- data.frame(dat_annot_transcript_type)
            datFinal <- merge(x = datFinal, y = dat_annot_transcript_type, by.x = "TXID", by.y = "row.names")
            colrem <- "TXID"
            datFinal <- datFinal[, !names(datFinal) %in% colrem]
          }
          datFinal <- datFinal[, c("FeatureID", "dat_annot_entrez", "dat_annot_genename", "dat_annot_transcript_type", "dat_annot_gene_type", names(datFinal)[7:length(names(datFinal))])]
          colnames(datFinal)[1:5] <- c("FeatureID", "EntrezID", "Gene Name", "TranscriptType", "GeneBiotype")
        }
      } else { # if anything other than human, mouse, rat, we don't have ensemblDB
        if (all(names(dat_annot_entrez) == names(dat_annot_genename))) {
          dat_annot <- cbind(dat_annot_entrez, dat_annot_genename)
          datFinal <- merge(x = dat_annot, y = dat, by.x = "row.names", by.y = "FeatureID_simple")
          if (de_analysis()$fType == "isoforms") {
            colrem <- "TXID"
            datFinal <- datFinal[, !names(datFinal) %in% colrem]
          }
          datFinal <- datFinal[, c("FeatureID", "dat_annot_entrez", "dat_annot_genename", names(datFinal)[5:length(names(datFinal))])]
          colnames(datFinal)[1:5] <- c("FeatureID", "EntrezID", "Gene Name")
        }
      }
      datFinal$EntrezID <- paste0(
        "<a href='https://www.ncbi.nlm.nih.gov/gene/?term=",
        as.vector(datFinal$EntrezID),
        "' target=\"_blank\"> ",
        as.vector(datFinal$EntrezID),
        " </a>"
      )
      dat <- datFinal
    }

    if (ttype == "1") {
      dat_final <- dat
    } else {
      dat_final <- dat[which(dat$Status %in% c(paste0("Down in ", de_analysis()$grB), paste0("Up in ", de_analysis()$grB))), ]
    }
    return(dat_final)
  })

  # Render DT ----
  output$de_data_out <- DT::renderDataTable({
    dt <- de_results_and_counts()
    sampleIDs <- de_sampleplan()$SampleID
    c_toround <- c("logFC", "FC", sampleIDs)
    c_toscientific <- c("PValue", "FDR")
    DT::datatable(dt,
      rownames = FALSE,
      style = "bootstrap",
      class = "table-bordered",
      escape = F,
      options = list(
        pagingType = "full",
        searching = TRUE,
        scrollX = TRUE,
        scrollY = "500px",
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#00404D', 'color': '#fff'});",
          "}"
        )
      )
    ) %>%
      formatRound(c_toround, 4) %>%
      formatSignif(c_toscientific, digits = 2, interval = 3)
  })

  # Download DT ----
  output$de_data_out_export <- downloadHandler(
    filename <- function() {
      fType <- input$de_feature_type
      fList <- de_export_list()
      paste(paste0("de-", fType), "tsv", sep = ".")
    },
    content <- function(file) {
      # sep=switch(input$de_export_file_type, 'csv'=';', 'tsv'="\t")
      write.table(x = de_results_and_counts(), file = file, sep = "\t", quote = FALSE, row.names = FALSE)
    }
  )

  # Gene list ----
  genes_list_selected <- reactive({
    if (length(grep("up", input$genes_list) > 0)) {
      de_features_volcano_desc()$genes_up_group2
    }
    else if (length(grep("down", input$genes_list) > 0)) {
      de_features_volcano_desc()$genes_up_group1
    }
    else {
      c(de_features_volcano_desc()$genes_up_group1, de_features_volcano_desc()$genes_up_group2)
    }
  })

  # Heatmap ----
  hmap <- reactive({
    validate(need(!is.null((res <- de_mark_results())), ""))
    validate(need(!is.null((sampleinfo <- de_sampleplan())), ""))
    validate(need(!is.null((dat <- data_filterCPM()$cpm)), ""))
    validate(need(!is.null((igenelist <- input$gene_list)), "Loading data"))
    if (igenelist == "up") {
      gene_list <- row.names(res)[grep("Up", res$Status)]
    } else if (igenelist == "down") {
      gene_list <- row.names(res)[grep("Down", res$Status)]
    } else {
      gene_list <- row.names(res)[grepl("Up|Down", res$Status)]
    }
    x <- dat[gene_list, ]
    validate(need(nrow(x) > 1, "Need more than 1 features to plot heatmap"))
    annot <- data.frame(group = sampleinfo$GroupLabels)
    rownames(annot) <- sampleinfo$SampleID
    group <- c("#1E90FF", "#FF8C00")
    names(group) <- c(unique(sampleinfo$GroupLabels)[1], unique(sampleinfo$GroupLabels)[2])
    anno_colors <- list(group = group)
    heatmap.colors <- colorRampPalette(c("#004D89", "#168EE9", "#E9EBEC", "#D73A01", "#B93100"))(100)
    p <- pheatmap::pheatmap(
      mat = as.matrix(x),
      color = colorRampPalette(c("#004D89", "#168EE9", "#E9EBEC", "#D73A01", "#B93100"))(100),
      scale = "row",
      clustering_distance_rows = input$heatmap_select_rowdist,
      clustering_distance_cols = input$heatmap_select_coldist,
      cluster_rows = if (input$dendrogram == "both" | input$dendrogram == "genes") {
        TRUE
      } else {
        FALSE
      },
      cluster_cols = if (input$dendrogram == "both" | input$dendrogram == "samples") {
        TRUE
      } else {
        FALSE
      },
      clustering_method = input$hclustFun,
      annotation_col = annot,
      annotation_colors = anno_colors,
      show_rownames = ifelse(input$heatmap_select_showgenename == "TRUE", TRUE, FALSE)
    )
    return(p)
  })

  # Render Heatmap ----
  output$heatmap <- renderPlot({
    # pdf(NULL)
    hmap()
    # dev.off()
  })

  output$heatmap_out <- renderUI({
    plotOutput(ns("heatmap"), width = "100%", height = "700px")
  })

  # Download heatmap ----
  output$de_features_heatmap_plot_export <- downloadHandler(
    filename <- function() {
      fType <- input$de_feature_type
      paste("de-heatmap", input$de_export_plot_type, sep = ".")
    },
    content <- function(file) {
      width <- 10
      height <- 8
      if (input$de_export_plot_type == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
      } else {
        pdf(file, width = width, height = height)
      }
      print(hmap())
      dev.off()
    }
  )

  # Create outputs ----
  # Recap Selected groups ----
  de_select_groups_recap <- reactive({
    if (length(input$sSelectoutliers) > 0) {
      c(input$sSelectgroupB, " vs ", input$sSelectgroupA, " (outliers: ", input$sSelectoutliers, ")")
    } else {
      c(input$sSelectgroupB, " vs ", input$sSelectgroupA)
    }
  })

  output$de_select_groups_recap <- renderText({
    de_select_groups_recap()
  })
  
  output$de_feature_type_recap <- renderText({
    de_feature_type_recap()
  })
  
  output$de_adjust_P_recap <- renderText({
    de_adjust_P_recap()
  })
  
  output$de_results_P_cutoff_recap <- renderText({
    de_results_P_cutoff_recap()
  })
  
  output$de_results_FC_cutoff_recap <- renderText({
    de_results_FC_cutoff_recap()
  })
  
  output$de_features_select_CPM_cutoff_recap <- renderText({
    de_features_select_CPM_cutoff_recap()
  })
  
  output$de_min_samples_with_CPM <- renderText({
    de_min_samples_with_CPM()
  }) ## NEW
  
  output$de_export_plot_type_recap <- renderText({
    de_export_plot_type_recap()
  })

  # create ID for the dataset that will be be saved
  ds_id <- eventReactive(input$de_data_save, {
    de_data_save$status <- ifelse(de_data_run() != 0, 1, 0)
    if (de_data_save$status != 0) {
      all_ids <- names(de_data_save$data[[de_feature_type_recap()]])
      all_ids <- sapply(all_ids, function(x) {
        strsplit(x, "\\.")[[1]][1]
      })
      ds_id <- paste(c(input$sSelectgroupB, "vs", input$sSelectgroupA, de_method(), input$select_covariates), collapse = "_")
      if (ds_id %in% all_ids) {
        id_table <- table(all_ids)
        i <- id_table[names(id_table) == ds_id]
        i <- i + 1
        ds_id <- paste0(ds_id, ".", as.character(i))
      }
    }
    return(ds_id)
  })

  # SAVE action ----
  observeEvent(input$de_data_save, {
    de_data_save$status <- ifelse(de_data_run() != 0, 1, 0)
    de_data_save$header <- c("Oups...", "Done!")[de_data_save$status + 1]
    validate(need(!is.null((Id <- ds_id())), ""))
    design_formula <- design_formula_pretty()
    de_analysis()$design_formula_pretty
    de_data_save$message <- c("There is nothing to save down here!", paste("Successfully saved as:<b>", Id, "</b>dataset"))[de_data_save$status + 1]
    if (de_data_save$status != 0) {
      validate(need(!is.null((res <- de_mark_results())), ""))
      validate(need(!is.null((sp <- de_sampleplan())), ""))
      P_select <- input$de_adjust_P
      P_cutoff <- input$de_results_P_cutoff
      F_cutoff <- input$de_results_FC_cutoff
      recap <- c(Id, paste(input$sSelectgroupB, "vs", input$sSelectgroupA, collapse = "_"), design_formula, de_features_select_CPM_cutoff_recap(), de_min_samples_with_CPM())
      volcano <- de_features_volcano_plot()
      summaryplot <- de_features_volcano_plot_summary()
      maplot <- de_features_ma_plot()
      de_data_save$data[[de_feature_type_recap()]][[Id]] <- list(
        res = res,
        groupA = input$sSelectgroupA,
        groupB = input$sSelectgroupB,
        SampleGroup = sp,
        P_select = P_select,
        P_cutoff = P_cutoff,
        F_cutoff = F_cutoff,
        outliers = input$sSelectoutliers,
        design = design_formula,
        recap = recap,
        volcano = volcano,
        summaryplot = summaryplot,
        maplot = maplot
      )
    }
  })

  output$recap_saved <- DT::renderDataTable({
    validate(need(!is.null((recap_table <- de_data_save$data[[de_feature_type_recap()]])), ""))
    l <- lapply(recap_table, function(x) {
      unlist(x$recap)
    })
    rec_tb <- data.frame(matrix(unlist(l), nrow = length(l), byrow = T))
    names(rec_tb) <- c("Dataset Name", "Condition", "model", "CPM threshold", "N of samples with CPM threshold")
    # https://rstudio.github.io/DT/002-rowdetails.html
    DT::datatable(
      cbind(" " = "&oplus;", rec_tb),
      escape = -2,
      options = list(
        searching = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 3, 4, 5, 6)),
          list(orderable = FALSE, className = "details-control", targets = 1)
        )
      ),
      callback = JS("
                      table.column(1).nodes().to$().css({cursor: 'pointer'});
                        var format = function(d) {
                        return '<div style=\"background-color:#eee; padding: .5em;\"> Condition: ' +
                            d[3] + '<br> Model: ' + d[4] + '<br> CPM threshold: ' + d[5] + '<br> Samples with CPM: ' + d[6] + '</div>';
                        };
                    table.on('click', 'td.details-control', function() {
                        var td = $(this), row = table.row(td.closest('tr'));
                        if (row.child.isShown()) {
                        row.child.hide();
                        td.html('&oplus;');
                        } else {
                        row.child(format(row.data())).show();
                        td.html('&CircleMinus;');
                        }
                    });")
    )
  })

  # Send message on SAVE ----
  observeEvent(input$de_data_save, {
    session$sendCustomMessage(type = "message", message = list(status = de_data_save$status, header = de_data_save$header, message = de_data_save$message))
  })

  list(
    de_data_save = de_data_save,
    SampleGroup = de_sampleplan,
    de_data_run = de_data_run
  )
}
