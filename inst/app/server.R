shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)

  observe_helpers(help_dir = "help_mds")

  project <- reactiveValues()

  chosefile <- reactiveValues(usedfile = "")
  
  observeEvent(input$load, {
    chosefile$usedfile <- "uploaded"
  })
  
  observeEvent(input$example, {
    chosefile$usedfile <- "example"
  })
  
  observe({
    req(chosefile$usedfile)
    if (chosefile$usedfile == "example") {
      organism <- "mouse"
      project_type <- "mRNA"
    } else if (chosefile$usedfile == "uploaded") {
      req(input$project_type)
      req(input$organism)
      project_type <- input$project_type
      organism <- input$organism
    }
    project$project_type <- project_type
    project$organism <- organism
    if (organism == "human") {
      library(org.Hs.eg.db)
      project$slibrary <- "org.Hs.eg.db"
      library(EnsDb.Hsapiens.v86)
      project$enslibrary <- "EnsDb.Hsapiens.v86"
    } else if (organism == "mouse") {
      library(org.Mm.eg.db)
      project$slibrary <- "org.Mm.eg.db"
      library(EnsDb.Mmusculus.v79)
      project$enslibrary <- "EnsDb.Mmusculus.v79"
    } else if (organism == "dog") {
      library(org.Cf.eg.db)
      project$slibrary <- "org.Cf.eg.db"
    } else if (organism == "rat") {
      library(org.Rn.eg.db)
      project$slibrary <- "org.Rn.eg.db"
      library(EnsDb.Rnorvegicus.v79)
      project$enslibrary <- "EnsDb.Rnorvegicus.v79"
    }
  })

  sampleplan <- reactive({
    if (chosefile$usedfile == "example") {
      sampleplan <- fread(file.path("example/SamplePlan.txt"), data.table = F, header = T)
    } else if (chosefile$usedfile == "uploaded") {
      validate(need(!is.null(input$spfile$datapath),'Please select a valid Sample plan file'))
      sampleplan <- fread(input$spfile$datapath, data.table = F, header = T)
    } else {
      sampleplan <- NULL
    }
    return(sampleplan)
  })

  rawcounts <- reactive({
    if (chosefile$usedfile == "example") {
      rawcounts <- fread(file.path("example/global_expression_genes_raw_subset.tsv"), data.table = F, header = T)
    } else if (chosefile$usedfile == "uploaded") {
      validate(need(!is.null(input$countfile$datapath),'Please select a valid file'))
      rawcounts <- fread(input$countfile$datapath, stringsAsFactors = F, check.names = FALSE, data.table = F, header = T)
      print(head(rawcounts))
    } else {
      rawcounts <- NULL
    }
    return(rawcounts)
  })

  expr_counts <- reactive({
    req(rawcounts())
    req(sampleplan())
    g_raw <- rawcounts()
    SamplePlan <- sampleplan()
    validate(need(length(which(names(g_raw) %in% c("Gene_name", "Ensembl_ID", "Entrez_ID"))) > 0, "You need to have at least one of these columns in your data: 'Gene_name' or 'Ensembl_ID' "))
    gnames <- names(g_raw)[which(names(g_raw) %in% c("Gene_name", "Ensembl_ID", "Entrez_ID"))]
    snumber <- length(SamplePlan$SampleID)
    validate(need(length(as.character(SamplePlan$SampleID)[which(as.character(SamplePlan$SampleID) %in% colnames(g_raw))])==snumber,''))
    sampleIDs <- trimws(as.character(SamplePlan$SampleID))
    g_raw <- g_raw[, c(gnames, sampleIDs)]
    if ("Gene_name" %in% gnames) {
      g_raw$rsum <- apply(g_raw[, sampleIDs], 1, sum)
      g_raw <- g_raw[with(g_raw, order(-rsum)), ]
      if (length(which(duplicated(g_raw$Gene_name))) > 0) {
        g_raw <- g_raw[-which(duplicated(g_raw$Gene_name)), ]
      }
      rownames(g_raw) <- g_raw$Gene_name
      project$keytype <- "SYMBOL"
    } else if ("Ensembl_ID" %in% gnames) {
      rownames(g_raw) <- g_raw$Ensembl_ID
      project$keytype <- "ENSEMBL"
    } else if ("Entrez_ID" %in% gnames) {
      rownames(g_raw) <- g_raw$Entrez_ID
      project$keytype <- "ENTREZID"
    }
    g_raw <- g_raw[, sampleIDs]
    print("g_raw")
    ExpressionCounts <- list()
    ExpressionCounts$genes <- g_raw[sort(rownames(g_raw)), ]
    print(head(ExpressionCounts$genes))
    return(
      list(
        ExpressionCounts = ExpressionCounts
      )
    )
  })

  expr_norm_counts <- reactive({
    req(expr_counts())
    req(sampleplan())
    ExpressionCounts <- expr_counts()$ExpressionCounts
    SamplePlan <- sampleplan()
    SamplePlan$SampleID <- factor(SamplePlan$SampleID, levels = SamplePlan$SampleID)
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Normalizing data.", detail = "Please wait...", value = 1)
    ExpressionNormCounts <- sapply(names(ExpressionCounts), function(Feature) {
      dds <- DESeqDataSetFromMatrix(countData = round(ExpressionCounts[[Feature]]), colData = SamplePlan, design = ~1)
      # test weather dds can work with vst function. Otherwise the varianceStabilizingTransformation will be used
      # the following lines are from the vst function validation part
      if (is.null(sizeFactors(dds)) & is.null(normalizationFactors(dds))) {
        dds_test <- estimateSizeFactors(dds)
      }
      baseMean <- rowMeans(counts(dds_test, normalized = TRUE))
      if (nrow(dds) < 1000 | sum(baseMean > 5) < 1000) {
        print("varianceStabilizingTransformation")
        dt <- list(
          cpm = lapply(list(raw = FALSE, log = TRUE), function(x) cpm(ExpressionCounts[[Feature]], normalized.lib.sizes = FALSE, log = x)),
          vst = assay(varianceStabilizingTransformation(dds))
        )
      } else {
        dt <- list(
          cpm = lapply(list(raw = FALSE, log = TRUE), function(x) cpm(ExpressionCounts[[Feature]], normalized.lib.sizes = FALSE, log = x)),
          vst = assay(vst(dds))
        )
      }
      colnames(dt$vst) <- levels(SamplePlan$SampleID)
      return(dt)
    }, simplify = FALSE)
    print("normalize")
    print(head(ExpressionNormCounts$genes$cpm$raw))
    return(
      list(
        ExpressionNormCounts = ExpressionNormCounts
      )
    )
  })

  dataset <- reactive({
    req(expr_counts())
    req(expr_norm_counts())
    req(sampleplan())
    SamplePlan <- sampleplan()
    ExpressionCounts <- expr_counts()$ExpressionCounts
    ExpressionNormCounts <- expr_norm_counts()$ExpressionNormCounts
    dataset <- list(
      SamplePlan = SamplePlan,
      ExpressionCounts = ExpressionCounts,
      ExpressionNormCounts = ExpressionNormCounts
    )
  })

  newsp <- callModule(moduleSampleplan, "sampleplan", dataset)

  summary_out <- reactive({
    t0 <- "SUMMARY STATISTICS<br/>"
    t0 <- paste(t0, paste0("Total number of samples: ", dim(dataset()$ExpressionCounts$genes)[2]), sep = "<br/>")
    t0 <- paste(t0, paste0("Total number of genes: ", dim(dataset()$ExpressionCounts$genes)[1]), sep = "<br/>")
  })

  output$datasummary <- renderUI({
    HTML(summary_out())
  })

  gobject <- reactive({
    req(newsp$sptable())
    use_group <- newsp$use_group()
    default.colors <- c(brewer.pal(12, "Paired")[1:length(levels(as.factor(newsp$sptable()$df[, use_group])))])
    names(default.colors) <- c(levels(as.factor(newsp$sptable()$df[, use_group])))
    list(default.colors = default.colors)
  })

  exprov <- callModule(moduleEO, "expression_overview", dataset, newsp, project)
  fs <- callModule(moduleFS, "expression_features", dataset, gobject, newsp)
  pca <- callModule(modulePCA, "pca_plot", gobject, dataset, de, newsp)
  de <- callModule(moduleDE, "DE", dataset, newsp, project)
  geneset <- callModule(moduleSET, "gene_sets", dataset, de, gobject, newsp)
  enrich <- callModule(moduleENRICHMENTAnalysis, "enrich", dataset, de, project)
  callModule(moduleReport, "Report", newsp, exprov, fs, de, pca, enrich, geneset)

  output$sessionInfo_R <- renderPrint({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering data.", detail = "Please wait...", value = 1)
    print(sessioninfo::session_info())
  })

})
