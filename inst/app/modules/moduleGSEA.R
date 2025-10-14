moduleENRICHMENTAnalysisUI <- function(id, label = "module gsea") {
  ns <- NS(id)
  tagList(
    column(
      width = 3,
      box(
        width = NULL,
        solidHeader = TRUE,
        status = "warning",
        title = div(icon("cog"), div(style = "display: inline-block; padding-left: 5px", "Analysis Setup")),
        HTML("<div style='color: #2B6587'><b>SELECT ENRICHMENT ANALYSIS TYPE:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
        selectInput(ns("analysis_type"), label = NULL, choices = list("Gene Set Enrichment Analysis" = "gsea", "Over-representation Analysis" = "enrich"), selected = "gsea"),
        htmlOutput(ns("analysis_info")),
        p(),
        p(),
        uiOutput(ns("grouped_analysis_params"))
      ),
      # User GMT ----
      box(
        width = NULL,
        solidHeader = TRUE,
        status = "warning",
        collapsed = TRUE,
        collapsible = TRUE,
        title = div(icon("cog"), div(style = "display: inline-block; padding-left: 5px", "Custom GMT")),
        fileInput(ns("gmtfile"), "Upload a new GMT", multiple = F) %>%
          shinyInput_label_embed(icon("info") %>% bs_embed_tooltip(title = "The GMT file format is a tab delimited file format that describes gene sets. See link below for example. ")),
        radioButtons(ns("geneIdtype"), "Gene Identifiers are:", choiceNames = c("GENESYMBOLS", "ENTREZIDS"), choiceValues = c("gsymbol", "entrezid"), selected = "gsymbol"),
        actionButton(ns("usefile"), "Use Uploaded File"),
        actionButton(ns("resetGMT"), "Use MSigDB Collections"),
        p(),
        h5(tags$a("more about GMT files", href = "http://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#GMT:_Gene_Matrix_Transposed_file_format_.28.2A.gmt.29", target = "blank"))
      )
    ),
    column(
      width = 9,
      # GSEA table ----
      boxPlus(
        width = 12,
        collapsed = FALSE,
        collapsible = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        status = "primary", title = div(icon("file-text-o"), div(style = "display: inline-block; padding-left: 5px", "Enrichment Analysis Results")),
        column(
          width = 12,
          htmlOutput(ns("table_title")),
          uiOutput(ns("analysis_results_out"))
        ),
        p(),
        downloadButton(ns("downloadresults"), "Download")
      ),
      boxPlus(
        width = 12,
        collapsed = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        closable = FALSE,
        status = "primary",
        title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "Enriched terms Plot")),
        sliderInput(ns("dotbarinput"), label = "Number of enriched terms", min = 2, max = 30, value = 10, width = "50%"),
        tabBox(
          width = 12,
          tabPanel(
            "Dotplot",
            column(
              width = 12,
              p(),
              p(),
              p(tags$i("The Dotplot depicts the gene ratios (number of core genes over the total number of genes in the set). The dots are colored by the adjusted p-value and their size is proportional with the size of the gene-set.")),
              uiOutput(ns("resdotplot_px")),
              p(),
              plotDownloadUI(ns("dotplotdownload"))
            )
          ),
          tabPanel(
            "Barplot",
            column(
              width = 12,
              p(),
              p(),
              p(tags$i("The Bar plot depicts the enrichment scores (adjusted p-values) and gene count as bar height and color.")),
              uiOutput(ns("resbarplot_px")),
              p(),
              plotDownloadUI(ns("barplotdownload"))
            )
          )
        )
      ),
      boxPlus(
        width = 12,
        collapsed = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        closable = FALSE,
        status = "primary",
        title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "Gene-Pathway Associations")),
        tabBox(
          width = 12,
          tabPanel(
            "Gene-Concept Network",
            sidebarLayout(
              sidebarPanel(
                width = 2,
                selectInput(ns("netinput"), label = "Number of enriched terms", choices = seq(1, 10, 1), selected = 3)
              ),
              mainPanel(
                width = 10,
                p(),
                p(),
                p(tags$i("The Gene-Concept Network provides information on the linkages of genes and pathways. Genes are colored by their FC and the size of the dots are proportional to the size of the gene-set")),
                withSpinner(plotOutput(outputId = ns("rescnetplot"), width = "auto", height = "500px"), color = "#2093bf"),
                p(),
                plotDownloadUI(ns("cnetplotdownload"))
              )
            )
          ),
          tabPanel(
            "Heatplot",
            sidebarLayout(
              sidebarPanel(
                width = 2,
                selectInput(ns("heatinput"), label = "Number of enriched terms", choices = seq(1, 20, 1), selected = 6),
                checkboxInput(ns("heatFC"), label = "Color by FoldChange", value = F)
              ),
              mainPanel(
                width = 10,
                uiOutput(ns("heatplotPanel"))
              )
            )
          ),
          tabPanel(
            "Upsetplot",
            sidebarLayout(
              sidebarPanel(
                width = 2,
                selectInput(ns("upsetinput"), label = "Number of enriched terms", choices = seq(1, 20, 1), selected = 6)
              ),
              mainPanel(
                width = 10,
                p(),
                p(),
                p(tags$i("The Upsetplot is an alternative to the Gene-Concept Network. We can visualize the number of overlapping genes among different gene-sets.")),
                uiOutput(ns("resupsetplot_px")),
                p(),
                plotDownloadUI(ns("upsetplotdownload"))
              )
            )
          ),
          #          tabPanel(
          #            'Enrichment map',
          #            sidebarLayout(
          #              sidebarPanel(width = 2,
          #                           selectInput(ns('emapinput'), label='Number of enriched terms', choices = seq(1,30,1), selected = 10)
          #                           #sliderInput(ns('emapinput'), label='Number of enriched terms', min = 1, max = 30, value = 10)
          #              ),
          #              mainPanel(
          #                width = 10,
          #                p(),
          #                p(),
          #                p(tags$i("Enrichment map organizes enriched terms into a network with edges connecting overlapping gene sets. In this way, mutually overlapping gene sets are tend to cluster together, making it easy to identify functional module.")),
          #                withSpinner(plotOutput(outputId=ns('resemapplot')),color="#2093bf"),
          #                p(),
          #                plotDownloadUI(ns('emapplotdownload'))
          #              )
          #            )
          #          ),
          tabPanel(
            "Ridgeplot",
            sidebarLayout(
              sidebarPanel(
                width = 2,
                selectInput(ns("ridgeinput"), label = "Number of enriched terms", choices = seq(1, 30, 1), selected = 10)
              ),
              mainPanel(
                width = 10,
                uiOutput(ns("ridgeplotPanel"))
              )
            )
          )
        )
      ),
      box(
        width = 6,
        collapsed = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "primary",
        title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "Heatmap")),
        uiOutput(ns("heatmapBox"))
      ),
      box(
        width = 6,
        collapsed = TRUE,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "primary",
        title = div(icon("file-image-o"), div(style = "display: inline-block; padding-left: 5px", "GSEA Plot")),
        uiOutput(ns("gseaplotBox"))
      )
    )
  )
}


moduleENRICHMENTAnalysis <- function(input, output, session, dataset, de, project) {
  ns <- session$ns

  # GENERAL PARAMETERS ----
  # Analysis info ----
  # gsea/enrich
  output$analysis_info <- renderText({
    gsea_info <- "<i>GSEA aggregates the per gene statistics across genes within a gene set, therefore making it possible to detect situations where <b>all genes in a predefined set change in a small but coordinated way </b>. "
    gsea_info <- paste(gsea_info, "Genes are ranked based on their pvalue and log2FC in the differential analysis. Given a priori defined set of gene (S), the goal of GSEA is to determine whether the members of (S) are randomly distributed throughout the ranked gene list (L) or primarily found at the top or bottom.</i>")
    gsea_info <- paste(gsea_info, "<a href='http://yulab-smu.top/clusterProfiler-book/chapter2.html#gene-set-enrichment-analysis' target='_blank'> more about this method </a>")
    enrich_info <- "<i>Over representation analysis is a widely used approach to determine whether known biological functions or processes are over-represented (= enriched) in an experimentally-derived gene list, e.g. a list of differentially expressed genes (DEGs).</i>"
    enrich_info <- paste(enrich_info, "<a href='http://yulab-smu.top/clusterProfiler-book/chapter2.html#over-representation-analysis' target='_blank'> more about this method </a>")
    if (input$analysis_type == "gsea") {
      text_info <- gsea_info
    } else {
      text_info <- enrich_info
    }
    return(text_info)
  })

  # DB info ----
  output$db_info <- renderText({
    validate(need(!is.null(input$select_GMT), ""))
    main_cat <- strsplit(input$select_GMT, "\\.")[[1]][1]
    if (display_selection$display == TRUE) {
      if (input$select_GMT == "H") {
        tx <- "<i>Hallmark gene sets are coherently expressed signatures derived by aggregating many MSigDB gene sets to represent well-defined biological states or processes.</i>"
      } else if (input$select_GMT == "C1") {
        tx <- "<i>Positional gene sets for each human chromosome and cytogenetic band.</i>"
      } else if (input$select_GMT == "C2") {
        tx <- "<i>Curated gene sets from online pathway databases, publications in PubMed, and knowledge of domain experts. <p style='color:#FF0000';>This is a large dataset, analysis may take a while</p></i> "
      } else if (input$select_GMT == "C2.CGP") {
        tx <- "<i>Gene sets represent expression signatures of genetic and chemical perturbations.<p style='color:#FF0000';>This is a large dataset, analysis may take a while</p></i>"
      } else if (input$select_GMT == "C2.CP") {
        tx <- "<i>Gene sets from pathway databases. Usually, these gene sets are canonical representations of a biological process compiled by domain experts.</i>"
      } else if (input$select_GMT == "C2.CP:BIOCARTA") {
        tx <- "<i>Canonical Pathways gene sets derived from the BioCarta pathway database.</i>"
      } else if (input$select_GMT == "C2.CP:KEGG") {
        tx <- "<i>Canonical Pathways gene sets derived from the KEGG pathway database.</i>"
      } else if (input$select_GMT == "C2.CP:PID") {
        tx <- "<i>Canonical Pathways gene sets derived from the Pathway Interaction Database.</i>"
      } else if (input$select_GMT == "C2.CP:REACTOME") {
        tx <- "<i>Canonical Pathways gene sets derived from the Reactome pathway database.</i>"
      } else if (input$select_GMT == "C2.CP:WIKIPATHWAYS") {
        tx <- "<i>Canonical Pathways gene sets derived from the WikiPathways pathway database.</i>"
      } else if (input$select_GMT == "C3") {
        tx <- "<i>Regulatory target gene sets based on gene target predictions for microRNA seed sequences and predicted transcription factor binding sites.<p style='color:#FF0000';>This is a large dataset, analysis may take a while</p></i>"
      } else if (input$select_GMT == "C3.MIR:MIRDB") {
        tx <- "<i>Gene sets containing high-confidence gene-level predictions of human miRNA targets as catalogued by miRDB v6.0 algorithm<p style='color:#FF0000';>This is a large dataset, analysis may take a while</p></i>"
      } else if (input$select_GMT == "C3.MIR:MIR_Legacy") {
        tx <- "<i>Older gene sets that contain genes sharing putative target sites (seed matches) of human mature miRNA in their 3'-UTRs.</i>"
      } else if (input$select_GMT == "C3.TFT:GTRD") {
        tx <- "<i>Genes that share GTRD v19.10 (Yevshin et al. 2019) predicted transcription factor binding sites in the region -1000,+500 bp around the TSS for the indicated transcription factor. </i>"
      } else if (input$select_GMT == "C3.TFT:TFT_Legacy") {
        tx <- "<i>Older gene sets that share upstream cis-regulatory motifs which can function as potential transcription factor binding sites. Based on work by Xie et al. 2005</i>"
      } else if (input$select_GMT == "C4") {
        tx <- "<i>Computational gene sets defined by mining large collections of cancer-oriented microarray data.</i>"
      } else if (input$select_GMT == "C4.CGN") {
        tx <- "<i>Gene sets defined by expression neighborhoods centered on 380 cancer-associated genes. This collection is described in Subramanian, Tamayo et al. 2005</i>"
      } else if (input$select_GMT == "C4.CM") {
        tx <- "<i>Gene sets defined by Segal et al. 2004. Briefly, the authors compiled gene sets ('modules') from a variety of resources such as KEGG, GO, and others. By mining a large compendium of cancer-related microarray data, they identified 456 such modules as significantly changed in a variety of cancer conditions.</i>"
      } else if (input$select_GMT == "C5") {
        tx <- "<i>Ontology gene sets consist of genes annotated by the same ontology term.<p style='color:#FF0000';>This is a large dataset, analysis may take a while</p></i>"
      } else if (input$select_GMT == "C5.GO:BP") {
        tx <- "<i>Gene sets derived from the GO Biological Process ontology.<p style='color:#FF0000';>This is a large dataset, analysis may take a while</p></i>"
      } else if (input$select_GMT == "C5.GO:CC") {
        tx <- "<i>Gene sets derived from the GO Cellular Component ontology.</i>"
      } else if (input$select_GMT == "C5.GO:MF") {
        tx <- "<i>Gene sets derived from the GO Molecular Function ontology.</i>"
      } else if (input$select_GMT == "C5.HPO") {
        tx <- "<i>Gene sets derived from the Human Phenotype ontology.<p style='color:#FF0000';>This is a large dataset, analysis may take a while</p></i>"
      } else if (input$select_GMT == "C6") {
        tx <- "<i>Oncogenic signature gene sets  defined directly from microarray gene expression data from cancer gene perturbations.</i>"
      } else if (input$select_GMT == "C7") {
        tx <- "<i>Immunologic signature gene sets defined directly from microarray gene expression data from immunologic studies.<p style='color:#FF0000';>This is a large dataset, analysis may take a while</p></i>"
      } else if (input$select_GMT == "C8") {
        tx <- "<i>Cell type signature gene sets curated from cluster markers identified in single-cell sequencing studies of human tissue.</i>"
      }
      tx <- paste0(tx, "<a href='https://www.gsea-msigdb.org/gsea/msigdb/collection_details.jsp#", main_cat, "' target='_blank'> more </a>")
    } else {
      tx <- ""
    }
    return(tx)
  })

  # Chose saved genelist ----
  # genes/isoforms
  fType <- reactive({
    validate(need(length(de$de_data_save$data) > 0, "First you need to run the differential analysis"))
    selectInput(ns("feature_type"), label = NULL, choices = as.character(names(de$de_data_save$data)), selected = 1)
  })

  output$feature_type <- renderUI({
    fType()
  })

  sdatasets <- reactive({
    validate(need(length(de$de_data_save$data) > 0, ""))
    validate(need(!is.null((input$feature_type)), ""))
    validate(need(length(de$de_data_save$data[[input$feature_type]]) > 0, ""))
    choices <- as.character(names(de$de_data_save$data[[input$feature_type]]))
    selectInput(ns("sSelectDatasets"), label = NULL, choices = choices)
  })

  output$sSelectDatasets <- renderUI({
    sdatasets()
  })

  # Parameters for filtering DE results (enrich only) ----
  output$cPanel <- renderUI({
    validate(need(!is.null((dtset <- input$sSelectDatasets)), "Loading data ..."))
    validate(need(!is.null((fType <- input$feature_type)), "Loading data ..."))
    validate(need(!is.null((res <- de$de_data_save$data[[fType]][[dtset]]$res)), "Loading data ..."))
    P_select <- de$de_data_save$data[[fType]][[dtset]]$P_select
    P_cutoff <- de$de_data_save$data[[fType]][[dtset]]$P_cutoff
    F_cutoff <- de$de_data_save$data[[fType]][[dtset]]$F_cutoff
    if (input$analysis_type == "enrich" & length(input$sSelectDatasets) > 0) {
      tagList(
        HTML("<div style='color: #9A9FA1'><b>FILTER DIFFERENTIAL ANALYSIS RESULTS:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
        selectInput(ns("adjust_P"), label = "Adjust p-values:", choices = list("yes (FDR)" = 1, "no" = 2), selected = P_select),
        sliderInput(inputId = ns("results_P_cutoff"), label = "Select FDR (p-value if no adjustment) threshold:", min = .001, max = .1, value = P_cutoff, step = .01),
        sliderInput(inputId = ns("results_FC_cutoff"), label = "Select log2 fold-change threshold:", min = 0, max = 10, value = F_cutoff, step = .5) # ,
      )
    } else {
      NULL
    }
  })

  # Grouped conditional input ----
  # we only display them if differential analysis has been previously conducted
  grouped_analysis_params <- reactive({
    validate(need(project$project_type=="mRNA", "The enrichment analysis is not available for other than mRNA data"))
    validate(need(length(de$de_data_save$data) > 0, "First you need to run the differential analysis"))
    tagList(
      uiOutput(ns("select_GMT")),
      htmlOutput(ns("db_info")),
      p(),
      HTML("<div style='color: #2B6587'><b>SELECT GENELIST (DIFF. ANALYSIS results):</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
      uiOutput(ns("feature_type")),
      uiOutput(ns("sSelectDatasets")),
      uiOutput(ns("cPanel")),
      actionButton(ns("runAnalysis"), label = "Run", icon = icon("caret-square-o-right")),
      awesomeRadio(ns("select_plot_type"), label = "Export plot as:", choices = list("pdf", "png"), status = "warning", inline = TRUE)
    )
  })

  output$grouped_analysis_params <- renderUI({
    grouped_analysis_params()
  })

  # Select gene-set Collection ----
  select_GMT <- reactive({
    # with msigdb library: as.data.frame(msigdbr_collections())
    if (display_selection$display == TRUE) {
      selectInput(
        ns("select_GMT"),
        label = NULL,
        choices = db_choices,
        selected = 1
      )
    } else {
      tagList(
        p(),
        HTML("<div style='color: #E60000'><b>Using custom GMT</b></div>"),
        p()
      )
    }
  })

  output$select_GMT <- renderUI({
    tagList(
      HTML("<div style='color: #2B6587'><b>SELECT GENE-SET COLLECTION:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
      select_GMT()
    )
  })

  # Custom GMT ----
  choseGMT <- reactiveValues(usedfile = "default")

  display_selection <- reactiveValues(display = TRUE)

  observeEvent(input$usefile, {
    choseGMT$usedfile <- "uploaded"
    display_selection$display <- FALSE
  })

  observeEvent(input$resetGMT, {
    choseGMT$usedfile <- "default"
    display_selection$display <- TRUE
  })

  # ENRICHMENT ANALYSIS ----
  # GMT selection ----
  pathways.hallmark <- reactive({
    if (choseGMT$usedfile == "default") {
      organism <- project$organism
      validate(need(!is.null((ID <- input$select_GMT)), ""))
      if (organism == "human") {
        org <- "Homo sapiens"
      } else if (organism == "mouse") {
        org <- "Mus musculus"
      } else if (organism == "rat") {
        org <- "Rattus norvegicus"
      } else if (organism == "zebrafish") {
        org <- "Danio rerio"
      } else if (organism == "dog") {
        org <- "Canis lupus familiaris"
      } else if (orgenism == "pig") org <- "Sus scrofa"
      category <- strsplit(input$select_GMT, "[.]")[[1]][1]
      subcategory <- strsplit(input$select_GMT, "[.]")[[1]][2]
      if (is.na(subcategory)) {
        m_df <- msigdbr(species = org, category = category)
      } else {
        m_df <- msigdbr(species = org, category = category, subcategory = subcategory)
      }
      m_term2gene <- subset(m_df, select = c("gs_id", "entrez_gene")) #
      m_term2name <- subset(m_df, select = c("gs_id", "gs_name"))
    } else {
      m_df <- read.gmt(input$gmtfile$datapath)
      if (input$geneIdtype == "gsymbol") {
        entrezId <- AnnotationDbi::mapIds(x = eval(parse(text = project$slibrary)), keys = m_df$gene, column = "ENTREZID", keytype = "SYMBOL")
        m_df$gene <- entrezId[m_df$gene]
      }
      m_df$cid <- paste0("cid", cumsum(!duplicated(m_df$term)))
      m_term2gene <- m_df %>% dplyr::select(cid, gene) # TERM2GENE
      m_term2name <- m_df %>% dplyr::select(cid, term) # TERM2NAME
    }
    return(list(
      m_term2gene = m_term2gene,
      m_term2name = m_term2name
    ))
  })

  # INPUT data for enrichment analysis ----
  features_subset_and_rank <- eventReactive(input$runAnalysis, {
    validate(need(!is.null(pathways.hallmark()), ""))
    validate(need(!is.null((dtset <- input$sSelectDatasets)), "Loading data ..."))
    validate(need(!is.null((fType <- input$feature_type)), "Loading data ..."))
    validate(need(!is.null((res <- de$de_data_save$data[[fType]][[dtset]]$res)), "Loading data ..."))
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering results.", detail = "Please wait...", value = 1)
    if (input$analysis_type == "enrich") {
      P_select <- ifelse(input$adjust_P == "1", "FDR", "PValue") # select the apropriate pvalue (adjusted or not)
      P_cutoff <- input$results_P_cutoff
      F_cutoff <- input$results_FC_cutoff
      res$Status <- abs(res$logFC) > F_cutoff & res[, P_select] < P_cutoff
      res <- res[which(res$Status > 0), ]
    }
    res$PValue <- ifelse(res$PValue == 0, 5e-324, res$PValue)
    res$fcSign <- sign(res$logFC)
    res$logP <- -log10(res$PValue)
    res$metric <- res$logP / res$fcSign
    if (fType == "genes") {
      res$Gene <- rownames(res)
      if (project$keytype == "ENSEMBL") {
        genesymbol <- AnnotationDbi::mapIds(x = eval(parse(text = project$slibrary)), keys = res$Gene, column = "SYMBOL", keytype = "ENSEMBL")
        res$Gene <- genesymbol[res$Gene]
      }
    } else {
      res$Gene <- sapply(rownames(res), function(x) {
        strsplit(x, ":")[[1]][1]
      })
    }
    y <- res[, c("Gene", "metric")]
    ranks <- deframe(y)
    geneList <- sort(ranks, decreasing = T)
    entrezId <- AnnotationDbi::mapIds(x = eval(parse(text = project$slibrary)), keys = names(geneList), column = "ENTREZID", keytype = "SYMBOL")
    names(geneList) <- entrezId # this is the main vector that is used for the GSEA analyis.
    # entrezids are ranked by log10(pvalue) and logFC sign (+/-)
    y <- res[, c("Gene", "logFC")]
    geneListFC <- deframe(y)
    names(geneListFC) <- entrezId[names(geneListFC)] # this is a second vector with logFC that will be used in ridgeplot
    return(list(
      geneList = geneList,
      geneListFC = geneListFC
    ))
  })

  # ENRICHMENT analysis ----
  # gsea/enrich(over-representation)
  analysisRes <- eventReactive(input$runAnalysis, {
    validate(need(!is.null((geneList <- features_subset_and_rank()$geneList)), "The gene list is empty"))
    validate(need(!is.null((m <- pathways.hallmark())), "There is no GMT"))
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering results.", detail = "Please wait...", value = 1)
    if (input$analysis_type == "gsea") {
      analysisRes_int <- GSEA(geneList,
        TERM2GENE = m$m_term2gene,
        TERM2NAME = m$m_term2name,
        exponent = 1,
        nPerm = 10000,
        minGSSize = 10,
        maxGSSize = 500,
        pvalueCutoff = 1, # no cutoff applied
        pAdjustMethod = "BH",
        verbose = TRUE,
        seed = FALSE,
        by = "fgsea"
      )
    } else if (input$analysis_type == "enrich") {
      analysisRes_int <- enricher(names(geneList),
        TERM2GENE = m$m_term2gene,
        TERM2NAME = m$m_term2name,
        minGSSize = 10,
        maxGSSize = 500,
        pvalueCutoff = 1, # no cutoff applied
        qvalueCutoff = 1,
        pAdjustMethod = "BH"
      )
    }
    return(analysisRes_int)
  })

  # RESULTS table formatted for plots
  analysisRes_form <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes())), ""))
    analysisRes@result$Description <- gsub("_", " ", analysisRes@result$Description)
    return(analysisRes)
  })

  # RESULTS table with genesymbols ----
  analysisRes_symbol <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes())), ""))
    analysisRes_symbol <- setReadable(analysisRes, project$slibrary, "ENTREZID")
    return(analysisRes_symbol)
  })

  # TABLE title (HTML) ----
  table_title <- eventReactive(input$runAnalysis, {
    validate(need(!is.null((analysisRes <- analysisRes())), ""))
    if (choseGMT$usedfile == "default") {
      validate(need(!is.null((ID <- input$select_GMT)), ""))
      ttile <- paste0(
        "<b><h3>",
        names(which(sapply(db_choices, function(x) ID == x))),
        "</h3><b><br><p style=color:#44A0D6><i>Hover over the column names to see their description</i></p>"
      )
    } else {
      ttitle <- "<b>CUSTOM</b>"
    }
  })

  output$table_title <- renderText({
    table_title()
  })

  # FORMAT result table ----
  result_table <- eventReactive(input$runAnalysis, {
    validate(need(!is.null((analysisRes_symbol <- analysisRes_symbol())), ""))
    analysisRes_symbol <- analysisRes_symbol %>%
      as_tibble() %>%
      arrange(p.adjust)
    analysisRes_symbol <- as.data.frame(analysisRes_symbol)
    analysisRes_symbol$Pathway <- paste0("<a href='https://www.gsea-msigdb.org/gsea/msigdb/cards/", analysisRes_symbol$Description, "' target=\"_blank\"> ", analysisRes_symbol$Description, " </a>")
    if (input$analysis_type == "gsea") {
      analysisRes_symbol <- analysisRes_symbol[, c("ID", "Description", "Pathway", "setSize", "enrichmentScore", "NES", "pvalue", "p.adjust", "qvalues", "rank", "leading_edge", "core_enrichment")]
    } else if (input$analysis_type == "enrich") {
      analysisRes_symbol <- analysisRes_symbol[, c("ID", "Description", "Pathway", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "geneID", "Count")]
    }
    return(analysisRes_symbol)
  })

  # TABLE output ----
  # https://stackoverflow.com/questions/32738975/how-can-you-add-an-explanation-to-shiny-datatable-column?noredirect=1&lq=1
  # https://stackoverflow.com/questions/31813601/using-renderdatatable-within-renderui-in-shiny
  output$analysis_results_out_gsea <- DT::renderDataTable({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering results.", detail = "Please wait...", value = 1)
    DT::datatable(result_table(),
      rownames = FALSE,
      selection = "single",
      style = "bootstrap",
      class = "table-bordered",
      escape = F,
      options = list(
        pagingType = "full",
        searching = TRUE,
        scrollX = TRUE,
        scrollY = "400px",
        columnDefs = list(
          list(targets = c(0, 1, 9, 10), visible = FALSE),
          list(
            targets = c(11),
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data != null && data.length > 13 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 13) + '...</span>' : data;",
              "}"
            )
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#00404D', 'color': '#fff'});",
          "}"
        )
      ),
      callback = htmlwidgets::JS("
                   var tips = ['','','Pathway',
                                'Number of genes in the gene set after filtering out those genes not in the expression dataset', 
                                'ES reflects the degree to which a gene set is overrepresented at the top or bottom of a ranked list of genes', 
                                'Normalized Enrichment Score (accounts for differences in gene set size)',
                                'Statistical significance of the enrichment score',
                                'FDR',
                                'Q-value',
                                '',
                                '',
                                'Leading edge: Subset of genes that contributes most to the enrichment result'],
                   header = table.columns().header();
                   for (var i = 0; i < tips.length; i++) {
                      $(header[i]).attr('title', tips[i]);
                   }")
    ) %>% formatRound(c("enrichmentScore", "NES", "pvalue", "p.adjust", "qvalues"), 5)
  })

  output$analysis_results_out_enrich <- DT::renderDataTable({
    DT::datatable(result_table(),
      rownames = FALSE,
      selection = "single",
      style = "bootstrap",
      class = "table-bordered",
      escape = F,
      options = list(
        pagingType = "full",
        searching = TRUE,
        scrollX = TRUE,
        scrollY = "400px",
        columnDefs = list(
          list(targets = c(0, 1), visible = FALSE),
          list(
            targets = c(8),
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data != null && data.length > 13 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 13) + '...</span>' : data;",
              "}"
            )
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
          "}"
        )
      ),
      callback = htmlwidgets::JS("
                      var tips = ['','','Pathway','Number of DE genes in geneset over the total number of DE genes present in this DB', 
                                  'PathwaySize / Size of the DB (total number of genes)', 
                                  'P value',
                                  'FDR', 'Q value', 'Differentially expressed genes in the pathway', 'Count of differentially expressed genes in the pathway'],
                      header = table.columns().header();
                      for (var i = 0; i < tips.length; i++) {
                          $(header[i]).attr('title', tips[i]);
                      }")
    ) %>% formatRound(c("pvalue", "p.adjust", "qvalue"), 5)
  })

  analysis_results_out <- eventReactive(input$runAnalysis, {
    if (input$analysis_type == "gsea") {
      DT::dataTableOutput(ns("analysis_results_out_gsea"))
    } else {
      DT::dataTableOutput(ns("analysis_results_out_enrich"))
    }
  })

  output$analysis_results_out <- renderUI({
    analysis_results_out()
  })

  # Dotplot and Barplot
  dotbarplot_height <- reactive({
    if (input$dotbarinput <= 15) {
      ph <- "400px"
    } else if (input$dotbarinput <= 20) {
      ph <- "500px"
    } else {
      ph <- "600px"
    }
  })

  # Dotplot ----
  resdotplot <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes_form())), ""))
    p <- dotplot(analysisRes, showCategory = input$dotbarinput) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 70))
    return(p)
  })

  output$resdotplot <- renderPlot({
    resdotplot()
  })

  output$resdotplot_px <- renderUI({
    plotOutput(outputId = ns("resdotplot"), height = dotbarplot_height())
  })

  # Barplot ----
  resbarplot <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes_form()@result)), ""))
    if (input$dotbarinput <= nrow(analysisRes)) {
      analysisRes <- analysisRes[1:input$dotbarinput, ]
    } else {
      analysisRes <- analysisRes
    }
    if (input$analysis_type == "gsea") {
      gene_count <- analysisRes %>%
        group_by(ID) %>%
        summarise(count = sum(str_count(core_enrichment, "/")) + 1)
      dot_df <- left_join(analysisRes, gene_count, by = "ID") %>% mutate(GeneRatio = count / setSize)
      dot_df$Description <- factor(dot_df$Description, levels = dot_df$Description[order(-dot_df$p.adjust)])
      dot_df <- as.data.frame(dot_df)
      p <- ggplot(data = dot_df, aes(x = stringr::str_wrap(Description, 70), y = count, fill = p.adjust)) +
        geom_bar(stat = "identity") +
        theme_bw(base_size = 14) +
        scale_fill_gradient(limits = c(0, 0.25), low = "red") +
        xlab(NULL) +
        coord_flip() +
        ylab("GeneCount")
    } else if (input$analysis_type == "enrich") {
      dot_df <- as.data.frame(analysisRes)
      dot_df$Description <- factor(dot_df$Description, levels = dot_df$Description[order(-dot_df$p.adjust)])
      p <- ggplot(data = dot_df, aes(x = stringr::str_wrap(Description, 70), y = Count, fill = p.adjust)) +
        geom_bar(stat = "identity") +
        theme_bw(base_size = 14) +
        scale_fill_gradient(limits = c(0, 0.25), low = "red") +
        xlab(NULL) +
        coord_flip() +
        ylab("GeneCount")
    }
    return(p)
  })

  output$resbarplot <- renderPlot({
    resbarplot()
  })

  output$resbarplot_px <- renderUI({
    plotOutput(outputId = ns("resbarplot"), height = dotbarplot_height())
  })

  # Gene-Concept Network ----
  netinput <- reactive({
    as.numeric(input$netinput)
  })

  rescnetplot <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes_symbol())), "No data"))
    validate(need(!is.null((geneList <- features_subset_and_rank()$geneList)), "The gene list is empty"))
    categories <- netinput()
    analysisRes@result$Description <- gsub("_", " ", analysisRes@result$Description)
    p <- cnetplot(analysisRes, foldChange = geneList, showCategory = categories)
    return(p)
  })

  output$rescnetplot <- renderPlot({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating plot.", detail = "Please wait...", value = 1)
    rescnetplot()
  })

  # Heatmap-like functional classification ----
  heatinput <- reactive({
    as.numeric(input$heatinput)
  })

  resheatplot <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes_symbol())), ""))
    validate(need(!is.null((geneList <- features_subset_and_rank()$geneList)), "The gene list is empty"))
    analysisRes@result$Description <- gsub("_", " ", analysisRes@result$Description)
    if (input$heatFC) {
      FC <- geneList
    } else {
      FC <- NULL
    }
    p <- heatplot(analysisRes, foldChange = FC, showCategory = heatinput())
    p <- p + aes(Gene, stringr::str_wrap(categoryID, 40)) + ylab(NULL)
    return(p)
  })

  output$resheatplot <- renderPlot({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating plot.", detail = "Please wait...", value = 1)
    resheatplot()
  })

  heatplot_height <- reactive({
    if (heatinput() < 15) {
      ph <- "400px"
    } else {
      ph <- "500px"
    }
  })

  heatplotPanel <- reactive({
    if (input$analysis_type == "gsea") {
      tagList(
        p(),
        p(),
        p(tags$i("This plot is not available for the gene set enrichment analysis"))
      )
    } else {
      tagList(
        p(),
        p(),
        p(tags$i("Similar to the Gene-Concept Network, the Heatplot is suitable for the visualisation of a larger number of terms (pathways).")),
        withSpinner(plotOutput(outputId = ns("resheatplot"), height = heatplot_height()), color = "#2093bf"),
        p(),
        plotDownloadUI(ns("heatplotdownload"))
      )
    }
  })

  output$heatplotPanel <- renderUI({
    heatplotPanel()
  })

  # Upsetplot ----
  upsetinput <- reactive({
    as.numeric(input$upsetinput)
  })

  resupsetplot <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes_symbol())), ""))
    analysisRes@result$Description <- gsub("_", " ", analysisRes@result$Description)
    if (input$analysis_type == "gsea") {
      if (upsetinput() <= nrow(analysisRes)) {
        analysisRes <- analysisRes[1:upsetinput(), ]
      } else {
        analysisRes <- analysisRes
      }
      genelist <- sapply(analysisRes$core_enrichment, function(x) strsplit(x, "/")[[1]])
      names(genelist) <- analysisRes$Description
      gm <- list_to_matrix(genelist)
      groups <- colnames(gm)
      gm <- as_tibble(apply(gm, 2, as.logical))
      p <- upset(gm, groups)
    } else if (input$analysis_type == "enrich") {
      p <- enrichplot::upsetplot(analysisRes, n = input$upsetinput) #+ 
    }
    return(p)
  })

  upsetplot_height <- reactive({
    if (upsetinput() < 15) {
      ph <- "400px"
    } else {
      ph <- "500px"
    }
  })

  output$resupsetplot <- renderPlot({
    resupsetplot()
  })

  output$resupsetplot_px <- renderUI({
    withSpinner(plotOutput(outputId = ns("resupsetplot"), height = upsetplot_height()), color = "#2093bf")
  })

  # Enrichment map ----
  emapinput <- reactive({
    as.numeric(input$emapinput)
  })

  resemapplot <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes_symbol())), ""))
    validate(need(!is.null((geneList <- features_subset_and_rank()$geneList)), "The gene list is empty"))
    p <- emapplot(analysisRes, showCategory = emapinput(), color = "p.adjust")
    return(p)
  })

  output$resemapplot <- renderPlot({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating plot.", detail = "Please wait...", value = 1)
    resemapplot()
  })

  # Ridge plot ----
  ridgeinput <- reactive({
    as.numeric(input$ridgeinput)
  })

  resridgeplot <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes_form())), ""))
    validate(need(!is.null((geneListFC <- features_subset_and_rank()$geneListFC)), "The gene list is empty"))
    if (input$analysis_type == "gsea") {
      analysisRes <- analysisRes[1:ridgeinput(), ]
      gs2id <- str_split(analysisRes$core_enrichment, "/")
      names(gs2id) <- analysisRes$ID
      gs2val <- lapply(gs2id, function(id) {
        res <- geneListFC[id]
        res <- res[!is.na(res)]
      })
      nn <- names(gs2val)
      i <- match(nn, analysisRes$ID)
      nn <- analysisRes$Description[i]
      j <- order(analysisRes$NES[i], decreasing = FALSE)
      len <- sapply(gs2val, length)
      gs2val.df <- data.frame(
        category = rep(nn, times = len),
        color = rep(analysisRes[i, "p.adjust"], times = len),
        value = unlist(gs2val)
      )
      colnames(gs2val.df)[2] <- "p.adjust"
      gs2val.df$category <- factor(gs2val.df$category, levels = nn[j])
      p <- ggplot(gs2val.df, aes(value, stringr::str_wrap(category, 60), fill = p.adjust)) +
        ggridges::geom_density_ridges() +
        scale_fill_continuous(
          low = "red", high = "blue", name = "p.adjust",
          guide = guide_colorbar(reverse = TRUE)
        ) +
        xlab("log2FC") +
        ylab(NULL) +
        DOSE::theme_dose()
    }
    return(p)
  })

  output$resridgeplot <- renderPlot({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating plot.", detail = "Please wait...", value = 1)
    resridgeplot()
  })

  ridgeplot_height <- reactive({
    if (input$ridgeinput < 15) {
      ph <- "400px"
    } else if (input$ridgeinput < 25) {
      ph <- "500px"
    } else {
      ph <- "600px"
    }
  })

  ridgeplotPanel <- reactive({
    if (input$analysis_type == "gsea") {
      tagList(
        p(),
        p(),
        p(tags$i("Ridgeplots are density plots of the frequency of log2FC values per gene within each set. It helps to interpret up/down-regulated pathways")),
        withSpinner(plotOutput(outputId = ns("resridgeplot"), height = ridgeplot_height()), color = "#2093bf"),
        p(),
        plotDownloadUI(ns("ridgeplotdownload"))
      )
    } else {
      tagList(
        p(),
        p(),
        p(tags$i("This plot is not available for the over-representation analysis"))
      )
    }
  })

  output$ridgeplotPanel <- renderUI({
    ridgeplotPanel()
  })

  # select pathway for gsea and heatmap plots
  pathwaySelect <- reactive({
    validate(need(!is.null((analysisRes <- analysisRes())), ""))
    pchoices <- analysisRes$Description
    selectInput(ns("pathwaySelect"), "Select Pathway", choices = pchoices, selected = 1)
  })

  output$pathwaySelect <- renderUI({
    pathwaySelect()
  })

  # Heatmap UI ----
  heatmapBox <- reactive({
    tagList(
      uiOutput(ns("pathwaySelect")),
      div(dropdownButton(
        circle = TRUE,
        status = "primary",
        icon = icon("gear"),
        tooltip = tooltipOptions(title = "Click to change parameters !"),
        tags$h3("Heatmap Parameters"),
        size = "sm",
        selectInput(ns("hclustFun"), "Clustering method", c("complete", "single", "average", "mcquitty", "median", "centroid", "ward.D", "ward.D2"), selected = "complete"),
        selectInput(ns("dendrogram"), "Apply Clustering:", c("none", "genes", "samples", "both"), selected = "both"),
        selectInput(ns("enrich_heatmap_select_rowdist"), label = "Feature-based distance metric:", choices = c("correlation", "euclidean", "manhattan", "maximum"), selected = "euclidean"),
        selectInput(ns("enrich_heatmap_select_coldist"), label = "Sample-based distance metric:", choices = c("correlation", "euclidean", "manhattan", "maximum"), selected = "euclidean")
      ), style = "display: inline-block"),
      div(downloadButton(ns("downloadheatmap"), "Download"), style = "display: inline-block"),
      p(),
      plotOutput(outputId = ns("render_heatmap"), width = "auto", height = "500px")
    )
  })

  output$heatmapBox <- renderUI({
    heatmapBox()
  })

  # Heatmap parameters ----
  heatmap_features <- reactive({
    tagList(
      HTML("<div style='color: #9A9FA1'><b>HEATMAP PARAMETERS:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
      column(width = 6, selectInput(ns("enrich_heatmap_select_rowdist"), label = "Gene-based distance metric:", choices = c("correlation", "euclidean", "manhattan", "maximum"), selected = "euclidean")),
      column(width = 6, selectInput(ns("enrich_heatmap_select_coldist"), label = "Sample-based distance metric:", choices = c("correlation", "euclidean", "manhattan", "maximum"), selected = "euclidean")),
      selectInput(ns("hclustFun"), "Clustering linkage col", c("complete", "single", "average", "mcquitty", "median", "centroid", "ward.D", "ward.D2"), selected = "complete"),
      selectInput(ns("dendrogram"), "Apply Clustering:", c("none", "row", "column", "both"), selected = "both"),
      div(
        style = "position: relative; border-style: dotted none none none; border-width: 2px; border-color: rgba(243, 156, 18, .8); width: 100%; height: 45px",
        div(
          style = "position: absolute; right: 0; top: 10px",
          div(actionButton(ns("rmd_plot"), label = "Report", icon = icon("book")), style = "display: inline-block")
        )
      )
    )
  })

  output$heatmap_features <- renderUI({
    heatmap_features()
  })

  # HEATMAP data ----
  data_heatmap <- reactive({
    validate(need(!is.null((i <- analysisRes_symbol())), ""))
    validate(need((!is.null(input$pathwaySelect) & input$pathwaySelect != ""), "Select a pathway to plot"))
    validate(need(!is.null((dtset <- input$sSelectDatasets)), "Loading data ..."))
    validate(need(!is.null((fType <- input$feature_type)), "Loading data ..."))
    SampleGroup <- de$de_data_save$data[[fType]][[dtset]]$SampleGroup
    if (input$analysis_type == "gsea") {
      genes <- i$core_enrichment[which(i$Description == input$pathwaySelect)]
    } else {
      genes <- i$geneID[which(i$Description == input$pathwaySelect)]
    }
    g <- unlist(strsplit(genes, split = "/"))
    m <- subset(x = dataset()$ExpressionNormCounts$genes$cpm$raw[g, ], select = SampleGroup$SampleID)
    list(Id = input$pathwaySelect, m = m)
  })

  # HEATMAP plot ----
  heatmapplot <- reactive({
    validate(need(!is.null(data_heatmap()), ""))
    validate(need(nrow(data_heatmap()$m) > 1, "Only 1 gene : can not represent heatmap"))
    validate(need(!is.null((dtset <- input$sSelectDatasets)), "Loading data ..."))
    validate(need(!is.null((fType <- input$feature_type)), "Loading data ..."))
    SampleGroup <- de$de_data_save$data[[fType]][[dtset]]$SampleGroup
    annot <- data.frame(group = SampleGroup$GroupLabels)
    rownames(annot) <- SampleGroup$SampleID
    group <- c("#1E90FF", "#FF8C00")
    names(group) <- c(unique(SampleGroup$GroupLabels)[1], unique(SampleGroup$GroupLabels)[2])
    anno_colors <- list(group = group)
    pheatmap::pheatmap(
      mat = data_heatmap()$m,
      color = colorRampPalette(c("#004D89", "#168EE9", "#E9EBEC", "#D73A01", "#B93100"))(100),
      scale = "row",
      clustering_distance_rows = input$enrich_heatmap_select_rowdist,
      clustering_distance_cols = input$enrich_heatmap_select_coldist,
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
      main = paste(data_heatmap()$Id, "gene expression")
    )
  })

  output$render_heatmap <- renderPlot({
    heatmapplot()
  })

  # GSEA plot ----
  gseaplotBox <- reactive({
    if (input$analysis_type == "gsea") {
      tagList(
        downloadButton(ns("downloadgseaplot"), "Download"),
        p(),
        plotOutput(outputId = ns("renderplot"), width = "auto", height = "500px")
      )
    } else {
      tagList(
        p(),
        p(),
        p(tags$i("This plot is not available for the over-representation analysis"))
      )
    }
  })

  output$gseaplotBox <- renderUI({
    gseaplotBox()
  })

  pathwayplot <- reactive({
    validate(need((!is.null(input$pathwaySelect) & input$pathwaySelect != ""), "Select a pathway to plot"))
    validate(need(!is.null((analysisRes <- analysisRes_symbol())), ""))
    GSID <- as.character(analysisRes$ID[which(analysisRes$Description == input$pathwaySelect)])
    p <- enrichplot::gseaplot2(analysisRes, geneSetID = GSID, title = input$pathwaySelect)
    return(p)
  })

  output$renderplot <- renderPlot({
    pathwayplot()
  })

  # DOWNLOADS ----
  # Download RESULTS table ----
  output$downloadresults <- downloadHandler(
    filename = function() {
      if (display_selection$display == TRUE) {
        paste0(input$analysis_type, "-", input$select_GMT, ".txt")
      } else {
        paste0(input$analysis_type, "-customGMT.txt")
      }
    },
    content = function(file) {
      write.table(analysisRes_symbol(), file = file, sep = "\t", quote = FALSE, row.names = FALSE)
    }
  )

  plottype <- reactive({
    input$select_plot_type
  })

  # Download dotplot ----
  callModule(plotDownload, "dotplotdownload", resdotplot, "dotplot", plottype)

  # Download barplot ----
  callModule(plotDownload, "barplotdownload", resbarplot, "barplot", plottype)

  # Download cnetplot ----
  callModule(plotDownload, "cnetplotdownload", rescnetplot, "cnetplot", plottype)

  # Download heatmlot ----
  callModule(plotDownload, "heatplotdownload", resheatplot, "heatplot", plottype)

  # Download upsetplot ----
  callModule(plotDownload, "upsetplotdownload", resupsetplot, "upsetplot", plottype)

  # Download emapplot ----
  callModule(plotDownload, "emapplotdownload", resemapplot, "emapplot", plottype)

  # Download ridgeplot ----
  callModule(plotDownload, "ridgeplotdownload", resridgeplot, "ridgeplot", plottype)

  # Download GSEAplot ----
  output$downloadgseaplot <- downloadHandler(
    filename = function() {
      validate(need(!is.null(input$pathwaySelect), ""))
      pathway <- input$pathwaySelect
      if (display_selection$display == TRUE) {
        paste0(input$analysis_type, "-", input$select_GMT, "-", pathway, ".", input$select_plot_type)
      } else {
        paste0(input$analysis_type, "-customGMT-", pathway, ".", input$select_plot_type)
      }
    },
    content = function(file) {
      width <- 12
      height <- 8
      if (input$select_plot_type == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
      } else {
        pdf(file, width = width, height = height)
      }
      print(pathwayplot())
      dev.off()
    }
  )

  # Download Heatmap ----
  output$downloadheatmap <- downloadHandler(
    filename = function() {
      validate(need(!is.null(input$pathwaySelect), ""))
      pathway <- input$pathwaySelect
      if (display_selection$display == TRUE) {
        paste0(input$analysis_type, "-heatmap-", input$select_GMT, "-", pathway, ".", input$select_plot_type)
      } else {
        paste0(input$analysis_type, "-heatmap-customGMT-", pathway, ".", input$select_plot_type)
      }
    },
    content = function(file) {
      width <- 12
      height <- 8
      if (input$select_plot_type == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
      } else {
        pdf(file, width = width, height = height)
      }
      print(heatmapplot())
      dev.off()
    }
  )

  ### save plots for report
  report_save <- reactiveValues()
  observeEvent(input$rmd_plot, {
    report_save$plot$status <- ifelse(input$rmd_plot != 0, 1, 0)
    report_save$plot$header <- c("Oups...", "Done!")[report_save$plot$status + 1]
    report_save$plot$message <- c("First click on run!", paste("Successfully saved as:<b>", (Id <- "enrich_gsea")))[report_save$plot$status + 1]
    if (report_save$plot$status != 0) {
      report_save$plot$report$data[[Id]] <- analysisRes()
      report_save$plot$report$plot[[Id]] <- pathwayplot()
      report_save$plot$report$heatmap[[Id]] <- heatmapplot()
      report_save$summary$collection[[Id]] <- input$select_GMT
      report_save$summary$enrich_heatmap_select_rowdist[[Id]] <- input$enrich_heatmap_select_rowdist
      report_save$summary$enrich_heatmap_select_coldist[[Id]] <- input$enrich_heatmap_select_coldist
      report_save$summary$hclustFun[[Id]] <- input$hclustFun
      report_save$summary$dendrogram[[Id]] <- input$dendrogram
    }
  })

  observeEvent(input$rmd_plot, {
    session$sendCustomMessage(type = "message", message = list(status = report_save$plot$status, header = report_save$plot$header, message = report_save$plot$message))
  })

  rlist <- list(report_save = report_save)
  
  return(rlist)
}
