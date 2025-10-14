### courtesy to PIVOT: Platform for Interactive analysis and Visualization Of Transcriptomics data 



##### Report module #####

# UI

moduleReportUI <- function(id, label="report module"){
  ns <- NS(id)
  tagList(
    uiOutput(ns("report_ui"))
  ) 
}


moduleReport <- function(input, output, session, newsp, exprov, fs, de, pca, enrich, geneset){
  
  ns <- session$ns
  
  # define report_ui ----
  output$report_ui <- renderUI({
    isolate({ # First render
      if(is.null(input$report_rmd)) {
        content1 <- "# rmarkdown code  ..."
      }
    })
    list(
      fluidRow(
        column(
          width = 12,
          box(
            width = 12,
            title = "R-markdown editor",
            status = "warning",
            solidHeader = T,
            collapsible = T,
            fluidRow(
              column(
                3,
                HTML("<div style='color: #000000'><b>REPORT PARAMETERS:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
                uiOutput(ns('fs_list')),  # feature-specific expressions for reporting
                uiOutput(ns('pca_list')),  # PCA for reporting
                uiOutput(ns('de_list')),  # Differential analysis for reporting
                uiOutput(ns('set_list')),  # Interaction plots for reporting
                uiOutput(ns('enrich_list')),  # Enrichment analysis - Fisher for reporting
                textAreaInput(ns('text_input'), 'Comments', placeholder = 'add your comments here', height = '50px')
                
              ),
              column(
                9,
                shinyAce::aceEditor(ns("rmd_code"), mode="markdown", height = "800px", value = content1)#,
                #   shinyAce::aceEditor(ns("report_rmd"), mode="markdown", height = "600px", value = content2)
              )
            ),
            fluidRow(
              column(
                4,
                actionButton(ns("getRmd"), "Get Rmd code", class = "btn btn-info"),
                actionButton(ns("evalRmd"), "Update Preview", class = "btn btn-success"),
                actionButton(ns("clearRmd"), "Clear editor", class = "btn btn-light")
                #downloadButton("savePreview", "Save Preview", class = "btn btn-success")
              ),
              column(
                3, 
                rmdDownloadUI(ns("rmd_export"))
              )
            )
          ),
          box(
            width = 12,
            title = "Report preview",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            htmlOutput(ns("reportPreview"))
          )
        )
      )
    )
  })
  
  
  
  # Header info: title, author, date, toc, theme ----
  #    rmd_static_codes <- reactive({
  #        paste0("---\ntitle: '", input$report_title, "'\nauthor: '", input$report_author,
  #            "'\ndate: '", Sys.Date(), "'\noutput:\n  html_document:\n    toc: ",
  #            input$report_toc, "\n    number_sections: ", input$report_ns,
  #            "\n    theme: ", input$report_theme, "\n---\n\n"
  #        )
  #    })
  
  
  rmd_static_codes <- reactive({
    paste0("---\ntitle: 'RNA-seq analysis report'\nauthor: 'iCONICS'\ndate: '", Sys.Date(), "'\noutput:\n  html_document:\n    toc: true\n    number_sections: true\n    theme: default\n---\n\n", input$text_input,"\n\n"
    )
  })
  
  
  # Study Design ----
  # get values
  # output values
  # create rmd code
  
  #    sp_list=reactive({
  #        if(!is.null((x=newsp$report_save$table))){
  #            tagList(
  #                selectInput(ns('sp_list'), 'Study Design', choices=names(x), multiple=T, selected=names(x))
  #            )
  #        }
  #    })
  
  
  #    output$sp_list=renderUI({ sp_list() })
  
  
  #    sp_rmd <- reactive({
  #        if(!is.null(input$sp_list)){
  #            paste0(
  #                "# Study Design\n\n```{r echo = FALSE, warning=FALSE, message = FALSE}\n\n
  #                DT::datatable(newsp$report_save$table[[\"",input$sp_list,"\"]], rownames=FALSE,
  #                selection = 'single', options = list(scrollX = TRUE, scrollY = '400px', lengthMenu = c(10, 20)))\n\n```\n\n"
  #            )
  #        }
  #    }) 
  sp_rmd <- reactive({
    paste0(
      "# Study Design\n\n```{r echo = FALSE, warning=FALSE, message = FALSE}\n\n
            # gr = newsp$sptable()$df[,newsp$use_group()]
            # dat = as.data.frame(table(gr))
             dat = newsp$sptable()$df
            # names(dat) = c('Condition','Number of samples')
             DT::datatable(dat, rownames=FALSE, width = '100%',
             options = list(searching = FALSE, paging = TRUE))\n\n```\n\n"
    )
  })
  
  
  # Mapping overview ----
  # get values (plots/tables)
  # output values
  # create rmd code
  
  
  
  #    mapping_list=reactive({  # make a list of saved plots and tables
  #        if (!is.null(x=mapping$report_save$plot$report)){
  #            tagList(
  #                selectInput(ns('mapping_list'), 'Mapping overview', choices=names(mapping$report_save$plot$report), multiple=T, selected = names(mapping$report_save$plot$report))
  #            )
  #        }
  #    })
  
  
  #    output$mapping_list = renderUI({ mapping_list() })
  
  
  #    mapping_rmd = reactive({
  #        if(!is.null(input$mapping_list)){
  #            out <- "# Mapping overview\n\n```{r echo = FALSE, warning=FALSE, message = FALSE}"
  #            for (i in 1:length(input$mapping_list)){
  #                out <- c(out, paste0('mapping$report_save$plot$report[[\"',input$mapping_list[i],'\"]]\n'))
  #                out <- c(out, paste0('DT::datatable(mapping$report_save$table$report[[\"',input$mapping_list[i],'\"]], rownames=FALSE,
  #                              selection = "single", options = list(scrollX = TRUE, scrollY = "500px", lengthMenu = c(20, 50, 100)))\n'))
  #            }
  #            out <- c(out, "```\n\n")
  #            rmd <- paste(out, collapse = "\n")
  #        }
  #    })
  
  
  # get values
  # output values
  # create rmd code
  
  #    eo_list = reactive({
  #        if(!is.null((x=exprov$report_save$plot$report))){
  #            tagList(
  #                selectInput(ns('eo_list'), 'Global expressions', choices=names(x), multiple=T, selected = names(x))
  #            )
  #        }
  #    })
  
  
  #    output$eo_list = renderUI({ eo_list() })
  
  
  #    eo_rmd = reactive({
  #        if(!is.null(input$eo_list)){
  #            out <- "# Global expressions\n\n```{r echo = FALSE, warning=FALSE, message = FALSE}"
  #            for (i in 1:length(input$eo_list)){
  #                out <- c(out, paste0('exprov$report_save$plot$report[[\"',input$eo_list[i],'\"]]\n'))
  #            }
  #            out <- c(out, "```\n\n")
  #            rmd <- paste(out, collapse = "\n")
  #        }
  #    })
  
  
  
  # EO - Feature Specific ----
  # get values
  # output values
  # create rmd code
  
  fs_list = reactive({
    if(!is.null((x=fs$report_save$plots))){
      tagList(
        selectInput(ns('fs_list'), 'Feature-specific expressions', choices=names(x), multiple=T, selected = names(x))
      )
    }
  })
  
  
  output$fs_list = renderUI({ fs_list() })
  
  
  fs_rmd = reactive({
    if(!is.null(input$fs_list)){
      out <- "# Feature-specific expressions {.tabset}\n\n"
      for (i in 1:length(input$fs_list)){
        #            g = strsplit(input$fs_list[i],"_")[[1]][1]
        g = input$fs_list[i]
        out <- c(out, paste0("## ",g," {.tabset}\n"))
        out <- c(out, "```{r echo = FALSE, warning=FALSE, message = FALSE}")
        gplots <- names(fs$report_save$plots[[g]])
        for (j in 1:length(gplots)){
          out <- c(out, paste0('fs$report_save$plots[[\"',g,'\"]][[\"',gplots[j],'\"]]\n'))
        }
        # out <- c(out, paste0('fs$report_save$plot$report[[\"',input$fs_list[i],'\"]]\n'))
        out <- c(out, "```\n\n")
      }        
      rmd <- paste(out, collapse = "\n")
    }
    #       return(rmd)
  })
  
  
  # PCA ----
  # get values
  # output values
  # create rmd code
  
  pca_list = reactive({
    if(!is.null((x=pca$report_save$plot$report))){
      tagList(
        selectInput(ns('pca_list'), 'PCA', choices=names(x$plot1), multiple=T, selected = names(x$plot1))
      )
    }
  })
  
  
  output$pca_list = renderUI({ pca_list() })
  
  
  pca_rmd = reactive({
    if(!is.null(input$pca_list)){
      out <- "# Principal Component Analysis\n\n## Filters: \n\n```{r echo = FALSE, warning=FALSE, message = FALSE}"
      # filtes
      out <- c(out,'
            dataview = ifelse(pca$report_save$summary$pca_features_select_data_view==1,"genes","isoforms")
            scaleunit = ifelse(pca$report_save$summary$pca_features_select_scaleunit==1,"Data is scaled","Data is not scaled")
            filteredout = ifelse(pca$report_save$summary$pca_features_filterout==1,"Low expressed features have been filtered out","Low expressed features have not been filtered out")
            dataview = ifelse(pca$report_save$summary$pca_features_select_data_view==1,"genes","isoforms")
            scaleunit = ifelse(pca$report_save$summary$pca_features_select_scaleunit==1,"Data is scaled","Data is not scaled")
            exclude_outliers = ifelse(!is.null(pca$report_save$summary$exclude_outliers),paste(pca$report_save$summary$exclude_outliers, collapse = " "),"None")
            exclude_groups = ifelse(!is.null(pca$report_save$summary$exclude_groups),paste(pca$report_save$summary$exclude_groups, collapse = " "),"None")')
      out <- c(out, "```\n\n")
      out <- c(out, "* Data type: `r dataview`")
      out <- c(out, "* `r scaleunit`")
      out <- c(out, "* `r filteredout`")
      out <- c(out, "* Excluded samples: `r exclude_outliers`")
      out <- c(out, "* Excluded groups: `r exclude_groups`")
      out <- c(out,"\n\n## Principall Components\n\n```{r echo = FALSE, warning=FALSE, message = FALSE}")
      # plots
      out <- c(out, paste0('pca$report_save$plot$report$plot1[["PCA_plots"]]\n\n'))
      out <- c(out, paste0('pca$report_save$plot$report$plot2[["PCA_plots"]]\n\n'))
      out <- c(out, paste0('pca$report_save$plot$report$plot3[["PCA_plots"]]\n\n'))
      out <- c(out, "```\n\n")
      # heatmap
      out <- c(out,"## Sample Distance Heatmap\n\n```{r echo = FALSE, warning=FALSE, message = FALSE}")
      out <- c(out, paste0('pca$report_save$plot$report$heatmap[["PCA_plots"]]\n\n```\n\n'))
      
      rmd <- paste(out, collapse = "\n")
    }
  })
  
  
  
  # Differential analysis ----
  # get values
  # output values
  # create rmd code
  
  
  ftypes = reactive({ names(de$de_data_save$data) })
  
  
  de_list=reactive({
    if (!is.null((ftypes()))){
      de_l = list()
      if ('genes' %in% ftypes()){
        de_choices = names(de$de_data_save$data[['genes']])
        de_l = list(
          HTML("<div style='color: #9A9FA1'><b>GENE-based:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
          pickerInput(inputId = ns("de_genes"),label = "Select differential analysis results:", choices = de_choices, 
                      options = list(
                        `actions-box` = TRUE, 
                        size = 10,
                        `selected-text-format` = "count > 3"
                      ), 
                      multiple = TRUE
          )
        )
      }
      if ('isoforms' %in% ftypes()){
        de_choices = names(de$de_data_save$data[['isoforms']])
        de_l2 = list(
          HTML("<div style='color: #9A9FA1'><b>TRANSCRIPT-based:</b></div><hr style='margin-top: 5px; margin-bottom: 10px'>"),
          pickerInput(inputId = ns("de_isoforms"),label = "Select differential analysis results:", choices = de_choices,           
                      options = list(
                        `actions-box` = TRUE,
                        size = 10,
                        `selected-text-format` = "count > 3"
                      ), 
                      multiple = TRUE
          )
        )
        de_l = c(de_l,de_l2)
      }
      return(de_l)
    }
  })
  
  output$de_list=renderUI({ de_list() })
  
  ds_select = reactive({
    validate(need(!is.null((ftypes=ftypes())),''))
    ds = list()
    if (('genes' %in% ftypes) & (!is.null(input$de_genes))){
      ds[['genes']]=input$de_genes
    }
    if (('isoforms' %in% ftypes) & (!is.null(input$de_isoforms))){
      ds[['isoforms']]=input$de_isoforms
    }
    return(ds)
  })
  
  
  
  de_rmd <- reactive({
    if(!is.null((ds_select=ds_select()))){
      ftypes = names(ds_select)
      out <- "# Differential analysis {.tabset}\n\n"
      for (ty in ftypes){
        out <- c(out,paste0('## ',ty,' {.tabset}\n'))
        for (ds in ds_select[[ty]]){
          out <- c(out, paste0('### ',ds,' {.tabset}\n'))
          out <- c(out, '\n```{r echo = FALSE, warning=FALSE, message = FALSE, fig.show="hold", out.width="50%"}')
          out <- c(out, paste0('dat = de$de_data_save$data[["',ty,'"]][["',ds,'"]]'))
          out <- c(out, 'design = paste(as.character(dat$design), collapse = " ")')
          out <- c(out, 'P_select=ifelse(dat$P_select == "1", "yes", "no")')
          out <- c(out, 'outliers = ifelse(is.null(dat$outliers),"NONE",paste(dat$outliers,collapse=","))')
          out <- c(out, 'de_params = data.frame(model = design,"excluded samples" = outliers,"CPM cutoff" = dat$recap[[4]],"Min samples with CPM" = dat$recap[[5]],"Pvalue is adjusted"=P_select,"Pvalue cutoff"=dat$P_cutoff,"log2FC cutoff"=dat$F_cutoff)')
          out <- c(out, 'DT::datatable(t(de_params), colnames = c("Parameters","Values"), width ="50%", 
                               options = list(searching = FALSE, paging = FALSE))')
          out <- c(out, 'dat$volcano')
          out <- c(out, 'dat$summaryplot')
          out <- c(out, 'dat$maplot')
          #                out <- c(out, 'dat$heatmap')
          out <- c(out,'```\n')
        }
      }
      rmd <- paste(out, collapse = "\n")
    }
  })
  
  
  # Gene Set Interactions ----    
  
  set_list = reactive({
    if(!is.null((x=geneset$report_save$plot1))){
      tagList(
        selectInput(ns('set_list'), 'GeneSet Interactions', choices=names(x), multiple=T, selected = names(x))
      )
    }
  })
  
  
  output$set_list = renderUI({ set_list() })
  
  set_rmd = reactive({
    if(!is.null((allsets=input$set_list))){
      out <- "# Gene Set Interactions {.tabset}\n\n"
      for (i in 1:length(allsets)){
        out <- c(out, paste0("## Interaction.",i," {.tabset}\n"))
        out <- c(out,"```{r echo = FALSE, warning=FALSE, message = FALSE}")
        out <- c(out, paste0('grid.draw(geneset$report_save$plot1[["',allsets[i],'"]])\n\n'))
        out <- c(out, paste0('grid.draw(geneset$report_save$plot2[["',allsets[i],'"]])\n\n'))
        out <- c(out, paste0('grid.draw(geneset$report_save$plot3[["',allsets[i],'"]])\n\n'))
        out <- c(out, "```\n\n")
      }
      rmd <- paste(out, collapse = "\n")
    }
  })
  
  
  # Enrichment analysis - Fisher ----
  # get values
  # output values
  # create rmd code
  
  enrich_list=reactive({
    if(!is.null((x=enrich$report_save$plot$report))){
      tagList(
        selectInput(ns('enrich_list'), 'Enrichment analysis - Fisher', choices=names(x$data), multiple=T, selected = names(x$data))
      )
    }
  })
  
  
  output$enrich_list=renderUI({ enrich_list() })
  
  
  enrich_rmd <- reactive({
    if(!is.null(input$enrich_list)){
      i = 1
      out <- "```{r echo = FALSE, warning=FALSE, message = FALSE}"
      ## filters
      
      out <- c(out,
               paste0("term = c('cellular component','biological process','molecular function','reactome pathway')[as.numeric(enrich$report_save$summary$enrich_features_select_term[[\"",input$enrich_list[i],"\"]])]"),
               paste0("padj_enrich = ifelse(enrich$report_save$summary$enrich_features_select_P[[\"",input$enrich_list[i],"\"]]==1,'P-value adjusted (FDR)','P-value is not adjusted')"),
               paste0("pcutoff_enrich = enrich$report_save$summary$enrich_features_select_P_cutoff[[\"",input$enrich_list[i],"\"]]"),
               paste0("Fcutoff_enrich = enrich$report_save$summary$enrich_features_select_F_cutoff[[\"",input$enrich_list[i],"\"]]"),
               paste0("Qcutoff_enrich = enrich$report_save$summary$enrich_features_select_Q_cutoff[[\"",input$enrich_list[i],"\"]]"),
               paste0("Ncutoff_enrich = enrich$report_save$summary$enrich_features_select_N_cutoff[[\"",input$enrich_list[i],"\"]]"),
               paste0("heatmap_rowdist_enrich = enrich$report_save$summary$enrich_heatmap_select_rowdist[[\"",input$enrich_list[i],"\"]]"),
               paste0("heatmap_coldist_enrich = enrich$report_save$summary$enrich_heatmap_select_coldist[[\"",input$enrich_list[i],"\"]]"),
               paste0("hclustfun_enrich = enrich$report_save$summary$hclustFun[[\"",input$enrich_list[i],"\"]]"),
               paste0("dendrogram_enrich = enrich$report_save$summary$dendrogram[[\"",input$enrich_list[i],"\"]]"))
      
      out <- c(out, "```\n\n")
      
      out <- c(out, "# Enrichment Analysis\n\n ")
      out <- c(out, "## Filters\n\n")
      out <- c(out, "* Enrichment term: `r term`")
      out <- c(out, "* `r padj_enrich`")
      out <- c(out, "* P-value cutoff: `r pcutoff_enrich`")
      out <- c(out, "* Q-value cutoff: `r Qcutoff_enrich`")
      out <- c(out, "* log2 Fold-Change cutoff: `r Fcutoff_enrich`")
      out <- c(out, "* Minimum number of genes `r Ncutoff_enrich`")
      out <- c(out, "\n\n")
      
      ## table
      out <- c(out,"```{r echo = FALSE, warning=FALSE, message = FALSE}")
      out <- c(out,paste0('DT::datatable(enrich$report_save$plot$report$data[[\"',input$enrich_list[i],'\"]], rownames=FALSE,
                                   selection = "single", options = list(scrollX = TRUE, scrollY = "500px", lengthMenu = c(20, 50, 100)))\n'))
      out <- c(out, "```\n\n")
      
      ## plot
      out <- c(out,"```{r echo = FALSE, warning=FALSE, message = FALSE}")
      out <- c(out, paste0('enrich$report_save$plot$report$plot[[\"',input$enrich_list[i],'\"]]\n\n'))
      out <- c(out, "```\n\n")
      
      ## heatmap
      out <- c(out, "### Heatmap ")
      out <- c(out, "* Row-distance: `r heatmap_rowdist_enrich`")
      out <- c(out, "* Column-distance: `r heatmap_coldist_enrich`")
      out <- c(out, "* Clustering function: `r hclustfun_enrich`")
      out <- c(out, "* Dendrogram: `r dendrogram_enrich`")
      out <- c(out,"\n\n```{r echo = FALSE, warning=FALSE, message = FALSE}")
      out <- c(out, paste0('enrich$report_save$plot$report$heatmap[[\"',input$enrich_list[i],'\"]]\n\n'))
      out <- c(out, "```\n\n")
      
      rmd <- paste(out, collapse = "\n")
    }
  })
  
  
  
  
  
  # Enrichment analysis - GSEAr ----
  # get values
  # output values
  # create rmd code
  
  gsea_list=reactive({
    if(!is.null((x=gsea$report_save$plot$report))){
      tagList(
        selectInput(ns('gsea_list'), 'Enrichment analysis - GSEA', choices=names(x$data), multiple=T, selected = names(x$data))
      )
    }
  })
  
  
  output$gsea_list=renderUI({ gsea_list() })
  
  
  gsea_rmd <- reactive({
    if(!is.null(input$gsea_list)){
      #  for (i in 1:length(input$gsea_list)){
      i = 1
      
      out <- "```{r echo = FALSE, warning=FALSE, message = FALSE}"
      ## filters
      
      out <- c(out,
               paste0("collection = gsea$report_save$summary$collection[[\"",input$gsea_list[i],"\"]]"),
               paste0("heatmap_rowdist_gsea = gsea$report_save$summary$enrich_heatmap_select_rowdist[[\"",input$gsea_list[i],"\"]]"),
               paste0("heatmap_coldist_gsea = gsea$report_save$summary$enrich_heatmap_select_coldist[[\"",input$gsea_list[i],"\"]]"),
               paste0("hclustfun_gsea = gsea$report_save$summary$hclustFun[[\"",input$gsea_list[i],"\"]]"),
               paste0("dendrogram_gsea = gsea$report_save$summary$dendrogram[[\"",input$gsea_list[i],"\"]]"))
      
      out <- c(out, "```\n\n")
      
      out <- c(out, "# Enrichment Analysis - GSEA \n\n ")
      out <- c(out, "* Collection: `r collection`")
      
      ## table
      out <- c(out,"```{r echo = FALSE, warning=FALSE, message = FALSE}")
      out <- c(out,paste0('DT::datatable(gsea$report_save$plot$report$data[[\"',input$gsea_list[i],'\"]], rownames=FALSE,
                                   selection = "single", options = list(scrollX = TRUE, scrollY = "500px", lengthMenu = c(20, 50, 100)))\n'))
      out <- c(out,"```{r echo = FALSE, warning=FALSE, message = FALSE}")
      out <- c(out, paste0('gsea$report_save$plot$report$plot[[\"',input$gsea_list[i],'\"]]\n\n'))
      out <- c(out, "```\n\n")
      
      ## heatmap
      out <- c(out, "### Heatmap ")
      out <- c(out, "* Row-distance: `r heatmap_rowdist_gsea`")
      out <- c(out, "* Column-distance: `r heatmap_coldist_gsea`")
      out <- c(out, "* Clustering function: `r hclustfun_gsea`")
      out <- c(out, "* Dendrogram: `r dendrogram_gsea`")
      out <- c(out,"\n\n```{r echo = FALSE, warning=FALSE, message = FALSE}")
      out <- c(out, paste0('gsea$report_save$plot$report$heatmap[[\"',input$gsea_list[i],'\"]]\n\n'))
      out <- c(out, "```\n\n")
      #  }
      rmd <- paste(out, collapse = "\n")
    }
  })
  
  
  
  # update editor ----
  # get all input values and display rmd code in editor
  
  observe({
    if (!is.null(input$getRmd)){
      if (input$getRmd!=0){
        rmd <- c(sp_rmd(), fs_rmd(), pca_rmd(), de_rmd(), set_rmd())
        rmd <- paste(rmd, collapse = "\n")
        isolate({shinyAce::updateAceEditor(session,"rmd_code",value = rmd)})
      }
    }
  })
  
  
  
  
  
  # Preview module
  output$reportPreview <- renderUI({
    if (input$evalRmd!=0){ # update preview
      error_I <- 0
      withProgress(message = 'Processing', value = 0, {
        isolate({
          fileName <- paste0("/tmp/tmp_",format(Sys.time(), "%Y%m%d%H%M%S"),".rmd") 
          fileConn<-file(fileName)  # creates file if doesn't exist
          tmp_content <- paste(rmd_static_codes(),input$rmd_code,sep = "\n")
          writeLines(tmp_content, fileConn)
          close(fileConn)
          filePreview <- paste0("tmp/tmp_preview_",format(Sys.time(), "%Y%m%d%H%M%S"),".html")
          incProgress(0.5, detail = "Synthesizing report...")
          tryCatch({
            rmarkdown::render(input = fileName, output_format = "html_document", output_file = paste0("/",filePreview)) #},
            error = function(e) {
              error_I <<- 1
            }
          })
          setProgress(1)
        })
        if(error_I) return("Failed to generate your report. Please make sure all specified modules have been run at least once and try again.")
        html_text = paste0("<iframe src='",filePreview,"', width='100%', height='800'></iframe>")
        return(HTML(html_text))  
      })
    }
  })
  
  
  observeEvent(input$clearRmd, {
    isolate({
      shinyAce::updateAceEditor(session,"rmd_code",value = " ")
      #updateSelectInput(
      #    session, ns('mapping_list'), choices = names(mapping$report_save$plot$report), 
      #    selected = NULL
      #)
    })
  }) 
  
  
  
  callModule(rmdDownload, "rmd_export",  rmd_static_codes, input$rmd_code, newsp, mapping, exprov, fs, pca, de, enrich, geneset)
  
  
}
