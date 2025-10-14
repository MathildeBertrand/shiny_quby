##########################################################################
############################## Plot + Table ensemble #####################
##########################################################################

moduleUI <- function(id, label = "Download") {
  ns <- NS(id)
  tagList(
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
          div(awesomeRadio(ns("table_extension"), label = "Download table as:", choices = list("csv", "tsv"), status = "warning", inline = TRUE), style = "display: inline-block;"),
          div(downloadButton(ns("downloadcsv"), "Download table"), style = "display: inline-block;"),
          p(),
          DT::dataTableOutput(outputId = ns("rendercsv"))
        )
      )
    )
  )
}

module <- function(input, output, session, datacsv, dataplot, filename = paste0("data_", Sys.Date()), filename1 = NULL, filename2 = NULL) {

  output$renderplot <- renderPlot({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering plot.", detail = "Please wait...", value = 1)
    print(dataplot())
  })

  output$downloadplot <- downloadHandler(
    filename = function() {
      if (!is.null(filename1) && !is.null(filename2)) {
        paste0(paste(filename, filename1(), filename2(), sep = "_"), ".", input$plot_extension)
      } else if (!is.null(filename1)) {
        paste0(paste(filename, filename1(), sep = "_"), ".", input$plot_extension)
      } else {
        paste0(filename, ".", input$plot_extension)
      }
    },
    content = function(file) {
      width <- 12
      height <- 8
      if (input$plot_extension == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
        print(dataplot())
        dev.off()
      } else {
        pdf(file, width = width, height = height)
        print(dataplot())
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

  output$downloadcsv <- downloadHandler(
    filename = function() {
      if (!is.null(filename1) && !is.null(filename2)) {
        paste0(paste(filename, filename1(), filename2(), sep = "_"), ".", input$table_extension)
      } else if (!is.null(filename1)) {
        paste0(paste(filename, filename1(), sep = "_"), ".", input$table_extension)
      } else {
        paste0(filename, ".", input$table_extension)
      }
    },
    content = function(file) {
      if (input$table_extension == "csv") {
        write.table(datacsv(), file = file, sep = ";", quote = FALSE, row.names = FALSE)
      } else {
        write.table(datacsv(), file = file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    }
  )
}


##########################################################################################################
############################## Plot + Table for FSmodule (additional input in Table) #####################
##########################################################################################################

moduleFSTableUI <- function(id, session, loadfs, label = "Download") {
  ns <- NS(id)

  unit <- reactive({
    loadfs()$fUnit
  })

  tagList(
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
          downloadButton(ns("downloadplot"), label),
          plotOutput(outputId = ns("renderplot"), width = "auto", height = "500px")
        ),
        tabPanel(
          "Data",
          icon = icon("file-text-o"),
          downloadButton(ns("downloadcsv"), label),
          p(),
          selectInput(
            ns("expression_features_select_DataTable"),
            label = "Select type of data:",
            choices = list("sample-based cpm and quantile rank" = 1, "group-based cpm summary statistic" = 2, "group-based quantile rank cpm summary statistic" = 3, "sample-based TPM and quantile rank" = 4, "group-based TPM summary statistic" = 5, "group-based quantile rank TPM summary statistic" = 6),
            selected = 1
          ),
          p(),
          DT::dataTableOutput(outputId = ns("rendercsv"))
        )
      )
    )
  )
}


moduleFSTable <- function(input, output, session, loadfs, dataplot, filename = paste0("data_", Sys.Date()), plottype, filetype, filename1 = NULL, filename2 = NULL, newsp) {
  
  output$renderplot <- renderPlot({
    print(dataplot())
  })

  datacsv <- reactive({
    i <- loadfs()
    validate(need(!is.null(i), ""))
    if (input$expression_features_select_DataTable == 1) {
      m <- data.frame(SampleID = newsp$sptable()$SampleID, "CPM.estimates" = c(i$L$fData), "Quantile.rank" = c(i$L$Quant))
    } else if (input$expression_features_select_DataTable == 4) {
      m <- data.frame(SampleID = newsp$sptable()$SampleID, "TPM.estimates" = c(i$L$fData), "Quantile.rank" = c(i$L$Quant))
    } else if (input$expression_features_select_DataTable == 2 | input$expression_features_select_DataTable == 4) {
      m <- data.frame(Item = rownames((m <- sapply(with(newsp$sptable(), split(SampleID, SampleGroup)), function(x) summary(i$L[["fData"]][x])))), m)
    } else {
      m <- data.frame(Item = rownames((m <- sapply(with(newsp$sptable(), split(SampleID, SampleGroup)), function(x) summary(i$L[["Quant"]][x])))), m)
    }
    return(m)
  })

  output$downloadplot <- downloadHandler(
    filename = function() {
      if (!is.null(filename1) && !is.null(filename2)) {
        paste0(paste(filename, filename1(), filename2(), sep = "_"), ".", plottype())
      } else if (!is.null(filename1)) {
        paste0(paste(filename, filename1(), sep = "_"), ".", plottype())
      } else {
        paste0(filename, ".", plottype())
      }
    },
    content = function(file) {
      width <- 12
      height <- 8
      if (plottype() == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
        print(dataplot())
        dev.off()
      } else {
        pdf(file, width = width, height = height)
        print(dataplot())
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
        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
        "}"
      )
    )
  )

  output$downloadcsv <- downloadHandler(
    filename = function() {
      if (!is.null(filename1) && !is.null(filename2)) {
        paste0(paste(filename, filename1(), filename2(), sep = "_"), ".", filetype())
      } else if (!is.null(filename1)) {
        paste0(paste(filename, filename1(), sep = "_"), ".", filetype())
      } else {
        paste0(filename, ".", filetype())
      }
    },
    content = function(file) {
      if (filetype() == "csv") {
        write.table(datacsv(), file = file, sep = ";", quote = FALSE, row.names = FALSE)
      } else {
        write.table(datacsv(), file = file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    }
  )
}

##########################################################################
############################## Plot only ################################
##########################################################################

plotDownloadUI <- function(id, label = "Download") {
  ns <- NS(id)
  downloadButton(ns("downloadplot"), label)
}


plotDownload <- function(input, output, session, dataplot, filename = paste0("data_", Sys.Date()), plottype) {

  output$downloadplot <- downloadHandler(
    filename = function() {
      paste0(filename, ".", plottype())
    },
    content = function(file) {
      width <- 12
      height <- 8
      if (plottype() == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
        print(dataplot())
        dev.off()
      } else {
        pdf(file, width = width, height = height)
        print(dataplot())
        dev.off()
      }
    }
  )
}
############################## Donwload PCA only ################################

plotDownloadPCAUI <- function(id, label = "Download PCA plot") {
  ns <- NS(id)
  downloadButton(ns("downloadplot"), label)
}

plotDownloadPCA <- function(input, output, session, dataplot, filename = paste0("PCA_plot_", Sys.Date()), plottype) {
  
  output$downloadplot <- downloadHandler(
    filename = function() {
      paste0(filename, ".", plottype())
    },
    content = function(file) {
      width <- 12
      height <- 8
      
      plot_obj <- dataplot()
      
      if (is.null(plot_obj) || !inherits(plot_obj, "gg")) {
        warning("No valid ggplot object to export.")
        file.create(file)  # Fichier vide, évite crash
        return()
      }
      
      if (plottype() == "png") {
        png(file, width = width, height = height, units = "in", res = 150)
        print(plot_obj)
        dev.off()
      } else {
        pdf(file, width = width, height = height)
        print(plot_obj)
        dev.off()
      }
    }
  )
}
##########################################################################
############################## File only ################################
##########################################################################

fileDownloadUI <- function(id, label = "Download") {
  ns <- NS(id)
  downloadButton(ns("downloadfile"), label)
}


fileDownload <- function(input, output, session, datacsv, filename = paste0("data_", Sys.Date()), filetype) {
  output$rendertable <- DT::renderDataTable(
    {
      datacsv()
    },
    rownames = FALSE,
    style = "bootstrap",
    class = "table-bordered",
    options = list(pagingType = "full", searching = FALSE, lengthMenu = list(c(10, 20, -1), list("10", "20", "All")), scrollX = TRUE, initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});", "}"))
  )

  output$downloadfile <- downloadHandler(
    filename = function() {
      paste0(filename, ".", filetype())
    },
    content = function(file) {
      if (filetype() == "csv") {
        write.table(datacsv(), file = file, sep = ";", quote = FALSE, row.names = FALSE)
      } else {
        write.table(datacsv(), file = file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    }
  )
}

##########################################################################
############################## Data table ################################
##########################################################################

tableRenderUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("ui")))
}


tableRender <- function(input, output, session, datacsv, filename = paste0("data_", Sys.Date())) {
  ns <- session$ns

  output$rendercsv <- DT::renderDataTable(
    {
      datacsv()
    },
    rownames = FALSE,
    style = "bootstrap",
    class = "table-bordered",
    options = list(
      pagingType = "full",
      searching = FALSE,
      lengthMenu = list(c(10, 20, -1), list("10", "20", "All")),
      scrollX = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
        "}"
      )
    )
  )

  output$downloadcsv <- downloadHandler(
    filename = function() {
      paste0(filename, ".csv")
    },
    content = function(file) {
      write.table(datacsv(), file = file, sep = ";", quote = FALSE, row.names = FALSE)
    }
  )

  output$ui <- renderUI(
    tagList(
      box(
        width = NULL,
        collapsible = TRUE,
        solidHeader = TRUE,
        status = "primary",
        title = div(icon("eye"), div(style = "display: inline-block; padding-left: 5px", "Data viewer")),
        downloadButton(ns("downloadcsv"), "Download"),
        p(),
        DT::dataTableOutput(outputId = ns("rendercsv"))
      )
    )
  )
}

##########################################################################
############################## Download RMD ##############################
##########################################################################

rmdDownloadUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns("saveRmd"), "Generate & Save", class = "btn btn-warning")
}

rmdDownload <- function(input, output, session, rmd_static_codes, report_rmd, newsp, mapping, exprov, fs, pca, de, enrich, gsea) {

  # credit: https://stackoverflow.com/questions/54277922/loading-message-for-slow-downloadhandler
  output$saveRmd <- downloadHandler(
    filename = function() {
      print("saving")
      paste0("report_", format(Sys.time(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      shiny::withProgress(
        message = "Generating the report",
        value = 0,
        {
          shiny::incProgress(1 / 10)
          Sys.sleep(1)
          shiny::incProgress(5 / 10)
          tmp_content <- paste0(rmd_static_codes(), report_rmd)
          print(tmp_content)
          error_I <- 0
          fileName <- paste0("/tmp/tmp_", format(Sys.time(), "%Y%m%d%H%M%S"), ".rmd")
          fileConn <- file(fileName)
          writeLines(tmp_content, fileConn)
          close(fileConn)
          rmarkdown::render(input = fileName, output_format = "html_document", output_file = file) # }
        }
      )
    }
  )
}
