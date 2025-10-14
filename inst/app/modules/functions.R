# from ComplexHeatmap package : https://rdrr.io/github/jokergoo/ComplexHeatmap/src/R/Upset.R
list_to_matrix <- function(lt, universal_set = NULL) {
  if (!is.null(universal_set)) {
    lt <- lapply(lt, function(x) intersect(x, universal_set))
  } else {
    universal_set <- unique(unlist(lt))
  }

  mat <- matrix(0, nrow = length(universal_set), ncol = length(lt))
  rownames(mat) <- sort(universal_set)
  colnames(mat) <- names(lt)
  for (i in seq_along(lt)) {
    mat[as.character(unique(lt[[i]])), i] <- 1
  }
  return(mat)
}

# Intersection plots (Venn and Upset)
# dtset: list of DE results (de$de_data_save$data[[input$feature_type]][[dtsetname]]$res)
# dtset.names: vector of selected datasets to intersect
setdata <- function(filtered_data, dtsets, regulated) {
  genes <- lapply(dtsets, function(i) {
    g <- filtered_data[[i]][[regulated]]
    return(g)
  })
  names(genes) <- dtsets
  if (length(dtsets) == 2) {
    genelists <- list(
      g1 = genes[[1]],
      g2 = genes[[2]],
      g1g2 = intersect(genes[[1]], genes[[2]])
    )
  } else if (length(dtsets) == 3) {
    genelists <- list(
      g1 = genes[[1]],
      g2 = genes[[2]],
      g3 = genes[[3]],
      g1g2 = intersect(genes[[1]], genes[[2]]),
      g1g3 = intersect(genes[[1]], genes[[3]]),
      g2g3 = intersect(genes[[2]], genes[[3]]),
      g1g2g3 = intersect(intersect(genes[[1]], genes[[2]]), genes[[3]])
    )
  } else if (length(dtsets) > 3) {
    gm <- list_to_matrix(genes)
    groups <- colnames(gm)
    gm <- as_tibble(apply(gm, 2, as.logical))
    genelists <- list(
      gm = gm,
      groups = groups
    )
  }
  return(genelists)
}

setplot <- function(genelists, dtsets) {
  if (length(dtsets) == 2) {
    splot <- draw.pairwise.venn(
      area1 = length(genelists$g1),
      area2 = length(genelists$g2),
      cross.area = length(genelists$g1g2),
      fill = c("blue", "red"),
      category = dtsets,
      cat.pos = c(200, 40),
      ind = FALSE
    ) # unless this is set to false grid.draw will throw error: Error in <Anonymous>: cannot open file 'Rplots.pdf'
  } else if (length(dtsets) == 3) {
    splot <- draw.triple.venn(
      area1 = length(genelists$g1),
      area2 = length(genelists$g2),
      area3 = length(genelists$g3),
      n12 = length(genelists$g1g2),
      n13 = length(genelists$g1g3),
      n23 = length(genelists$g2g3),
      n123 = length(genelists$g1g2g3),
      fill = c("blue", "red", "green"),
      category = dtsets,
      cat.just = list(c(0, 0), c(0, 0), c(0, 0)),
      ind = FALSE
    ) # unless this is set to false grid.draw will throw error: Error in <Anonymous>: cannot open file 'Rplots.pdf'
  } else if (length(dtsets) > 3) {
    splot <- upset(genelists$gm, genelists$groups, name = "Intersection of gene lists")
  }
  return(splot)
}

settable <- function(genelists, dtsets) {
  gInt <- list()
  if (length(dtsets) == 2) {
    gInt[["Intersection"]] <- genelists$g1g2
    gInt[[paste(dtsets[1], "_only")]] <- genelists$g1[-which(genelists$g1 %in% genelists$g2)]
    gInt[[paste(dtsets[2], "_only")]] <- genelists$g2[-which(genelists$g2 %in% genelists$g1)]
  } else if (length(dtsets) == 3) {
    gInt[["Intersection_all"]] <- genelists$g1g2g3
    gInt[[paste0(dtsets[1], "_only")]] <- genelists$g1[-which(genelists$g1 %in% c(genelists$g2, genelists$g3))]
    gInt[[paste0(dtsets[2], "_only")]] <- genelists$g2[-which(genelists$g2 %in% c(genelists$g1, genelists$g3))]
    gInt[[paste0(dtsets[3], "_only")]] <- genelists$g3[-which(genelists$g3 %in% c(genelists$g1, genelists$g2))]
    gInt[[paste0("Int_", dtsets[1], "_", dtsets[2])]] <- genelists$g1g2
    gInt[[paste0("Int_", dtsets[1], "_", dtsets[3])]] <- genelists$g1g3
    gInt[[paste0("Int_", dtsets[2], "_", dtsets[3])]] <- genelists$g2g3
  } else {
    gInt <- genelists$gm
  }
  return(gInt)
}

# Returns TRUE if a status is valid; throws error otherwise.
validateStatusPlus <- function(status) {
  if (status %in% validStatusesPlus) {
    return(TRUE)
  }

  stop(
    "Invalid status: ", status, ". Valid statuses are: ",
    paste(validStatusesPlus, collapse = ", "), "."
  )
}

validStatusesPlus <- c(
  "primary", "success", "info", "warning", "danger",
  "navy", "teal", "purple", "orange", "maroon", "black"
)

# Returns TRUE if a color is a valid color defined in AdminLTE, throws error
# otherwise.
validateColor <- function(color) {
  if (color %in% validColors) {
    return(TRUE)
  }
  stop(
    "Invalid color: ", color, ". Valid colors are: ",
    paste(validColors, collapse = ", "), "."
  )
}

validColors <- c(
  "red", "yellow", "aqua", "blue", "light-blue", "green",
  "navy", "teal", "olive", "lime", "orange", "fuchsia",
  "purple", "maroon", "black"
)

"%OR%" <- function(a, b) if (!is.null(a)) a else b

boxPlusCustom <- function(..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE,
                          background = NULL, width = 6, height = NULL, collapsible = FALSE,
                          collapsed = FALSE, closable = TRUE, enable_label = FALSE,
                          label_text = NULL, label_status = "primary", enable_dropdown = FALSE,
                          dropdown_icon = "wrench", dropdown_menu = NULL, enable_sidebar = FALSE,
                          sidebar_content = NULL, sidebar_width = 25, sidebar_background = "#222d32",
                          sidebar_start_open = FALSE, footer_padding = TRUE, maximizable = FALSE) {
  if (sidebar_width < 0 || sidebar_width > 100) {
    stop("The box sidebar should be between 0 and 100")
  }
  boxClass <- "card"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "card-solid")
  }
  if (!is.null(status)) {
    validateStatusPlus(status)
    boxClass <- paste0(boxClass, " card-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-card")
  }
  if (!is.null(background)) {
    validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  if (enable_sidebar) {
    if (sidebar_start_open) {
      boxClass <- paste0(boxClass, " direct-chat direct-chat-contacts-open")
    }
    else {
      boxClass <- paste0(boxClass, " direct-chat")
    }
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- shiny::tags$h3(class = "card-title", title)
  }
  boxToolTag <- NULL
  if (collapsible || closable) {
    boxToolTag <- shiny::tags$div(class = "card-tools pull-right")
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) {
      "plus"
    } else {
      "minus"
    }
    collapseTag <- shiny::tags$button(
      class = paste0("btn btn-tool"),
      `data-card-widget` = "collapse", shiny::icon(collapseIcon)
    )
  }
  maximizeTag <- NULL
  if (maximizable) {
    maximizeTag <- shiny::tags$button(
      class = "btn btn-tool",
      `data-card-widget` = "maximize", type = "button", shiny::tags$i(class = "fa fa-expand")
    )
  }
  closableTag <- NULL
  if (closable) {
    closableTag <- shiny::tags$button(
      class = "btn btn-tool",
      `data-card-widget` = "remove", type = "button", shiny::tags$i(shiny::icon("times"))
    )
  }
  labelTag <- NULL
  if (enable_label) {
    labelTag <- dashboardLabel(label_text, status = label_status)
  }
  dropdownTag <- NULL
  if (enable_dropdown) {
    dropdownTag <- shiny::tags$div(class = "btn-group", shiny::tags$button(
      type = "button",
      class = "btn btn-tool dropdown-toggle", `data-toggle` = "dropdown",
      shiny::icon(dropdown_icon)
    ), shiny::tagList(dropdown_menu))
  }
  sidebarTag <- NULL
  if (enable_sidebar) {
    sidebarTag <- shiny::tags$button(
      class = "btn btn-tool",
      `data-card-widget` = "chat-pane-toggle", `data-toggle` = "tooltip",
      `data-original-title` = "More", title = NA, type = "button",
      shiny::icon("info")
    )
  }
  boxToolTag <- shiny::tagAppendChildren(
    boxToolTag, labelTag,
    dropdownTag, sidebarTag, collapseTag, closableTag, maximizeTag
  )
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- shiny::tags$div(
      class = "card-header", titleTag,
      boxToolTag
    )
  }
  boxPlusTag <- shiny::tags$div(class = if (!is.null(width)) {
    paste0("col-sm-", width)
  }, shiny::tags$div(
    class = boxClass,
    style = if (!is.null(style)) {
      style
    }, headerTag, shiny::tags$div(
      class = "card-body",
      ..., if (enable_sidebar) {
        shiny::tags$div(
          style = "z-index: 10000;", class = "direct-chat-contacts",
          shiny::tags$ul(class = "contacts-list", shiny::tags$li(style = paste0(
            "width: ",
            sidebar_width, "%;"
          ), sidebar_content))
        )
      }
    ), if (!is.null(footer)) {
      shiny::tags$div(class = if (isTRUE(footer_padding)) {
        "card-footer"
      } else {
        "box-footer no-padding"
      }, footer)
    }
  ))
  translation_rate <- paste0(100 - sidebar_width, "%")
  shiny::tagList(
    shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML(paste0(
      ".direct-chat-contacts {\n                 -webkit-transform: translate(100%, 0);\n                 -ms-transform: translate(100%, 0);\n                 -o-transform: translate(100%, 0);\n                 transform: translate(100%, 0);\n                 position: absolute;\n                 top: 0;\n                 bottom: 0;\n                 height: 100%;\n                 width: 100%;\n                 background: ",
      sidebar_background, ";\n                 color: #fff;\n                 overflow: auto;\n              }\n              .direct-chat-contacts-open .direct-chat-contacts {\n                -webkit-transform: translate(",
      translation_rate, ", 0);\n                -ms-transform: translate(",
      translation_rate, ", 0);\n                -o-transform: translate(",
      translation_rate, ", 0);\n                transform: translate(",
      translation_rate, ", 0);\n              }\n              "
    ))))),
    boxPlusTag
  )
}
