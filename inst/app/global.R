## shiny/bs/js
library(shiny)
library(shinyWidgets)
library(shinyhelper)
library(shinycssloaders)
library(shinydashboardPlus)
library(shinyBS)
library(shinydashboard)
library(bsplus)
library(markdown)
library(rintrojs)
## format
#library(tidyverse)
library(dplyr)
library(tibble)
library(stringr)
library(reshape)
library(DT)
library(data.table)
library(scales)
library(openxlsx)
## visualisations
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(ggiraph)
library(ComplexUpset)
library(pheatmap)
library(heatmaply)
library(VennDiagram)
## analysis
library(FactoMineR)
library(edgeR)
library(DESeq2)
library(clusterProfiler)
library(msigdbr)
library(ggvenn)
library(UpSetR)

# source codes
source("modules/SPmodule.R")
source("modules/modules.R")
source("modules/PCAmodule.R")
source("modules/eomodule.R")
source("modules/FSmodule.R")
source("modules/DEmodule.R")
source("modules/SETmodule.R")
source("modules/moduleGSEA.R")
source("modules/reportmodule.R")
source("modules/functions.R")

# add tmp folder for reporting
os <- Sys.info()["sysname"]
tmp_dir <- tempdir()

if (os == "Windows") {
  split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
  newtemp <- rev(split_path(tempdir()))
  newtemp <- newtemp[1:length(newtemp)-1]
  addResourcePath("tmp", paste(newtemp, collapse = "/"))
} else {
  addResourcePath("tmp","/tmp")
}

#addResourcePath("tmp","/tmp") # allows /tmp folder to be used , by default only www is used as source folder

# db_choices
db_choices <- list(
  "H: hallmark gene sets" = "H",
  "C1: positional gene sets" = "C1",
  "C2: curated gene sets" = "C2",
  "   CGP: chemical and genetic perturbations" = "C2.CGP",
  "   CP: Canonical pathways" = "C2.CP",
  "   CP:BIOCARTA: BioCarta gene sets" = "C2.CP:BIOCARTA",
  "   CP:KEGG_MEDICUS: KEGG gene sets" = "C2.CP:KEGG_MEDICUS",
  "   CP:PID: Pathway Interaction Database" = "C2.CP:PID",
  "   CP:REACTOME: Reactome gene sets" = "C2.CP:REACTOME",
  "   CP:WIKIPATHWAYS: WikiPathways" = "C2.CP:WIKIPATHWAYS",
  "C3: motif gene sets" = "C3",
  "   MIR:MIRDB: microRNA targets" = "C3.MIR:MIRDB",
  "   MIR:MIR_Legacy" = "C3.MIR:MIR_Legacy",
  "   TFT:GTRD: transcription factor targets" = "C3.TFT:GTRD",
  "   TFT:TFT_Legacy" = "C3.TFT:TFT_Legacy",
  "C4: computational gene sets" = "C4",
  "   CGN: cancer gene neighborhoods" = "C4.CGN",
  "   CM: cancer modules" = "C4.CM",
  "C5: GO gene sets" = "C5",
  "   BP: GO biological process" = "C5.GO:BP",
  "   CC: GO cellular component" = "C5.GO:CC",
  "   MF: GO molecular function" = "C5.GO:MF",
  "   HPO: Human Phenotype Ontology" = "C5.HPO",
  "C6: oncogenic signatures" = "C6",
  "C7: immunologic signatures" = "C7",
  "C8: cell type signature gene sets" = "C8"
)

draw_colnames <- function(coln, ...) {
  m <- length(coln)
  x <- (1:m) / m - 1 / 2 / m
  grid::grid.text(coln, x = x, y = unit(0.96, "npc"), vjust = 0.5, hjust = 1, rot = 90, gp = grid::gpar(...))
}
assignInNamespace(x = "draw_colnames", value = draw_colnames, ns = "pheatmap")
