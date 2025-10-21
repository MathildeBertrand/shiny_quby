# --- LIBRAIRIES ---
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("ggiraph", quietly = TRUE)) install.packages("ggiraph")

library(shiny)
library(ggiraph)

# --- PATCH ICONES ---
options(shiny.icons.fontawesome = "v4")
old_icon_map <- list(
  "floppy-o"    = "floppy-disk",
  "file-image-o"= "file-image",
  "file-text-o" = "file-lines",
  "hand-o-up"   = "hand-pointer"
)
icon <- function(name, ...) {
  if (name %in% names(old_icon_map)) {
    name <- old_icon_map[[name]]
  }
  shiny::icon(name, ...)
}

# --- PATCH GGIRAPH ---
ggiraphOutput  <- function(outputId, width = "100%", height = "400px") {
  girafeOutput(outputId, width = width, height = height)
}
renderGgiraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  renderGirafe(expr, env = env, quoted = quoted)
}
renderggiraph <- renderGgiraph

# --- RESET DEVICES ---
while (!is.null(dev.list())) dev.off()

# --- RÉGLAGES GRAPHIQUES ---
options(
  shiny.usecairo = TRUE,
  shiny.plotType = "ragg_png",
  bitmapType = "cairo",
  shiny.launch.browser = TRUE, # Ouvre dans un navigateur complet
  shiny.sanitize.errors = FALSE
)

# --- PACKAGES DE RENDU ---
suppressMessages({
  if (!requireNamespace("Cairo", quietly = TRUE)) install.packages("Cairo")
  if (!requireNamespace("ragg", quietly = TRUE)) install.packages("ragg")
  library(Cairo)
  library(ragg)
})

# Bug connu : ggiraph disparaît sur certaines versions de Chrome/RStudio -> forcer redraw
options(ggiraph.maxsvg = 10e6)

# --- DÉTECTION AUTOMATIQUE DU CHEMIN D’INSTALLATION DE L’APPLI ---
# On cherche le dossier "app" dans l’arborescence du package QubyRSQ
pkg_path <- system.file("app", package = "QubyRSQ")

if (pkg_path == "") {
  stop("Impossible de trouver le dossier 'app' du package QubyRSQ")
}

# --- LANCEMENT DE L’APPLICATION ---
shiny::runApp(pkg_path, display.mode = "normal")
