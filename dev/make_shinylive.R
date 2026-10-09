## Builds a browser-only (Shinylive / WebAssembly) copy of the app for static
## hosting such as GitHub Pages. Data loaded in that copy never leave the
## user's browser.
##
## Usage, from the package root:  Rscript dev/make_shinylive.R [output_dir]
## Preview:  httpuv::runStaticServer("_shinylive")
##
## The package is not available as a WebAssembly binary, so the app directory
## is assembled from the package sources: the R files are sourced by app.R and
## the example datasets are loaded from the .rda files.

out <- commandArgs(trailingOnly = TRUE)
out <- if (length(out) > 0) out[1] else "_shinylive"

app <- file.path(tempdir(), "pmxdchk-shinylive")
unlink(app, recursive = TRUE)
dir.create(file.path(app, "src"), recursive = TRUE)

for (f in list.files("R", full.names = TRUE)) {
  code <- readLines(f, warn = FALSE)
  # Sourced code has no package namespace to qualify against.
  code <- gsub("pmxdchk::", "", code, fixed = TRUE)
  writeLines(code, file.path(app, "src", basename(f)))
}
file.copy(list.files("data", "\\.rda$", full.names = TRUE), app)

writeLines(c(
  "library(shiny)",
  "library(bslib)",
  "library(DT)",
  "library(ggplot2)",
  "library(dplyr)",
  "library(readr)",
  "library(purrr)",
  "library(tibble)",
  "library(rlang)",
  "for (f in list.files('src', full.names = TRUE)) source(f, local = TRUE)",
  "for (f in list.files('.', '\\\\.rda$')) load(f, envir = environment())",
  "register_builtin_checks()",
  "golem_add_external_resources <- function() NULL",
  "ui <- function(request) {",
  "  tagList(",
  "    div(",
  "      class = 'alert alert-info m-3 mb-0',",
  "      'Browser demo: the app runs entirely in your browser. Data you load ',",
  "      'are not uploaded to any server.'",
  "    ),",
  "    app_ui(request)",
  "  )",
  "}",
  "shinyApp(ui, app_server)"
), file.path(app, "app.R"))

shinylive::export(app, out)
