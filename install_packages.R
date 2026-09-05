# Install packages required by the public Shiny review app.
# Run once in the R console, then start the app with shiny::runApp().

required_packages <- c(
  "shiny",
  "bslib",
  "tidyverse",
  "readxl",
  "DT",
  "wordcloud2"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  install.packages(missing_packages)
}

message("Ready. Run shiny::runApp() from the repository folder.")
