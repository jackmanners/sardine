#' Render a Withings sleep report to HTML or PDF
#'
#' Generates a small report for a `withings_sleep` object using R Markdown.
#'
#' @param sleep A `withings_sleep` object (from [withings_sleep()]).
#' @param output_file Path to write the report (e.g., "report.html" or "report.pdf").
#' @param format One of "html" or "pdf". Defaults to "html".
#' @param title Document title. Defaults to "Withings Sleep Report".
#' @return The normalized path to the rendered file.
#' @export
withings_sleep_report_render <- function(sleep, output_file, format = c("html", "pdf"), title = "Withings Sleep Report") {
  format <- match.arg(format)
  if (!inherits(sleep, "withings_sleep")) stop("sleep must be a withings_sleep object")
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("The 'rmarkdown' package is required to render reports. Please install it: install.packages('rmarkdown')")
  }

  output_format <- if (format == "html") "html_document" else "pdf_document"

  # Comprehensive self-contained Rmd
  rmd <- c(
    "---",
    paste0("title: ", title),
    paste0("output:\n  ", output_format, ":\n    self_contained: true"),
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
    "library(sardine)",
    "library(plotly)",
    "library(jsonlite)",
    "# Source the epoch plots file - it's in the same directory as this is being loaded from",
    "epoch_plots_file <- system.file('R', 'withings-epoch-plots.R', package='sardine')",
    "if (file.exists(epoch_plots_file)) {",
    "  source(epoch_plots_file)",
    "} else {",
    "  # Development mode - source from package directory",
    "  pkg_root <- getwd()",
    "  while (!file.exists(file.path(pkg_root, 'DESCRIPTION')) && pkg_root != dirname(pkg_root)) {",
    "    pkg_root <- dirname(pkg_root)",
    "  }",
    "  epoch_plots_file <- file.path(pkg_root, 'R', 'withings-epoch-plots.R')",
    "  if (file.exists(epoch_plots_file)) source(epoch_plots_file)",
    "}",
    "```",
    "",
    "<style>",
    ".modal {",
    "  display: none;",
    "  position: fixed;",
    "  z-index: 1000;",
    "  left: 0;",
    "  top: 0;",
    "  width: 100%;",
    "  height: 100%;",
    "  overflow: auto;",
    "  background-color: rgba(0,0,0,0.6);",
    "}",
    ".modal-content {",
    "  background-color: #fefefe;",
    "  margin: 2% auto;",
    "  padding: 20px;",
    "  border: 1px solid #888;",
    "  width: 90%;",
    "  max-width: 1200px;",
    "  border-radius: 8px;",
    "}",
    ".modal-header {",
    "  display: flex;",
    "  justify-content: space-between;",
    "  align-items: center;",
    "  margin-bottom: 20px;",
    "}",
    ".close-btn {",
    "  color: #aaa;",
    "  font-size: 28px;",
    "  font-weight: bold;",
    "  cursor: pointer;",
    "  border: none;",
    "  background: none;",
    "}",
    ".close-btn:hover {",
    "  color: #000;",
    "}",
    "</style>",
    "",
    "## Summary Statistics",
    "",
    "### Duration, Efficiency & Regularity",
    "",
    "```{r}",
    "stats <- sardine:::.duration_efficiency_regularity(sleep$summary)",
    "if (!is.null(stats) && requireNamespace('knitr', quietly = TRUE)) knitr::kable(stats) else if (!is.null(stats)) print(stats) else cat('No summary data available')",
    "```",
    "",
    "### Sleep Ritual",
    "",
    "```{r}",
    "ritual <- sardine:::.sleep_ritual(sleep$summary)",
    "if (!is.null(ritual) && requireNamespace('knitr', quietly = TRUE)) knitr::kable(ritual) else if (!is.null(ritual)) print(ritual) else cat('No ritual data available')",
    "```",
    "",
    "### Sleep Vitals",
    "",
    "```{r}",
    "vitals <- sardine:::.sleep_vitals(sleep$summary)",
    "if (!is.null(vitals) && requireNamespace('knitr', quietly = TRUE)) knitr::kable(vitals) else if (!is.null(vitals)) print(vitals) else cat('No vitals data available')",
    "```",
    "",
    "## Nightly Sleep Timing",
    "",
    "```{r fig.height=8, out.width='100%'}",
    "timing_result <- sardine:::.render_horizontal_sleep_timing_plot(sleep$summary, sleep$epoch)",
    "if (!is.null(timing_result)) {",
    "  timing_result$plot",
    "} else {",
    "  cat('Not enough data to generate timing plot')",
    "}",
    "```",
    "",
    "### Nightly Metrics",
    "",
    "```{r}",
    "if (!is.null(timing_result) && !is.null(timing_result$metrics)) {",
    "  if (requireNamespace('knitr', quietly = TRUE)) {",
    "    knitr::kable(timing_result$metrics, align = 'c')",
    "  } else {",
    "    print(timing_result$metrics)",
    "  }",
    "} else {",
    "  cat('No metrics available')",
    "}",
    "```",
    "",
    "```{r generate-epoch-plots, include=FALSE}",
    "# Generate epoch plots for all sleep IDs",
    "epoch_plots_data <- list()",
    "if (!is.null(sleep$summary) && nrow(sleep$summary) > 0) {",
    "  for (i in seq_len(nrow(sleep$summary))) {",
    "    sleep_id <- sleep$summary$id[i]",
    "    plots <- sardine:::.generate_nightly_epoch_plots(sleep_id, sleep$epoch, sleep$summary)",
    "    if (!is.null(plots)) {",
    "      # Convert plotly objects to plotly JSON (list format)",
    "      epoch_plots_data[[as.character(sleep_id)]] <- lapply(plots, function(p) {",
    "        if (!is.null(p)) plotly::plotly_json(p, jsonedit = FALSE) else NULL",
    "      })",
    "    }",
    "  }",
    "}",
    "```",
    "",
    "```{r embed-epoch-plots, results='asis', echo=FALSE}",
    "# Embed epoch plots as JavaScript variable",
    "if (length(epoch_plots_data) > 0) {",
    "  plots_json <- jsonlite::toJSON(epoch_plots_data, auto_unbox = TRUE)",
    "  cat('<script>var epochPlots = ', plots_json, ';</script>', sep='')",
    "} else {",
    "  cat('<script>var epochPlots = {};</script>')",
    "}",
    "```",
    "",
    "```{r results='asis', echo=FALSE}",
    "cat('<div id=\"nightlyModal\" class=\"modal\"><div class=\"modal-content\"><div class=\"modal-header\"><h3 id=\"modalTitle\">Nightly Detail</h3><button class=\"close-btn\" onclick=\"closeModal()\">&times;</button></div><div id=\"modalBody\"><p>Click on a sleep bar.</p></div></div></div>')",
    "cat('<script>(function(){function closeModal(){var m=document.getElementById(\"nightlyModal\");if(m)m.style.display=\"none\";}function showNightlyDetail(id,date){var m=document.getElementById(\"nightlyModal\");var t=document.getElementById(\"modalTitle\");var b=document.getElementById(\"modalBody\");if(!m||!t||!b){console.error(\"Modal elements not found\");return;}t.textContent=\"Sleep: \"+date;if(typeof epochPlots===\"undefined\"||!epochPlots[id]){b.innerHTML=\"<p>No detailed data available for this night.</p>\";m.style.display=\"block\";return;}var plots=epochPlots[id];var html=\"\";var plotOrder=[\"hypnogram\",\"hr\",\"rr\",\"snoring\",\"movement\"];plotOrder.forEach(function(key,idx){if(plots[key]){html+=\"<div id=\\'plot_\"+idx+\"\\' style=\\'margin-bottom:20px\\'></div>\";}});b.innerHTML=html;plotOrder.forEach(function(key,idx){if(plots[key]){try{var plotData=JSON.parse(plots[key]);Plotly.newPlot(\"plot_\"+idx,plotData.data,plotData.layout,{responsive:true});}catch(e){console.error(\"Error rendering plot \"+key+\":\",e);}}});m.style.display=\"block\";}window.closeModal=closeModal;window.showNightlyDetail=showNightlyDetail;window.addEventListener(\"click\",function(e){var m=document.getElementById(\"nightlyModal\");if(m&&e.target===m)closeModal();});var att=0,max=100;var check=setInterval(function(){att++;var el=document.querySelector(\".plotly\");if(el&&el.on){clearInterval(check);el.on(\"plotly_click\",function(data){if(data.points&&data.points.length>0){var p=data.points[0];if(p.customdata){showNightlyDetail(p.customdata,p.y);}}});}else if(att>=max){clearInterval(check);}},100);})();</script>')",
    "```"
  )
  
  rmd_path <- tempfile(fileext = ".Rmd")
  writeLines(rmd, con = rmd_path, useBytes = TRUE)

  env <- new.env(parent = baseenv())
  env$sleep <- sleep

  # Ensure output_file is absolute path
  output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
  
  out <- rmarkdown::render(rmd_path, output_file = output_file, output_format = output_format, envir = env, quiet = TRUE)
  normalizePath(out, winslash = "/", mustWork = FALSE)
}
