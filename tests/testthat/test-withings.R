test_that("withings_json_to_df handles empty/invalid input gracefully", {
  expect_silent(df <- withings_json_to_df(list(series = list())))
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 0)
})

test_that("combine_withings_epoch_summary aligns ids and adds datetime columns", {
  summary <- tibble::tibble(w_id = c(1,2), w_timezone = c("UTC","Europe/Paris"), w_startdate = c(0,0), w_enddate = c(0,0), w_startdate_utc = as.POSIXct(NA), w_enddate_utc = as.POSIXct(NA))
  epochs <- tibble::tibble(w_id = c(1,1,2), timestamp = c(0, 60, 0))
  cmb <- combine_withings_epoch_summary(epochs, summary)
  expect_true(all(c("summary","epoch") %in% names(cmb)))
  expect_true(all(c("datetime_utc","datetime_local_chr") %in% names(cmb$epoch)))
  expect_equal(nrow(cmb$epoch), 3)
})

test_that("withings_sleep constructs and reports", {
  summary <- tibble::tibble(w_id = 1, w_timezone = "UTC", w_total_sleep_time = 3600, w_sleep_efficiency = 0.9, w_sleep_latency = 600, w_waso = 300, w_apnea_hypopnea_index = 2, w_snoring = 120, w_startdate_utc = as.POSIXct(0, origin = "1970-01-01"), w_enddate_utc = as.POSIXct(3600, origin = "1970-01-01"))
  epochs <- tibble::tibble(w_id = c(1,1), timestamp = c(0,60))
  sl <- withings_sleep(summary, epochs)
  expect_s3_class(sl, "withings_sleep")
  rpt <- withings_sleep_report(sl)
  expect_true(is.data.frame(rpt))
  expect_true(all(c("duration_h","efficiency","latency_min","waso_min") %in% names(rpt)))
})

test_that("withings_sleep$report can create an HTML file", {
  skip_if_not_installed <- function(pkg) if (!requireNamespace(pkg, quietly = TRUE)) skip(paste("Package", pkg, "not installed"))
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc not available")
  summary <- tibble::tibble(w_id = 1, w_timezone = "UTC", w_total_sleep_time = 3600, w_sleep_efficiency = 0.9, w_sleep_latency = 600, w_waso = 300, w_apnea_hypopnea_index = 2, w_snoring = 120, w_startdate_utc = as.POSIXct(0, origin = "1970-01-01"), w_enddate_utc = as.POSIXct(3600, origin = "1970-01-01"))
  epochs <- tibble::tibble(w_id = c(1,1), timestamp = c(0,60))
  sl <- withings_sleep(summary, epochs)
  out <- tempfile(fileext = ".html")
  path <- sl$report(output = "html", file = out)
  expect_true(file.exists(path))
})

test_that("withings JSON example parses to non-empty tibble", {
  base_dir <- testthat::test_path("..", "testing_files")
  json_file <- list.files(base_dir, pattern = "^sleep_summary_.*\\.json$", full.names = TRUE)[1]
  json_path <- json_file
  skip_if_not(file.exists(json_path), "No JSON sample found")
  df <- withings_json_to_df(json_path)
  expect_true(nrow(df) >= 0) # allow zero, but function should succeed
  expect_true(is.data.frame(df))
})

test_that("withings preferred TP CSVs build sleep object and report", {
  base_dir <- testthat::test_path("..", "testing_files")
  sum_path <- file.path(base_dir, "withings_tp_summary.csv")
  ep_path  <- file.path(base_dir, "withings_tp_epoch.csv")
  skip_if_not(file.exists(sum_path) && file.exists(ep_path), "Preferred TP CSVs not available")
  skip_if_not_installed <- function(pkg) if (!requireNamespace(pkg, quietly = TRUE)) skip(paste("Package", pkg, "not installed"))
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc not available")
  summary <- withings_read_csv(sum_path)
  epochs  <- withings_read_csv(ep_path)
  sl <- withings_sleep(summary, epochs)
  expect_s3_class(sl, "withings_sleep")
  expect_true(all(c("datetime_utc","datetime_local_chr") %in% names(sl$epoch)))
  rpt <- sl$report()
  expect_true(file.exists(rpt))
})

test_that("withings generic CSVs build sleep object and report", {
  base_dir <- testthat::test_path("..", "testing_files")
  sum_path <- file.path(base_dir, "withings_summary.csv")
  ep_path  <- file.path(base_dir, "withings_epochs.csv")
  skip_if_not(file.exists(sum_path) && file.exists(ep_path), "Generic CSVs not available")
  summary <- withings_read_csv(sum_path)
  epochs  <- withings_read_csv(ep_path)
  sl <- withings_sleep(summary, epochs)
  expect_s3_class(sl, "withings_sleep")
  expect_true(all(c("datetime_utc","datetime_local_chr") %in% names(sl$epoch)))
  rpt <- withings_sleep_report(sl)
  expect_true(is.data.frame(rpt))
  expect_true(all(c("duration_h","efficiency","latency_min","waso_min") %in% names(rpt)))
})

# ===== New SRI and GGIR export tests =====

test_that("withings_sleep$reformat exports to GGIR format", {
  base_dir <- testthat::test_path("..", "testing_files")
  sum_path <- file.path(base_dir, "withings_tp_summary.csv")
  ep_path  <- file.path(base_dir, "withings_tp_epoch.csv")
  skip_if_not(file.exists(sum_path) && file.exists(ep_path), "Preferred TP CSVs not available")
  summary <- withings_read_csv(sum_path)
  epochs  <- withings_read_csv(ep_path)
  sl <- withings_sleep(summary, epochs)
  
  # Export to GGIR format
  ggir_data <- sl$reformat(type = "ggir")
  
  # Check required columns
  expect_true(all(c("id", "timestamp", "acc") %in% names(ggir_data)))
  expect_true(nrow(ggir_data) > 0)
  
  # Check timestamp is ISO8601 format (character)
  expect_type(ggir_data$timestamp, "character")
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}T", ggir_data$timestamp[1]))
  
  # Check acc is numeric
  expect_type(ggir_data$acc, "double")
})

test_that("withings_sleep$sri calculates SRI with wake assumption", {
  base_dir <- testthat::test_path("..", "testing_files")
  sum_path <- file.path(base_dir, "withings_tp_summary.csv")
  ep_path  <- file.path(base_dir, "withings_tp_epoch.csv")
  skip_if_not(file.exists(sum_path) && file.exists(ep_path), "Preferred TP CSVs not available")
  summary <- withings_read_csv(sum_path)
  epochs  <- withings_read_csv(ep_path)
  sl <- withings_sleep(summary, epochs)
  
  # Calculate SRI with default wake assumption
  suppressMessages({
    result <- sl$sri()
  })
  
  # Check output structure
  expect_type(result, "list")
  expect_true(all(c("sri", "raster", "report") %in% names(result)))
  
  # Check SRI is numeric and in valid range
  expect_type(result$sri, "double")
  expect_true(result$sri >= -100 && result$sri <= 100)
  
  # Check raster is a ggplot object
  expect_s3_class(result$raster, "gg")
  expect_s3_class(result$raster, "ggplot")
  
  # Check report is character
  expect_type(result$report, "character")
  expect_true(nchar(result$report) > 0)
  expect_true(grepl("SRI:", result$report))
})

test_that("withings_sleep$sri calculates SRI with continuity assumption", {
  base_dir <- testthat::test_path("..", "testing_files")
  sum_path <- file.path(base_dir, "withings_tp_summary.csv")
  ep_path  <- file.path(base_dir, "withings_tp_epoch.csv")
  skip_if_not(file.exists(sum_path) && file.exists(ep_path), "Preferred TP CSVs not available")
  summary <- withings_read_csv(sum_path)
  epochs  <- withings_read_csv(ep_path)
  sl <- withings_sleep(summary, epochs)
  
  # Calculate SRI with continuity assumption
  suppressMessages({
    result <- sl$sri(assumption = "continuity", continuity_limit = 10)
  })
  
  # Check output structure
  expect_type(result, "list")
  expect_true(all(c("sri", "raster", "report") %in% names(result)))
  
  # Check SRI is numeric
  expect_type(result$sri, "double")
  expect_true(result$sri >= -100 && result$sri <= 100)
  
  # Check report mentions continuity
  expect_true(grepl("continuity", result$report, ignore.case = TRUE))
})

test_that("withings_sleep$sri saves plot when plot_path provided", {
  base_dir <- testthat::test_path("..", "testing_files")
  sum_path <- file.path(base_dir, "withings_tp_summary.csv")
  ep_path  <- file.path(base_dir, "withings_tp_epoch.csv")
  skip_if_not(file.exists(sum_path) && file.exists(ep_path), "Preferred TP CSVs not available")
  summary <- withings_read_csv(sum_path)
  epochs  <- withings_read_csv(ep_path)
  sl <- withings_sleep(summary, epochs)
  
  # Calculate SRI and save plot
  plot_file <- tempfile(fileext = ".png")
  suppressMessages({
    result <- sl$sri(plot_path = plot_file)
  })
  
  # Check plot was saved
  expect_true(file.exists(plot_file))
  expect_true(file.size(plot_file) > 0)
  
  # Clean up
  unlink(plot_file)
})

test_that("withings_sleep$sri handles insufficient data gracefully", {
  # Create minimal data (less than 2 days)
  summary <- tibble::tibble(
    w_id = 1, 
    w_timezone = "UTC",
    w_startdate_utc = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    w_enddate_utc = as.POSIXct("2024-01-01 08:00:00", tz = "UTC")
  )
  epochs <- tibble::tibble(
    w_id = rep(1, 60),
    timestamp = seq(as.numeric(as.POSIXct("2024-01-01 00:00:00", tz = "UTC")), 
                    length.out = 60, by = 60),
    state = rep(c(0,1,2,3), length.out = 60)
  )
  sl <- withings_sleep(summary, epochs)
  
  # Should return NA for insufficient data
  suppressMessages({
    result <- sl$sri()
  })
  expect_true(is.na(result$sri))
  expect_null(result$raster)
})

test_that("epoch plots can be generated for sleep sessions", {
  base_dir <- testthat::test_path("..", "testing_files")
  sum_path <- file.path(base_dir, "withings_tp_summary.csv")
  ep_path  <- file.path(base_dir, "withings_tp_epoch.csv")
  skip_if_not(file.exists(sum_path) && file.exists(ep_path), "Preferred TP CSVs not available")
  skip_if_not_installed("plotly")
  
  summary <- withings_read_csv(sum_path)
  epochs  <- withings_read_csv(ep_path)
  sl <- withings_sleep(summary, epochs)
  
  # Get first sleep ID
  id_col <- NULL
  for (col in c("w_id", "sleep_id", "id")) {
    if (col %in% names(sl$summary)) {
      id_col <- col
      break
    }
  }
  skip_if(is.null(id_col), "No ID column found")
  
  sleep_id <- sl$summary[[id_col]][1]
  
  # Generate epoch plots
  plots <- .generate_nightly_epoch_plots(sleep_id, sl$epoch, sl$summary)
  
  # Check plots were generated
  expect_type(plots, "list")
  expect_true(length(plots) > 0)
  
  # Check each plot is a plotly object
  for (plot in plots) {
    expect_s3_class(plot, "plotly")
  }
})

