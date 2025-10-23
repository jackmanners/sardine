#' @importFrom ggplot2 ggplot aes geom_point geom_vline scale_y_continuous scale_x_continuous scale_color_manual xlab ylab theme_classic theme element_text element_blank guides guide_legend ggsave
NULL

#' Create a Withings Sleep Analysis Object
#'
#' @description
#' Constructs a `withings_sleep` object from Withings sleep data for analysis,
#' reporting, and export. Can work with epoch data alone (for SRI calculation)
#' or with both summary and epoch data (for full reporting).
#'
#' @param summary Optional. A data frame or tibble containing sleep summary data
#'   with one row per sleep session. If NULL, only epoch-based analyses (like SRI)
#'   will be available. Expected columns include:
#'   \itemize{
#'     \item \strong{ID column}: One of `w_id`, `sleep_id`, or `id` to match with epoch data
#'     \item \strong{Timezone}: One of `w_timezone` or `timezone` (e.g., "America/New_York", "UTC")
#'     \item \strong{Sleep metrics} (optional): `w_total_sleep_time`, `w_sleep_efficiency`,
#'       `w_sleep_latency`, `w_waso`, `w_apnea_hypopnea_index`, `w_snoring`
#'     \item \strong{Timestamps}: `w_startdate_utc`, `w_enddate_utc` as POSIXct
#'   }
#' @param epoch A data frame or tibble containing minute-by-minute sleep epoch data.
#'   Required columns:
#'   \itemize{
#'     \item \strong{timestamp}: Unix timestamp (seconds since 1970-01-01 00:00:00 UTC)
#'     \item \strong{state}: Sleep state code (0 = awake, 1 = light sleep, 2 = deep sleep, 3 = REM)
#'     \item \strong{ID column}: One of `w_id`, `sleep_id`, or `id` to match with summary
#'   }
#'   Optional columns for enhanced analysis:
#'   \itemize{
#'     \item `hr`: Heart rate (beats per minute)
#'     \item `rr`: Respiratory rate (breaths per minute)
#'     \item `snoring`: Snoring intensity
#'     \item `mvt_score`: Movement score (used as acceleration proxy for GGIR)
#'   }
#' @param id_cols Character vector of candidate ID column names to try for matching
#'   summary and epoch data. Default: `c("w_id", "sleep_id", "id")`
#' @param tz_cols Character vector of candidate timezone column names in summary data.
#'   Default: `c("w_timezone", "timezone")`
#'
#' @return An S3 object of class `withings_sleep` with components:
#'   \describe{
#'     \item{summary}{Processed summary data (NULL if not provided)}
#'     \item{epoch}{Processed epoch data with added datetime columns}
#'     \item{$report()}{Method to generate HTML/PDF report (requires summary data)}
#'     \item{$reformat()}{Method to export data in various formats (e.g., GGIR)}
#'     \item{$sri()}{Method to calculate Sleep Regularity Index from epoch data}
#'   }
#'
#' @details
#' The function is flexible and can handle different input scenarios:
#' 
#' **Epoch-only mode** (summary = NULL):
#' - Only `$sri()` and `$reformat()` methods will work
#' - Useful when you only have raw epoch data and want to calculate sleep regularity
#' - `$report()` will fail with an informative error
#' 
#' **Full mode** (both summary and epoch):
#' - All methods available including interactive HTML reports
#' - Data are automatically combined and aligned by ID
#' - Datetime columns added to epoch data for plotting
#'
#' @examples
#' \dontrun{
#' # Load data from CSV files
#' summary <- read.csv("withings_summary.csv")
#' epoch <- read.csv("withings_epoch.csv")
#' 
#' # Full analysis with both summary and epoch
#' wsleep <- withings_sleep(summary, epoch)
#' wsleep$report(output = "html", file = "sleep_report.html")
#' wsleep$sri()
#' 
#' # Epoch-only analysis (e.g., for SRI calculation)
#' wsleep_minimal <- withings_sleep(summary = NULL, epoch = epoch)
#' sri_result <- wsleep_minimal$sri()
#' cat(sri_result$report)
#' 
#' # Export to GGIR format for external analysis
#' ggir_data <- wsleep$reformat(type = "ggir")
#' write.csv(ggir_data, "sleep_for_ggir.csv", row.names = FALSE)
#' }
#'
#' @seealso
#' \code{\link{withings_sleep_report}} for summary statistics,
#' \code{\link{reformat.withings_sleep}} for data export,
#' \code{\link{withings_sleep_report_render}} for HTML/PDF report generation
#'
#' @export
withings_sleep <- function(summary = NULL, epoch, id_cols = c("w_id","sleep_id","id"), tz_cols = c("w_timezone","timezone")) {
  # Handle epoch-only mode
  if (is.null(summary)) {
    obj <- list(summary = NULL, epoch = epoch)
    class(obj) <- "withings_sleep"
    obj$report <- function(...) {
      stop("Report generation requires summary data. Create withings_sleep object with summary argument.")
    }
    obj$reformat <- function(type = "ggir", ...) reformat.withings_sleep(obj, type = type, ...)
    obj$sri <- function(tz = NULL, plot_path = NULL, assumption = "wake", continuity_limit = NULL) {
      wsleep_sri(obj, tz = tz, plot_path = plot_path, assumption = assumption, continuity_limit = continuity_limit)
    }
    return(obj)
  }
  
  # Full mode: combine summary and epoch data
  combined <- combine_withings_epoch_summary(epoch, summary, id_cols = id_cols, tz_cols = tz_cols)
  
  obj <- list(summary = combined$summary, epoch = combined$epoch)
  class(obj) <- "withings_sleep"
  obj$report <- function(output = c("html", "pdf"), file = tempfile(fileext = paste0(".", match.arg(output))), ...) {
    withings_sleep_report_render(obj, output_file = file, format = match.arg(output), ...)
  }
  obj$reformat <- function(type = "ggir", ...) reformat.withings_sleep(obj, type = type, ...)
  obj$sri <- function(tz = NULL, plot_path = NULL, assumption = "wake", continuity_limit = NULL) {
    wsleep_sri(obj, tz = tz, plot_path = plot_path, assumption = assumption, continuity_limit = continuity_limit)
  }
  obj
}

#' Reformat Withings Sleep Data for External Use
#'
#' @description
#' Exports epoch-level sleep data from a `withings_sleep` object into formats
#' compatible with external analysis tools like GGIR.
#'
#' @param x A `withings_sleep` object created by \code{\link{withings_sleep}}
#' @param type Character string specifying output format. Currently supported:
#'   \describe{
#'     \item{"ggir"}{GGIR-compatible CSV format with columns: id, timestamp (ISO8601), acc}
#'   }
#' @param ... Additional arguments (currently unused, reserved for future formats)
#'
#' @return A data frame in the requested format:
#'   \describe{
#'     \item{GGIR format}{
#'       \itemize{
#'         \item \code{id}: Sleep session identifier
#'         \item \code{timestamp}: ISO8601 formatted timestamp (e.g., "2024-01-01T00:00:00Z")
#'         \item \code{acc}: Acceleration proxy (uses \code{mvt_score} from epoch data if available)
#'       }
#'     }
#'   }
#'
#' @details
#' **GGIR Format Details:**
#' 
#' The GGIR package expects externally-derived epoch data in a specific CSV format.
#' This function converts Withings epoch data to meet those requirements:
#' - Timestamps are converted from Unix timestamps to ISO8601 format in UTC
#' - Movement score (\code{mvt_score}) is used as a proxy for acceleration
#' - ID column is automatically detected (tries w_id, sleep_id, then id)
#' 
#' The resulting data can be used with GGIR's part 1 for sleep analysis and
#' Sleep Regularity Index (SRI) calculation.
#'
#' @examples
#' \dontrun{
#' # Create withings_sleep object
#' wsleep <- withings_sleep(summary, epoch)
#' 
#' # Export to GGIR format
#' ggir_data <- wsleep$reformat(type = "ggir")
#' 
#' # Save for use with GGIR
#' write.csv(ggir_data, "withings_for_ggir.csv", row.names = FALSE)
#' 
#' # Use with GGIR package
#' library(GGIR)
#' GGIR::g.part1(
#'   datadir = ".",
#'   outputdir = "output",
#'   f0 = 1, f1 = 1,
#'   overwrite = TRUE,
#'   externalsleepdata = "withings_for_ggir.csv"
#' )
#' }
#'
#' @seealso \code{\link{withings_sleep}} for creating the sleep object
#'
#' @export
reformat.withings_sleep <- function(x, type = c("ggir"), ...) {
  type <- match.arg(type)
  if (type == "ggir") {
    epoch_df <- x$epoch
    # Choose ID column
    id_col <- if ("w_id" %in% names(epoch_df)) "w_id"
      else if ("sleep_id" %in% names(epoch_df)) "sleep_id"
      else "id"
    # Convert timestamp to ISO8601
    epoch_df$timestamp_iso <- as.POSIXct(epoch_df$timestamp, origin = "1970-01-01", tz = "UTC")
    epoch_df$timestamp_iso <- format(epoch_df$timestamp_iso, "%Y-%m-%dT%H:%M:%SZ")
    # Use movement score as proxy for acc, or NA
    epoch_df$acc <- if ("mvt_score" %in% names(epoch_df)) epoch_df$mvt_score else NA
    # Select columns for GGIR
    ggir_df <- data.frame(
      id = epoch_df[[id_col]],
      timestamp = epoch_df$timestamp_iso,
      acc = epoch_df$acc
      # Add other columns if needed
    )
    return(ggir_df)
  }
  stop("Unknown format type: ", type)
}

#' Generate a concise report for a withings_sleep object
#'
#' @param x withings_sleep
#' @return A tibble summarising key metrics per record
#' @export
withings_sleep_report <- function(x) {
  if (!inherits(x, "withings_sleep")) stop("x must be a withings_sleep object")
  sm <- x$summary
  id_col <- intersect(names(sm), c("w_id","sleep_id","id"))[1]
  tz_col <- intersect(names(sm), c("w_timezone","timezone"))[1]
  if (is.na(id_col)) id_col <- names(sm)[1]
  # Build vectors safely even if columns are missing
  n <- nrow(sm)
  col_or_na <- function(df, col, type = c("dbl","chr","time")) {
    type <- match.arg(type)
    if (col %in% names(df)) return(df[[col]])
    switch(type,
           dbl = rep(NA_real_, n),
           chr = rep(NA_character_, n),
           time = as.POSIXct(rep(NA, n)))
  }
  # Multi-candidate resolver
  take_first <- function(df, candidates, type = c("dbl","chr","time")) {
    type <- match.arg(type)
    for (c in candidates) {
      if (c %in% names(df)) return(df[[c]])
    }
    switch(type,
           dbl = rep(NA_real_, n),
           chr = rep(NA_character_, n),
           time = as.POSIXct(rep(NA, n)))
  }
  tz_vec <- if (!is.na(tz_col) && tz_col %in% names(sm)) sm[[tz_col]] else rep(NA_character_, n)
  out <- tibble::tibble(
    !!rlang::sym(id_col) := sm[[id_col]],
    timezone = tz_vec,
    start_utc = take_first(sm, c("w_startdate_utc","startdate_utc"), "time"),
    end_utc = take_first(sm, c("w_enddate_utc","enddate_utc"), "time"),
    duration_h = as.numeric(take_first(sm, c("w_total_sleep_time","total_sleep_time"), "dbl")) / 3600,
    efficiency = take_first(sm, c("w_sleep_efficiency","sleep_efficiency"), "dbl"),
    latency_min = as.numeric(take_first(sm, c("w_sleep_latency","sleep_latency"), "dbl")) / 60,
    waso_min = as.numeric(take_first(sm, c("w_waso","waso"), "dbl")) / 60,
    ahi = take_first(sm, c("w_apnea_hypopnea_index","apnea_hypopnea_index"), "dbl"),
    snoring_min = as.numeric(take_first(sm, c("w_snoring","snoring"), "dbl")) / 60
  )
  out
}

#' @export
print.withings_sleep <- function(x, ...) {
  cat("Withings sleep object\n")
  cat("  Records:", x$info$n_records, "\n")
  cat("  Epoch rows:", x$info$n_epochs, "\n")
  if ("w_startdate_utc" %in% names(x$summary)) {
    rng <- range(x$summary$w_startdate_utc, na.rm = TRUE)
    if (all(is.finite(rng))) cat("  Date range (UTC):", format(rng[1]), "to", format(rng[2]), "\n")
  }
  invisible(x)
}

#' Calculate Sleep Regularity Index (SRI) from Withings Epoch Data
#'
#' @description
#' Computes the Sleep Regularity Index (SRI) from minute-by-minute sleep epoch data
#' and generates a raster plot visualization of sleep/wake patterns across days.
#' This is an internal function called by the `$sri()` method of `withings_sleep` objects.
#'
#' @param x A `withings_sleep` object with epoch data
#' @param tz Character string specifying timezone for plotting (e.g., "America/New_York").
#'   If NULL, attempts to extract from summary data. Defaults to "UTC" if not found.
#' @param plot_path Optional file path to save the raster plot (e.g., "sri_plot.png").
#'   If NULL, plot is generated but not saved.
#' @param assumption Character string specifying how to handle missing epoch data:
#'   \describe{
#'     \item{"wake"}{(default) Treat all missing epochs as wake (state = 0)}
#'     \item{"continuity"}{Carry forward the last known sleep/wake state for missing epochs}
#'   }
#' @param continuity_limit Optional integer. When `assumption = "continuity"`, this limits
#'   how many consecutive minutes to carry forward a state before reverting to wake.
#'   For example, `continuity_limit = 10` means gaps longer than 10 minutes revert to wake.
#'   Only applies when `assumption = "continuity"`. Default: NULL (no limit).
#'
#' @return A list with three components:
#'   \describe{
#'     \item{sri}{Numeric value between -100 and 100. Higher values indicate more regular
#'       sleep-wake patterns. Values above 80 are considered good regularity.}
#'     \item{raster}{A ggplot2 object showing sleep/wake patterns across days. Black = sleep,
#'       light red = missing/wake. Days are defined as noon-to-noon periods.}
#'     \item{report}{Character string with formatted summary including SRI value, data coverage,
#'       assumption details, and processing statistics.}
#'   }
#'
#' @details
#' **Sleep Regularity Index (SRI):**
#' 
#' SRI quantifies the day-to-day consistency of sleep-wake patterns by comparing binary
#' sleep/wake states at each minute across consecutive 24-hour periods. The formula is:
#' 
#' \deqn{SRI = -100 + 200 \times (1 - \text{mean}(|S_{t+24h} - S_t|))}
#' 
#' where \eqn{S_t} is the sleep state (0 or 1) at time t.
#' 
#' **Interpretation:**
#' - SRI = 100: Perfect regularity (same sleep/wake pattern every day)
#' - SRI = 0: Random sleep/wake patterns
#' - SRI = -100: Completely reversed patterns (sleep when usually awake, vice versa)
#' - SRI > 80: Generally considered good sleep regularity
#' - SRI < 60: May indicate irregular sleep schedules
#' 
#' **Missing Data Handling:**
#' 
#' Real-world epoch data often has gaps. This function provides two strategies:
#' 
#' 1. **"wake" assumption (default)**: Conservative approach treating all missing data as wake.
#'    Best when gaps represent true wake periods (e.g., device removed during day).
#' 
#' 2. **"continuity" assumption**: Assumes sleep/wake state continues during short gaps.
#'    Useful for handling brief data dropouts during sleep sessions.
#'    - Respects sleep session boundaries (never carries state across different nights)
#'    - Optional `continuity_limit` prevents infinite carry-forward into long gaps
#' 
#' **Requirements:**
#' - Minimum 2 days of data (2 × 24 × 60 = 2,880 minutes)
#' - Epoch data must include `timestamp` and `state` columns
#' - Sleep states are binarized: 0 = wake, 1+ (light/deep/REM) = sleep
#'
#' @examples
#' \dontrun{
#' # Create withings_sleep object (epoch-only mode)
#' wsleep <- withings_sleep(summary = NULL, epoch = epoch_data)
#' 
#' # Calculate SRI with default settings
#' result <- wsleep$sri()
#' cat(result$report)
#' print(result$sri)
#' print(result$raster)
#' 
#' # Save plot to file
#' result <- wsleep$sri(plot_path = "sri_plot.png")
#' 
#' # Use continuity assumption for missing data
#' result <- wsleep$sri(
#'   assumption = "continuity",
#'   continuity_limit = 10  # Max 10 minutes carry-forward
#' )
#' 
#' # Specify timezone explicitly
#' result <- wsleep$sri(tz = "America/Los_Angeles", plot_path = "sri_LA.png")
#' }
#'
#' @references
#' Phillips AJK, et al. (2017). Irregular sleep/wake patterns are associated with
#' poorer academic performance and delayed circadian and sleep/wake timing.
#' Scientific Reports, 7, 3216.
#'
#' @seealso \code{\link{withings_sleep}} for creating sleep objects
#'
#' @keywords internal
wsleep_sri <- function(x, tz = NULL, plot_path = NULL, assumption = "wake", continuity_limit = NULL) {
  epoch <- x$epoch
  # Get timezone from summary if not provided
  if (is.null(tz) || tz == "UTC") {
    tz <- NA
    if (!is.null(x$summary)) {
      for (col in c("w_timezone", "timezone")) {
        if (col %in% names(x$summary)) {
          tz <- x$summary[[col]][1]
          break
        }
      }
    }
    if (is.na(tz) || tz == "") tz <- "UTC"
  }
  ts <- as.numeric(epoch$timestamp)
  ts_seq <- seq(min(ts), max(ts), by = 60)
  # Convert to POSIXct in correct tz for plotting
  ts_seq_posix <- as.POSIXct(ts_seq, origin = "1970-01-01", tz = tz)
  state_map <- rep(0, length(ts_seq))
  names(state_map) <- ts_seq
  idx <- match(ts, ts_seq)
  
  n_total <- length(ts_seq)
  n_known <- length(idx)
  n_missing <- n_total - n_known
  message(sprintf("Total minutes: %d, Known: %d, Missing: %d (%.1f%%)",
                  n_total, n_known, n_missing, 100 * n_missing / n_total))
  
  # Identify sleep ID column
  id_col <- NULL
  for (col in c("w_id", "sleep_id", "id")) {
    if (col %in% names(epoch)) {
      id_col <- col
      break
    }
  }
  
  # Mark which indices in ts_seq belong to which sleep session
  sleep_session_map <- rep(NA, length(ts_seq))
  if (!is.null(id_col)) {
    sleep_session_map[idx] <- epoch[[id_col]]
  }
  
  # Fill in known values
  state_map[idx] <- ifelse(epoch$state > 0, 1, 0)
  
  # Track statistics for debugging
  n_between_sessions <- 0
  n_continuity_filled <- 0
  n_limit_exceeded <- 0
  
  # Apply assumption logic for missing values
  if (assumption == "continuity") {
    # Forward fill with optional limit, respecting sleep session boundaries
    last_known <- 0  # default to wake
    gap_counter <- 0
    current_session <- NA
    for (i in seq_along(state_map)) {
      if (i %in% idx) {
        # Known value - this is actual epoch data
        last_known <- state_map[i]
        gap_counter <- 0
        current_session <- sleep_session_map[i]
      } else {
        # Missing value - need to decide what to do
        gap_counter <- gap_counter + 1
        
        # Look ahead to see if next known data point is from a different session
        next_session_idx <- which(idx > i)[1]
        if (!is.na(next_session_idx)) {
          next_session <- sleep_session_map[idx[next_session_idx]]
          if (!is.na(current_session) && !is.na(next_session) && current_session != next_session) {
            # We're between two different sleep sessions - force wake
            state_map[i] <- 0
            n_between_sessions <- n_between_sessions + 1
            next
          }
        }
        
        # Within same session - apply continuity rules
        if (!is.null(continuity_limit) && gap_counter > continuity_limit) {
          # Exceeded limit, revert to wake
          state_map[i] <- 0
          n_limit_exceeded <- n_limit_exceeded + 1
        } else {
          # Continue last known state
          state_map[i] <- last_known
          n_continuity_filled <- n_continuity_filled + 1
        }
      }
    }
    message(sprintf("Continuity stats: %d between sessions (wake), %d filled by continuity, %d limit exceeded",
                    n_between_sessions, n_continuity_filled, n_limit_exceeded))
  }
  # If assumption == "wake", state_map already defaults to 0 for missing values
  
  n <- length(state_map)
  if (n < 2*24*60) return(list(sri = NA, raster = NULL))
  SWV1 <- state_map[1:(n-(24*60))]
  SWV2 <- state_map[((24*60)+1):n]
  sri <- -100 + 200*(1-mean(abs(SWV2-SWV1),na.rm=TRUE))
  
  # Always generate raster plot
  SWS <- data.frame(trans = state_map, t = ts_seq)
  SWS$tmin <- round(SWS$t/60)
  SWS$t_posix <- as.POSIXct(SWS$t, origin = "1970-01-01", tz = tz)
  onind <- which(SWS$trans == 1 | is.na(SWS$trans))
  if (length(onind) > 0 && onind[length(onind)] == nrow(SWS)){
    onind <- onind[-length(onind)]
  }
  slt <- vector(); grp <- vector(); slt_posix <- vector()
  for (i in seq_along(onind)){
    idx1 <- onind[i]
    idx2 <- onind[i]+1
    if (idx2 > nrow(SWS) || is.na(SWS$t_posix[idx1]) || is.na(SWS$t_posix[idx2])) next
    wrt <- SWS$tmin[idx1]:SWS$tmin[idx2]
    wrt_posix <- seq(SWS$t_posix[idx1], SWS$t_posix[idx2], by = "min")
    if (length(wrt) == 0 || length(wrt_posix) == 0) next
    slt <- c(slt, wrt)
    slt_posix <- c(slt_posix, wrt_posix)
    if (SWS$trans[idx1] == 1 && !is.na(SWS$trans[idx1])){
      grp <- c(grp, rep("Sleep", length(wrt)))
    } else {
      grp <- c(grp, rep("NA", length(wrt)))
    }
  }
  maxdays <- ceiling((range(SWS$t)[2] - range(SWS$t)[1])/60/60/24)
  rdf <- data.frame(grp = grp, t = slt, t_posix = as.POSIXct(slt_posix, origin = "1970-01-01", tz = tz), day = maxdays)
  # Robust hour calculation
  hour_raw <- suppressWarnings(as.numeric(strftime(rdf$t_posix, format = "%H")) + as.numeric(strftime(rdf$t_posix, format = "%M"))/60)
  hour_raw[!is.finite(hour_raw) | is.na(hour_raw)] <- 0
  rdf$hour <- hour_raw
  # Calculate day as noon-to-noon (shift by 12 hours before taking date)
  rdf$date_shifted <- as.Date(rdf$t_posix - 12*60*60, tz = tz)
  rdf$day_num <- as.numeric(rdf$date_shifted - min(rdf$date_shifted)) + 1
  abb_x <- seq(0, 24, by = 4)
  max_day <- max(rdf$day_num, na.rm = TRUE)
  abb_y <- seq(1, max_day, by = max(1, floor(max_day/20)))
  # Remove any rows with NA or non-finite hour
  rdf <- rdf[is.finite(rdf$hour) & !is.na(rdf$hour) & !is.na(rdf$day_num), ]
  raster_plot <- ggplot(rdf, aes(x = hour, y = day_num, color = grp)) +
    geom_point(size=6, shape="|") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 24, linetype = "dotted", color = "gray50", linewidth = 0.5) +
    scale_y_continuous(breaks=abb_y, labels=abb_y) +
    scale_x_continuous(breaks=abb_x, limits=c(0,24), labels=abb_x) +
    scale_color_manual(values=c("#000000", "#FFABAB")) +
    xlab(paste0("Time (",tz,", h)")) +
    ylab("Day") +
    theme_classic() +
    theme(text = element_text(size = 18), axis.text = element_text(size = 16), legend.title = element_blank()) +
    guides(color = guide_legend(override.aes = list(shape = 15)))
  
  # Save plot if path is provided
  if (!is.null(plot_path)) {
    ggsave(plot_path, plot = raster_plot, width = 10, height = 8, dpi = 300)
  }
  
  # Create report summary
  report_lines <- c(
    "=== Sleep Regularity Index (SRI) Report ===",
    "",
    sprintf("SRI: %.2f", sri),
    sprintf("Range: -100 (completely irregular) to 100 (perfectly regular)"),
    "",
    "Data Summary:",
    sprintf("  Total time span: %d minutes (%.1f days)", n_total, n_total/60/24),
    sprintf("  Known epochs: %d minutes (%.1f%%)", n_known, 100 * n_known / n_total),
    sprintf("  Missing epochs: %d minutes (%.1f%%)", n_missing, 100 * n_missing / n_total),
    "",
    sprintf("Assumption method: %s", assumption)
  )
  
  if (assumption == "continuity") {
    report_lines <- c(report_lines,
      sprintf("  Between-session gaps (wake): %d minutes", n_between_sessions),
      sprintf("  Within-session filled by continuity: %d minutes", n_continuity_filled)
    )
    if (!is.null(continuity_limit)) {
      report_lines <- c(report_lines,
        sprintf("  Continuity limit exceeded (wake): %d minutes", n_limit_exceeded),
        sprintf("  Continuity limit: %d minutes", continuity_limit)
      )
    }
  }
  
  report_lines <- c(report_lines,
    "",
    sprintf("Timezone: %s", tz),
    sprintf("Days analyzed: %d (noon-to-noon boundaries)", max_day),
    ""
  )
  
  report <- paste(report_lines, collapse = "\n")
  
  list(sri = sri, raster = raster_plot, report = report)
}
