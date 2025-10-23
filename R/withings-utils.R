#' Withings JSON to Data Frame
#'
#' Converts Withings sleep summary JSON (series-based) into a tidy tibble.
#'
#' @param x A JSON string, a file path to JSON, or a parsed list (from jsonlite::fromJSON)
#' @return A tibble with w_* prefixed columns for summary metrics and metadata.
#'         Includes derived POSIXct columns in UTC for start/end/created/modified.
#' @export
withings_json_to_df <- function(x) {
  # Accept JSON text, path, or parsed list
  obj <- x
  if (is.character(x) && length(x) == 1L) {
    if (file.exists(x)) {
      obj <- jsonlite::fromJSON(x, simplifyVector = TRUE)
    } else {
      obj <- jsonlite::fromJSON(x, simplifyVector = TRUE)
    }
  }
  if (is.raw(x)) {
    obj <- jsonlite::fromJSON(rawToChar(x), simplifyVector = TRUE)
  }

  if (!is.list(obj)) stop("Input must be JSON text, a file path, or a parsed list")
  series <- obj$series %||% obj$data %||% obj$items
  if (is.null(series) || length(series) == 0) return(tibble::tibble())

  # If series is a data.frame (common when simplifyVector=TRUE), split into row-wise list
  if (is.data.frame(series)) {
    series <- split(series, seq_len(nrow(series)))
  }

  flatten_one <- function(s) {
    # Ensure list-like access even if s is a one-row data.frame
    get1 <- function(name, default = NULL) {
      if (is.null(s[[name]])) return(default)
      val <- s[[name]]
      if (is.data.frame(val) && nrow(val) == 1) return(as.vector(t(val)))
      if (is.list(val) && length(val) == 1) return(val[[1]])
      val
    }

    # Top-level fields
    top <- list(
      w_timezone = get1("timezone", get1("tz", NA_character_)),
      w_model = get1("model", NA_real_),
      w_model_id = get1("model_id", NA_real_),
      w_startdate = get1("startdate", NA_real_),
      w_enddate = get1("enddate", NA_real_),
      w_date = get1("date", NA_character_),
      w_created = get1("created", NA_real_),
      w_modified = get1("modified", NA_real_)
    )

    dat <- get1("data", list()) %||% list()
    # If dat was a one-row data.frame coerced to vector, rebuild named list
    if (is.data.frame(dat) && nrow(dat) == 1) {
      dat <- as.list(dat[1, , drop = TRUE])
    } else if (is.atomic(dat) && !is.null(names(dat))) {
      dat <- as.list(dat)
    }
    # Clean invalid names, then prefix nested fields with w_
    if (length(dat)) {
      nm <- names(dat)
      keep <- !is.null(nm) & nzchar(nm)
      if (any(!keep)) dat <- dat[keep]
      if (length(dat)) {
        dat <- stats::setNames(dat, paste0("w_", names(dat)))
      }
    }

    out <- c(top, dat)

    # Derive POSIXct columns (UTC)
    to_time <- function(sec) ifelse(is.na(sec) | is.null(sec), NA_real_, as.numeric(sec))
    out$w_startdate_utc <- if (!is.null(out$w_startdate)) as.POSIXct(to_time(out$w_startdate), origin = "1970-01-01", tz = "UTC") else as.POSIXct(NA)
    out$w_enddate_utc   <- if (!is.null(out$w_enddate))   as.POSIXct(to_time(out$w_enddate),   origin = "1970-01-01", tz = "UTC") else as.POSIXct(NA)
    out$w_created_utc   <- if (!is.null(out$w_created))   as.POSIXct(to_time(out$w_created),   origin = "1970-01-01", tz = "UTC") else as.POSIXct(NA)
    out$w_modified_utc  <- if (!is.null(out$w_modified))  as.POSIXct(to_time(out$w_modified),  origin = "1970-01-01", tz = "UTC") else as.POSIXct(NA)

    # Best-effort ID fields if present
  sleep_id_val <- if (is.list(dat)) dat[["w_sleep_id"]] else NULL
  out$w_id <- get1("w_id", get1("id", sleep_id_val %||% NA))
  out$w_withings_index <- if (is.list(dat)) (dat[["w_withings_index"]] %||% NA) else NA
    # Ensure simple scalars (no lists)
    out <- lapply(out, function(v) if (length(v) == 0) NA else v)
  tibble::as_tibble(out, .name_repair = "unique")
  }

  res <- purrr::map_dfr(series, flatten_one)
  # Normalize types for common numeric seconds columns that may be character
  num_cols <- c("w_total_sleep_time", "w_total_timeinbed", "w_lightsleepduration",
                "w_remsleepduration", "w_deepsleepduration", "w_sleep_latency",
                "w_wakeup_latency", "w_wakeupduration", "w_waso", "w_hr_average",
                "w_hr_min", "w_hr_max", "w_rr_average", "w_rr_min", "w_rr_max",
                "w_snoring", "w_snoringepisodecount", "w_apnea_hypopnea_index",
                "w_breathing_disturbances_intensity")
  for (nm in intersect(names(res), num_cols)) {
    res[[nm]] <- suppressWarnings(as.numeric(res[[nm]]))
  }
  res
}


#' Read Withings CSV helper
#'
#' @param path Path to CSV (epochs or summary)
#' @return tibble
#' @keywords internal
withings_read_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}
