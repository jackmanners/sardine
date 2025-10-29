#' Withings report summary statistics
#'
#' Helpers to compute duration/efficiency, sleep ritual, and vitals summaries
#'
#' @name withings_summary_stats
#' @keywords internal
NULL

#' @rdname withings_summary_stats
.format_hours_and_minutes <- function(decimal_hours) {
  if (is.na(decimal_hours) || is.null(decimal_hours)) return("\u2014")
  h <- floor(decimal_hours)
  m <- round((decimal_hours - h) * 60)
  paste0(h, "h", m)
}

#' @rdname withings_summary_stats
.is_weekend <- function(dates) {
  # Convert to POSIXct if character
  if (is.character(dates)) {
    dates <- as.POSIXct(dates, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    # Fallback if format doesn't match
    if (all(is.na(dates))) dates <- as.POSIXct(dates, tz = "UTC")
  }
  # Saturday=6, Sunday=0 in R (Mon=1, Sun=0)
  wday <- as.POSIXlt(dates)$wday
  wday >= 6 | wday == 0
}

#' @rdname withings_summary_stats
.take_col <- function(df, candidates) {
  for (c in candidates) {
    if (c %in% names(df)) return(df[[c]])
  }
  rep(NA, nrow(df))  # Return NA vector of correct length if not found
}

#' Duration, efficiency, regularity summary
#'
#' @param summary processed summary tibble
#' @return tibble with aggregated metrics
#' @keywords internal
.duration_efficiency_regularity <- function(summary) {
  valid <- summary[!is.na(.take_col(summary, c("enddate_utc","w_enddate_utc"))), ]
  if (nrow(valid) == 0) return(NULL)
  
  # Resolve total_sleep_time (seconds)
  tst <- .take_col(valid, c("total_sleep_time","w_total_sleep_time"))
  if (is.null(tst)) return(NULL)
  tst_h <- as.numeric(tst) / 3600
  
  # Time in bed
  tib <- .take_col(valid, c("total_timeinbed","w_total_timeinbed"))
  tib_h <- if (!is.null(tib)) as.numeric(tib) / 3600 else NULL
  
  # Sleep latency (seconds)
  lat <- .take_col(valid, c("sleep_latency","w_sleep_latency"))
  lat_min <- if (!is.null(lat)) as.numeric(lat) / 60 else NULL
  
  # Sleep efficiency
  eff <- .take_col(valid, c("sleep_efficiency","w_sleep_efficiency"))
  eff_pct <- if (!is.null(eff)) as.numeric(eff) * 100 else NULL
  
  # End date for weekend check
  end_utc <- .take_col(valid, c("enddate_utc","w_enddate_utc"))
  is_wknd <- .is_weekend(end_utc)
  
  weekday_tst <- tst_h[!is_wknd]
  weekend_tst <- tst_h[is_wknd]
  
  tibble::tibble(
    Metric = c(
      "Sleep Duration Weekdays",
      "Sleep Duration Weekends",
      "Sleep Latency Avg (min)",
      "Time In Bed (TIB, h)",
      "Total Sleep Time (TST, h)",
      "Sleep Efficiency (%)"
    ),
    Value = c(
      .format_hours_and_minutes(mean(weekday_tst, na.rm = TRUE)),
      .format_hours_and_minutes(mean(weekend_tst, na.rm = TRUE)),
      as.character(round(mean(lat_min, na.rm = TRUE))),
      .format_hours_and_minutes(mean(tib_h, na.rm = TRUE)),
      .format_hours_and_minutes(mean(tst_h, na.rm = TRUE)),
      as.character(round(mean(eff_pct, na.rm = TRUE), 2))
    )
  )
}

#' Sleep ritual summary
#'
#' @param summary processed summary tibble
#' @return tibble with ritual metrics (bedtime, wake-up, etc.)
#' @keywords internal
.sleep_ritual <- function(summary) {
  valid <- summary[!is.na(.take_col(summary, c("enddate_utc","w_enddate_utc"))), ]
  if (nrow(valid) == 0) return(NULL)
  
  start_utc <- .take_col(valid, c("startdate_utc","w_startdate_utc"))
  end_utc <- .take_col(valid, c("enddate_utc","w_enddate_utc"))
  dur_sleep <- .take_col(valid, c("durationtosleep","w_durationtosleep"))
  dur_wake <- .take_col(valid, c("durationtowakeup","w_durationtowakeup"))
  
  if (is.null(start_utc) || is.null(end_utc)) return(NULL)
  
  # Convert to POSIXct if character
  if (is.character(start_utc)) {
    start_utc <- as.POSIXct(start_utc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    if (all(is.na(start_utc))) start_utc <- as.POSIXct(.take_col(valid, c("startdate_utc","w_startdate_utc")), tz = "UTC")
  }
  if (is.character(end_utc)) {
    end_utc <- as.POSIXct(end_utc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    if (all(is.na(end_utc))) end_utc <- as.POSIXct(.take_col(valid, c("enddate_utc","w_enddate_utc")), tz = "UTC")
  }
  
  # Get timezone for each record - use local timezone for time calculations
  tz_col <- .take_col(valid, c("timezone","w_timezone"))
  
  # Helper function to convert to minutes from midnight in local timezone
  to_mins <- function(dt_utc, tz) {
    # Convert each UTC time to its local timezone
    local_times <- mapply(function(dt, timezone) {
      if (is.na(dt) || is.null(timezone) || is.na(timezone)) return(NA)
      local_dt <- as.POSIXlt(dt, tz = timezone)
      local_dt$hour * 60 + local_dt$min
    }, dt_utc, tz, SIMPLIFY = TRUE)
    return(local_times)
  }
  
  mins_to_str <- function(m) {
  if (is.na(m)) return("\u2014")
    h <- floor(m / 60) %% 24
    mn <- round(m %% 60)
    # Convert to 12-hour format with AM/PM
    period <- if (h < 12) "AM" else "PM"
    h12 <- if (h == 0) 12 else if (h > 12) h - 12 else h
    sprintf("%d:%02d %s", h12, mn, period)
  }
  
  bedtime <- to_mins(start_utc, tz_col)
  waketime <- to_mins(end_utc, tz_col)
  
  sleep_mins <- if (!is.null(dur_sleep)) as.numeric(dur_sleep) / 60 else 0
  wake_mins <- if (!is.null(dur_wake)) as.numeric(dur_wake) / 60 else 0
  
  is_wknd <- .is_weekend(end_utc)
  
  # Helper to average times that may cross midnight (for bedtime calculations)
  # Times between midnight and 6am are treated as "next day" for averaging
  mean_time_crossing_midnight <- function(mins) {
    # Adjust times: if time is between 0-360 (midnight to 6am), add 1440 (treat as next day)
    adjusted <- ifelse(mins < 360, mins + 1440, mins)
    mean_val <- mean(adjusted, na.rm = TRUE)
    # Wrap back to 0-1439 range
    mean_val %% 1440
  }
  
  tibble::tibble(
    Metric = c(
      "Weekdays Bedtime",
      "Weekdays Time to Fall Asleep",
      "Weekdays Wake-Up",
      "Weekdays Get-Up",
      "Weekends Bedtime",
      "Weekends Time to Fall Asleep",
      "Weekends Wake-Up",
      "Weekends Get-Up"
    ),
    Time = c(
      mins_to_str(mean_time_crossing_midnight(bedtime[!is_wknd])),
      mins_to_str(mean_time_crossing_midnight((bedtime + sleep_mins)[!is_wknd])),
      mins_to_str(mean(waketime[!is_wknd], na.rm = TRUE)),
      mins_to_str(mean((waketime + wake_mins)[!is_wknd], na.rm = TRUE)),
      mins_to_str(mean_time_crossing_midnight(bedtime[is_wknd])),
      mins_to_str(mean_time_crossing_midnight((bedtime + sleep_mins)[is_wknd])),
      mins_to_str(mean(waketime[is_wknd], na.rm = TRUE)),
      mins_to_str(mean((waketime + wake_mins)[is_wknd], na.rm = TRUE))
    )
  )
}

#' Sleep vitals summary
#'
#' @param summary processed summary tibble
#' @return tibble with AHI, snoring, HR stats
#' @keywords internal
.sleep_vitals <- function(summary) {
  valid <- summary[!is.na(.take_col(summary, c("enddate_utc","w_enddate_utc"))), ]
  if (nrow(valid) == 0) return(NULL)
  
  ahi <- .take_col(valid, c("apnea_hypopnea_index","w_apnea_hypopnea_index"))
  snor <- .take_col(valid, c("snoring","w_snoring"))
  tst <- .take_col(valid, c("total_sleep_time","w_total_sleep_time"))
  hr_avg <- .take_col(valid, c("hr_average","w_hr_average"))
  hr_min <- .take_col(valid, c("hr_min","w_hr_min"))
  hr_max <- .take_col(valid, c("hr_max","w_hr_max"))
  
  if (!is.null(ahi)) {
    ahi_avg <- mean(ahi, na.rm = TRUE)
    ahi_min <- min(ahi, na.rm = TRUE)
    ahi_max <- max(ahi, na.rm = TRUE)
  } else {
    ahi_avg <- ahi_min <- ahi_max <- NA
  }
  
  snor_min <- if (!is.null(snor)) as.numeric(snor) / 60 else NULL
  
  tibble::tibble(
    Metric = c("AHI (events/hr)", "Snoring (min/night)", "Overnight Heart Rate (bpm)"),
    Min = c(
  ifelse(is.na(ahi_min), "\u2014", as.character(round(ahi_min, 1))),
  "\u2014",
  ifelse(is.null(hr_min), "\u2014", as.character(round(min(hr_min, na.rm = TRUE))))
    ),
    Avg = c(
  ifelse(is.na(ahi_avg), "\u2014", as.character(round(ahi_avg, 1))),
  ifelse(is.null(snor_min), "\u2014", as.character(round(mean(snor_min, na.rm = TRUE)))),
  ifelse(is.null(hr_avg), "\u2014", as.character(round(mean(hr_avg, na.rm = TRUE))))
    ),
    Max = c(
  ifelse(is.na(ahi_max), "\u2014", as.character(round(ahi_max, 1))),
  "\u2014",
  ifelse(is.null(hr_max), "\u2014", as.character(round(max(hr_max, na.rm = TRUE))))
    )
  )
}
