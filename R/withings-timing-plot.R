#' Horizontal sleep timing plot (plotly)
#'
#' Renders a bar chart of sleep start/end times across nights, with a metrics table
#'
#' @param summary summary tibble (processed)
#' @param epoch optional epoch tibble (for extended wake episodes if needed)
#' @return list with plot (plotly object) and metrics (data frame), or NULL
#' @keywords internal
.render_horizontal_sleep_timing_plot <- function(summary, epoch = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    warning("plotly package required for interactive timing plot")
    return(NULL)
  }
  
  valid <- summary[!is.na(.take_col(summary, c("enddate_utc","w_enddate_utc"))), ]
  if (nrow(valid) == 0) return(NULL)
  
  # Sort by end date
  end_utc <- .take_col(valid, c("enddate_utc","w_enddate_utc"))
  valid <- valid[order(end_utc), ]
  
  # Helper: minutes from noon (for plotting horizontal bars across midnight)
  mins_from_noon <- function(dt, tz = "UTC") {
    if (is.na(dt)) return(NA)
    local_dt <- as.POSIXlt(dt, tz = tz)
    ((local_dt$hour * 60 + local_dt$min) - 720 + 1440) %% 1440
  }
  
  # Format in timezone
  fmt_tz <- function(dt, tz = "UTC", fmt = "%d-%b %H:%M") {
    if (is.na(dt)) return(NA)
    format(dt, format = fmt, tz = tz, usetz = FALSE)
  }
  
  plot_data <- lapply(seq_len(nrow(valid)), function(i) {
    row <- valid[i, ]
    tz <- .take_col(row, c("timezone","w_timezone"))
    if (is.null(tz) || is.na(tz)) tz <- "UTC"
    
    start_utc <- .take_col(row, c("startdate_utc","w_startdate_utc"))
    end_utc <- .take_col(row, c("enddate_utc","w_enddate_utc"))
    
    if (is.null(start_utc) || is.null(end_utc)) return(NULL)
    
    # Ensure dates are POSIXct (handle both string and POSIXct input)
    if (is.character(start_utc)) {
      start_utc <- as.POSIXct(start_utc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      if (is.na(start_utc)) start_utc <- as.POSIXct(.take_col(row, c("startdate_utc","w_startdate_utc")), tz = "UTC")
    }
    if (is.character(end_utc)) {
      end_utc <- as.POSIXct(end_utc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      if (is.na(end_utc)) end_utc <- as.POSIXct(.take_col(row, c("enddate_utc","w_enddate_utc")), tz = "UTC")
    }
    
    start_min <- mins_from_noon(start_utc, tz)
    end_min_raw <- mins_from_noon(end_utc, tz)
    
    if (is.na(start_min) || is.na(end_min_raw)) return(NULL)
    
    # Wrap if end < start (crosses midnight)
    end_min <- if (end_min_raw < start_min) end_min_raw + 1440 else end_min_raw
    
    lat <- .take_col(row, c("sleep_latency","w_sleep_latency"))
    latency_sec <- if (!is.null(lat) && !is.na(lat)) as.numeric(lat) else 0
    
    sleep_id <- .take_col(row, c("id","w_id","sleep_id"))
    if (is.null(sleep_id)) sleep_id <- i
    
    tst <- .take_col(row, c("total_sleep_time","w_total_sleep_time"))
    tib <- .take_col(row, c("total_timeinbed","w_total_timeinbed"))
    eff <- .take_col(row, c("sleep_efficiency","w_sleep_efficiency"))
    ahi <- .take_col(row, c("apnea_hypopnea_index","w_apnea_hypopnea_index"))
    snor <- .take_col(row, c("snoring","w_snoring"))
    hr_avg <- .take_col(row, c("hr_average","w_hr_average"))
    out_bed <- .take_col(row, c("out_of_bed_count","w_out_of_bed_count"))
    
    list(
      id = sleep_id,
      y = fmt_tz(end_utc, tz, "%d-%b"),
      base = start_min,
      duration = end_min - start_min,
      start_min = start_min,
      latency_min = latency_sec / 60,
      start_str = fmt_tz(start_utc, tz),
      end_str = fmt_tz(end_utc, tz),
      tst_h = if (!is.null(tst)) as.numeric(tst) / 3600 else NA,
      tib_h = if (!is.null(tib)) as.numeric(tib) / 3600 else NA,
      eff_pct = if (!is.null(eff)) as.numeric(eff) * 100 else NA,
      ahi = if (!is.null(ahi)) round(as.numeric(ahi), 1) else NA,
      snor_min = if (!is.null(snor)) round(as.numeric(snor) / 60) else NA,
      hr_avg = if (!is.null(hr_avg)) round(as.numeric(hr_avg)) else NA,
      out_bed = if (!is.null(out_bed)) as.integer(out_bed) else NA
    )
  })
  
  plot_data <- Filter(Negate(is.null), plot_data)
  if (length(plot_data) == 0) return(NULL)
  
  # Handle duplicate dates (multiple sleeps on same day) by adding suffixes
  y_labels_raw <- sapply(plot_data, `[[`, "y")
  y_counts <- table(y_labels_raw)
  y_counter <- list()
  y_labels <- sapply(y_labels_raw, function(label) {
    if (y_counts[[label]] > 1) {
      if (is.null(y_counter[[label]])) y_counter[[label]] <<- 1
      count <- y_counter[[label]]
      y_counter[[label]] <<- count + 1
      paste0(label, " (", count, ")")
    } else {
      label
    }
  })
  
  # Update plot_data with new labels
  for (i in seq_along(plot_data)) {
    plot_data[[i]]$y <- y_labels[i]
  }
  
  # Extract vectors for traces
  sleep_ids <- sapply(plot_data, `[[`, "id")
  durations <- sapply(plot_data, `[[`, "duration")
  bases <- sapply(plot_data, `[[`, "base")
  latencies <- sapply(plot_data, `[[`, "latency_min")
  start_mins <- sapply(plot_data, `[[`, "start_min")
  
  # X-axis ticks (time labels)
  tick_vals <- seq(0, 48 * 60, by = 60)
  tick_text <- sapply(tick_vals, function(v) {
    h <- ((v + 720) %/% 60) %% 24
    sprintf("%02d:00", h)
  })
  
  min_x <- min(bases)
  max_x <- max(bases + durations)
  
  # Build metrics table (separate from plot)
  metrics_df <- data.frame(
    Date = y_labels,
    TST = sapply(plot_data, function(d) ifelse(is.na(d$tst_h), "—", .format_hours_and_minutes(d$tst_h))),
    TIB = sapply(plot_data, function(d) ifelse(is.na(d$tib_h), "—", .format_hours_and_minutes(d$tib_h))),
    Efficiency = sapply(plot_data, function(d) ifelse(is.na(d$eff_pct), "—", paste0(round(d$eff_pct), "%"))),
    HR = sapply(plot_data, function(d) ifelse(is.na(d$hr_avg), "—", paste0(d$hr_avg, " bpm"))),
    Snoring = sapply(plot_data, function(d) ifelse(is.na(d$snor_min), "—", paste0(d$snor_min, " min"))),
    AHI = sapply(plot_data, function(d) ifelse(is.na(d$ahi), "—", as.character(d$ahi))),
    `Out of Bed` = sapply(plot_data, function(d) ifelse(is.na(d$out_bed), "—", as.character(d$out_bed))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Build plotly using plot_ly directly with data vectors
  p <- plotly::plot_ly()
  
  # Add duration bars (sleep time)
  p <- plotly::add_trace(
    p,
    type = "bar",
    orientation = "h",
    y = y_labels,
    x = durations,
    base = bases,
    customdata = sleep_ids,
    marker = list(color = "#75baf5"),
    name = "Sleep",
    hovertemplate = paste0(
      y_labels, "<br>",
      "Start: ", sapply(plot_data, `[[`, "start_str"), "<br>",
      "End: ", sapply(plot_data, `[[`, "end_str"), "<br>",
      "<i>Click for details</i><extra></extra>"
    )
  )
  
  # Add latency bars (white overlay at start)
  p <- plotly::add_trace(
    p,
    type = "bar",
    orientation = "h",
    y = y_labels,
    x = latencies,
    base = start_mins,
    marker = list(color = "white", line = list(color = "#75baf5", width = 1)),
    name = "Latency",
    hovertemplate = paste0(
      "Sleep Latency<br>",
      "Duration: ", round(latencies, 1), " min<extra></extra>"
    )
  )
  
  # Layout (no annotations - metrics in separate table)
  p <- plotly::layout(
    p,
    barmode = "overlay",
    xaxis = list(
      title = "Time of Day",
      tickmode = "array",
      tickvals = tick_vals,
      ticktext = tick_text,
      showgrid = FALSE,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1
    ),
    yaxis = list(
      title = "",
      type = "category",
      autorange = "reversed",
      automargin = TRUE,
      showgrid = FALSE,
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      showline = TRUE,
      linecolor = "black",
      linewidth = 1
    ),
    hovermode = "closest",
    showlegend = FALSE,
    height = max(400, length(y_labels) * 30 + 100),
    margin = list(l = 80, r = 20, t = 40, b = 40)
  )
  
  list(plot = p, metrics = metrics_df)
}
