#' Generate nightly epoch plots for a single sleep session
#'
#' @param sleep_id The sleep ID to generate plots for
#' @param epoch_data The epoch data frame
#' @param summary_data The summary data frame  
#' @return A list of plotly plot objects
#' @keywords internal
.generate_nightly_epoch_plots <- function(sleep_id, epoch_data, summary_data) {
  if (is.null(epoch_data) || nrow(epoch_data) == 0) return(NULL)
  # Validate sleep_id
  if (is.null(sleep_id) || length(sleep_id) < 1 || all(is.na(sleep_id))) return(NULL)
  # Reduce to scalar if vector provided
  sleep_id <- sleep_id[1]
  
  # Determine which ID column to use for matching
  id_col <- NULL
  for (col in c("w_id", "sleep_id", "id")) {
    if (col %in% names(epoch_data)) {
      id_col <- col
      break
    }
  }
  if (is.null(id_col)) return(NULL)
  # Guard against missing id column in epoch data
  if (!(id_col %in% names(epoch_data))) return(NULL)
  night_data <- epoch_data[epoch_data[[id_col]] %in% sleep_id, ]
  if (nrow(night_data) == 0) return(NULL)
  
  # Get timezone from summary - use same ID column detection
  summary_id_col <- NULL
  for (col in c("w_id", "sleep_id", "id")) {
    if (col %in% names(summary_data)) {
      summary_id_col <- col
      break
    }
  }
  if (is.null(summary_id_col)) return(NULL)
  summary_row <- summary_data[summary_data[[summary_id_col]] %in% sleep_id, ]
  if (nrow(summary_row) == 0) return(NULL)
  timezone <- summary_row$timezone[1]
   if (length(timezone) == 0 || is.na(timezone)) timezone <- "UTC"
  
  plots <- list()
  
  # 1. Hypnogram (Sleep Stages)
  plots$hypnogram <- .plot_hypnogram(night_data, timezone)
  
  # 2. Heart Rate
  if ("hr" %in% names(night_data)) {
    plots$hr <- .plot_epoch_line(night_data, "hr", "Heart Rate (bpm)", timezone)
  }
  
  # 3. Respiratory Rate
  if ("rr" %in% names(night_data)) {
    plots$rr <- .plot_epoch_line(night_data, "rr", "Respiratory Rate (breaths/min)", timezone)
  }
  
  # 4. Snoring
  if ("snoring" %in% names(night_data)) {
    plots$snoring <- .plot_epoch_line(night_data, "snoring", "Snoring", timezone)
  }
  
  # 5. Movement Score
  if ("mvt_score" %in% names(night_data)) {
    plots$movement <- .plot_epoch_line(night_data, "mvt_score", "Movement Score", timezone)
  }
  
  return(plots)
}

#' Plot hypnogram (sleep stages over time)
#' @keywords internal
.plot_hypnogram <- function(data, timezone) {
  if (!"state" %in% names(data)) return(NULL)
  
  # Stage mapping
  stage_map <- data.frame(
    state = c(0, 1, 2, 3),
    name = c("Awake", "Light", "Deep", "REM"),
    color = c("#EBEBEB", "#669BBC", "#00498D", "#883E8E"),
    plot_y = c(4, 1, 2, 3)
  )
  
  # Join with data
  plot_data <- merge(data, stage_map, by = "state", all.x = TRUE)
  plot_data <- plot_data[order(plot_data$timestamp), ]
  
  # Convert timestamps to datetime
  if ("timestamp" %in% names(plot_data)) {
    plot_data$datetime <- as.POSIXct(plot_data$timestamp, origin = "1970-01-01", tz = timezone)
  } else if ("datetime_utc" %in% names(plot_data)) {
    plot_data$datetime <- as.POSIXct(plot_data$datetime_utc, tz = timezone)
  } else {
    return(NULL)
  }
  
  # Create bar plot
  p <- plotly::plot_ly(
    data = plot_data,
    x = ~datetime,
    y = ~plot_y,
    type = "bar",
    height = 200,
    marker = list(color = ~color),
    text = ~name,
    hovertemplate = "<b>%{text}</b><br>%{x|%H:%M}<extra></extra>"
  ) %>%
    plotly::layout(
      title = "Sleep Stages",
      xaxis = list(title = "Time"),
      yaxis = list(
        title = "Sleep Stage",
        tickmode = "array",
        tickvals = c(1, 2, 3, 4),
        ticktext = c("Light", "Deep", "REM", "Awake"),
        range = c(0.5, 4.5)
      ),
      bargap = 0,
      showlegend = FALSE
    )
  
  return(p)
}

#' Plot epoch line chart (HR, RR, etc.)
#' @keywords internal
.plot_epoch_line <- function(data, column, label, timezone) {
  if (!column %in% names(data)) return(NULL)
  
  # Filter out NA values
  plot_data <- data[!is.na(data[[column]]), ]
  if (nrow(plot_data) < 2) return(NULL)
  
  # Convert timestamps to datetime
  if ("timestamp" %in% names(plot_data)) {
    plot_data$datetime <- as.POSIXct(plot_data$timestamp, origin = "1970-01-01", tz = timezone)
  } else if ("datetime_utc" %in% names(plot_data)) {
    plot_data$datetime <- as.POSIXct(plot_data$datetime_utc, tz = timezone)
  } else {
    return(NULL)
  }
  
  plot_data <- plot_data[order(plot_data$datetime), ]
  
  # Create line plot
  p <- plotly::plot_ly(
    data = plot_data,
    x = ~datetime,
    y = plot_data[[column]],
    type = "scatter",
    mode = "lines",
    line = list(width = 1.5, color = "#75baf5"),
    hovertemplate = paste0("%{y:.2f}<extra></extra>")
  ) %>%
    plotly::layout(
      title = label,
      xaxis = list(title = "Time"),
      yaxis = list(title = label, rangemode = "tozero"),
      showlegend = FALSE
    )
  
  return(p)
}
