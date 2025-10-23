#' Combine Withings epoch and summary data
#'
#' Matches epoch rows to summary rows by id (w_id/sleep_id) and adds a datetime
#' column converted from unix timestamp using the matched timezone.
#'
#' @param epochs tibble/data.frame with columns id or w_id, and timestamp (unix seconds)
#' @param summary tibble/data.frame from withings_json_to_df() or CSV with w_id/sleep_id and timezone
#' @param id_cols character vector of candidate id column names (default: c("w_id","sleep_id","id"))
#' @param tz_cols character vector of candidate timezone columns (default: c("w_timezone","timezone"))
#' @return list with elements: summary (tibble), epoch (tibble with datetime_utc and datetime_local_chr), and a join map
#' @export
combine_withings_epoch_summary <- function(epochs, summary,
                                           id_cols = c("w_id", "sleep_id", "id"),
                                           tz_cols = c("w_timezone", "timezone")) {
  if (is.null(epochs) || is.null(summary)) stop("epochs and summary are required")
  ep <- tibble::as_tibble(epochs)
  sm <- tibble::as_tibble(summary)

  # Determine ID columns
  ep_id <- intersect(names(ep), id_cols)[1]
  sm_id <- intersect(names(sm), c("w_id", id_cols))[1]
  if (is.na(ep_id) || is.na(sm_id)) stop("No matching id columns found between epochs and summary")

  # Determine timezone column
  sm_tz <- intersect(names(sm), tz_cols)[1]
  if (is.na(sm_tz)) sm_tz <- NA_character_

  # Build a small lookup for timezone by id (unique)
  tz_lookup <- sm |>
    dplyr::select(dplyr::all_of(c(sm_id, sm_tz))) |>
    dplyr::distinct() |>
    dplyr::rename(.id = dplyr::all_of(sm_id), .tz = dplyr::all_of(sm_tz))

  # Join timezone into epochs
  ep2 <- ep |>
    dplyr::rename(.id = dplyr::all_of(ep_id)) |>
    dplyr::left_join(tz_lookup, by = ".id")

  # Compute datetime from unix timestamp using timezone
  if (!"timestamp" %in% names(ep2)) stop("epochs must have a 'timestamp' column of unix seconds")
  # Create POSIXct in UTC first
  ep2$datetime_utc <- as.POSIXct(as.numeric(ep2$timestamp), origin = "1970-01-01", tz = "UTC")
  # Create localized datetime string using the provided timezone (best-effort; format keeps tz label)
  has_tz <- !is.na(ep2$.tz) & ep2$.tz != ""
  ep2$datetime_local_chr <- NA_character_
  if (any(has_tz)) {
    ep2$datetime_local_chr[has_tz] <- mapply(
      function(dt, tz) format(dt, tz = tz, usetz = TRUE),
      ep2$datetime_utc[has_tz], ep2$.tz[has_tz]
    )
  }

  # Restore id column name
  names(ep2)[names(ep2) == ".id"] <- ep_id

  list(summary = sm, epoch = ep2, .mapping = list(epoch_id_col = ep_id, summary_id_col = sm_id, tz_col = sm_tz))
}
