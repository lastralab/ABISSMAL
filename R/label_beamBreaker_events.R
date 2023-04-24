#' @title label_beamBreaker_events
#' @description Use data from two pairs of beam breakers to label either entrance and exit movements (in or out of a nest container or nesting area).
#' 
#' @param threshold A single numeric value representing a temporal threshold in seconds that will be used to assign events across the beam breaker pairs to unique entrance or exit movements
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the raw data is saved across sensors inside the path above. For instance, "raw_combined".
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @return A .csv file with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the lead and rear beam breaker pairs, a unique label for the given event (e.g. entrance or exit), a unique numeric identifier for the given event, and information about the given data processing stage. Each row in the .csv file is a labeled event across the outer and inner beam breaker pairs that was identified using a given temporal threshold
#' 

# Make a function to label entrances and exits
label_beamBreaker_events <- function(threshold, sensor_id_col, timestamps_col, path, data_dir, out_dir, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Check that the temporal thresholds is numeric
  if(!is.numeric(threshold)){
    stop('The temporal threshold needs to be numeric (in seconds)')
  }
  
  # Create the directory for saving the labeled data files (if it doesn't exist)
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the pre-processed beam breaker data
  preproc_data <- read.csv(file.path(path, data_dir, "pre_processed_data_IRBB.csv")) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    # Drop columns that aren't needed here
    dplyr::select(-c("thin_threshold_s", "data_stage", "date_pre_processed"))
  
  # Check that the raw data is a data frame
  if(!is.data.frame(preproc_data)){
    stop('The pre-processed data needs to be a data frame')
  }
  
  # Ensure the timestamps are ordered, then calculate the time lags between the two beam breaker pairs. Here this is done using the leading differences between the beam breaker pairs to identify possible entrances and exits, respectively
  preproc_data2 <- preproc_data %>% 
    dplyr::arrange(timestamp_ms, desc = FALSE) %>%
    # Add unique row IDs to facilitate widening the data frame
    rowid_to_column() %>% 
    pivot_wider(
      names_from = !!sym(sensor_id_col),
      values_from = !!sym(timestamps_col)
    ) 
  
  diffs_df <- preproc_data2 %>% 
    dplyr::select(`Outer Beam Breaker`, `Inner Beam Breaker`) %>% 
    # The lead() call moves the timestamps for the given column one row index up, so that the difference calculation can be performed with the timestamps for the other column
    dplyr::mutate(
      # Here negative differences mean the outer pair triggered first
      outer_inner_diffs = `Outer Beam Breaker` - lead(`Inner Beam Breaker`, default = first(`Inner Beam Breaker`)),
      # Here negative differences mean the inner pair triggered first
      inner_outer_diffs = `Inner Beam Breaker` - lead(`Outer Beam Breaker`, default = first(`Outer Beam Breaker`))
    ) %>% 
    # Convert these differences to boolean based on the given threshold (in seconds)
    dplyr::mutate(
      binary_outer_inner_diffs = (abs((round(outer_inner_diffs, 2))) <= threshold),
      binary_inner_outer_diffs = (abs((round(inner_outer_diffs, 2))) <= threshold)
    ) 
  
  # Then make a data frame with the timestamps of the entrances and exits identified using the given temporal threshold
  
  # Find indices of entrances for outer beam breaker timestamps, or indices of exits for the inner beam breaker timestamps
  e1_ent <- which(diffs_df$binary_outer_inner_diffs)
  e1_exi <- which(diffs_df$binary_inner_outer_diffs)
  
  # Find indices of entrances for inner beam breaker timestamps, or indices of exits for the outer beam breaker timestamps by adding 1 to the indices above
  e2_ent <- e1_ent + 1
  e2_exi <- e1_exi + 1
  
  # Use these indices to filter the original pre-processed data frame so that the outer and inner beam breaker timestamps for entrances or exits are placed in the same row
  ee_df <- data.frame(
    outer_beamBreaker_timestamp = preproc_data2 %>% 
      slice(e1_ent) %>% 
      pull(`Outer Beam Breaker`),
    inner_beamBreaker_timestamp = preproc_data2 %>% 
      slice(e2_ent) %>% 
      pull(`Inner Beam Breaker`),
    type = "entrance"
  ) %>% 
    bind_rows(
      exi_df <- data.frame(
        outer_beamBreaker_timestamp = preproc_data2 %>% 
          slice(e2_exi) %>% 
          pull(`Outer Beam Breaker`),
        inner_beamBreaker_timestamp = preproc_data2 %>% 
          slice(e1_exi) %>% 
          pull(`Inner Beam Breaker`),
        type = "exit"
      )
    ) %>%
    dplyr::arrange(outer_beamBreaker_timestamp, desc = FALSE) %>% 
    rowid_to_column() %>% 
    dplyr::rename(
      `unique_beamBreaker_event` = rowid
    ) %>% 
    dplyr::mutate(
      data_stage = "labeling_events",
      temporal_threshold_s = threshold,
      date_labeled = paste(Sys.Date(), Sys.time(), sep = " ")
    ) %>% 
    # Add back the original metadata
    dplyr::inner_join(
      preproc_data %>%
        dplyr::select(-c("sensor_id")),
      by = c("outer_beamBreaker_timestamp" = "timestamp_ms")
    ) %>%
    dplyr::select(data_type, chamber_id, year, month, day, outer_beamBreaker_timestamp, inner_beamBreaker_timestamp, type, unique_beamBreaker_event, data_stage, temporal_threshold_s, date_labeled)
  
  # Save the labeled data
  write.csv(ee_df, file.path(path, out_dir, "labeled_beamBreaker_data.csv"), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
