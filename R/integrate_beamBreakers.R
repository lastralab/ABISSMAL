#' @title integrate_beamBreakers
#' @description Use data from two pairs of beam breakers to label either entrance and exit movements (in or out of a nest container or nesting area).
#' 
#' @param irbb_file_nm A character string. This should be the name of the file that contains all of the pre-processed and labeled beam breaker detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the beam breaker data in the subsequent arguments
#' @param threshold A single numeric value representing a temporal threshold in seconds that will be used to assign events across the beam breaker pairs to unique entrance or exit movements
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param outer_irbb_nm A character value. The label used for the outer pair of beam breakers (e.g. the first pair of beam breakers that an animal encounters when moving into a nest container or area). This should be a value in the column named `sensor_id_col`
#' @param inner_irbb_nm A character value. The label used for the inner pair of beam breakers (e.g. the second pair of beam breakers that an individual encounters when moving into a nest container or area). This should be a value in the column named `sensor_id_col`
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the raw data is saved across sensors inside the path above. For instance, "raw_combined".
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @return A .csv file with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the lead and rear beam breaker pairs, a unique label for the given event (e.g. entrance or exit), a unique numeric identifier for the given event, and information about the given data processing stage. Each row in the .csv file is a labeled event across the outer and inner beam breaker pairs that was identified using a given temporal threshold
#' 

# Make a function to integrate between beam breaker pairs to infer entrance and exit movements
integrate_beamBreakers <- function(irbb_file_nm, threshold, sensor_id_col, timestamps_col, outer_irbb_nm, inner_irbb_nm, path, data_dir, out_dir, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
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
  preproc_data <- read.csv(file.path(path, data_dir, irbb_file_nm)) %>% 
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
    dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm)) %>% 
    # The lead() call moves the timestamps for the given column one row index up, so that the difference calculation can be performed with the timestamps for the other column
    dplyr::mutate(
      leading_outer = lead(!!sym(outer_irbb_nm), default = first(!!sym(outer_irbb_nm))),
      leading_inner = lead(!!sym(inner_irbb_nm), default = first(!!sym(inner_irbb_nm)))                     
    ) %>% 
    dplyr::mutate(
      # Here negative differences mean the outer pair triggered first
      outer_inner_diffs = !!sym(outer_irbb_nm) - leading_inner,
      # Here negative differences mean the inner pair triggered first
      inner_outer_diffs = !!sym(inner_irbb_nm) - leading_outer
    ) %>% 
    # Convert these differences to boolean based on the given threshold (in seconds)
    dplyr::mutate(
      binary_outer_inner_diffs = (abs((round(outer_inner_diffs, 2))) <= threshold),
      binary_inner_outer_diffs = (abs((round(inner_outer_diffs, 2))) <= threshold)
    ) 
  
  # Make data frames with the timestamps of the entrances and exits identified using the given temporal threshold
  entrances_df <- diffs_df %>%
    dplyr::mutate(
      !!inner_irbb_nm := leading_inner,
      diffs = outer_inner_diffs
    ) %>% 
    # Add back metadata about the beam breaker event labels
    dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm), diffs, binary_outer_inner_diffs) %>% 
    # Then filter for beam breaker events that match the logic for entrances
    dplyr::filter(
      binary_outer_inner_diffs
    ) %>% 
    dplyr::mutate(
      direction_inferred = "entrance"
    ) %>% 
    dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm), diffs, direction_inferred)
  
  # Then find exit events
  exits_df <- diffs_df %>%
    # Make sure to remove any beam breaker events already identified as entrances from the pool of possible candidates
    dplyr::filter(
      !(leading_outer %in% entrances_df[[outer_irbb_nm]]) &
        !(!!sym(inner_irbb_nm) %in% entrances_df[[inner_irbb_nm]])
    ) %>% 
    dplyr::mutate(
      !!outer_irbb_nm := leading_outer,
      diffs = inner_outer_diffs
    ) %>% 
    dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm), diffs, binary_inner_outer_diffs) %>% 
    # Then filter for beam breaker events that match the logic for exits
    dplyr::filter(
      binary_inner_outer_diffs
    ) %>% 
    dplyr::mutate(
      direction_inferred = "exit"
    ) %>% 
    dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm), diffs, direction_inferred)

  # Join the data frames and check for duplicated timestamps once more across the entrance and exit events
  ee_df <- entrances_df %>%
    bind_rows(
      exits_df
    ) %>% 
    dplyr::arrange(!!sym(outer_irbb_nm), desc = FALSE) %>% 
    rowid_to_column() %>% 
    dplyr::rename(
      `unique_entranceExit` = rowid
    ) %>% 
    dplyr::mutate(
      data_stage = "integration",
      temporal_threshold_s = threshold,
      date_labeled = paste(Sys.Date(), Sys.time(), sep = " ")
    ) %>% 
    # Rename the outer beam breaker column for the metadata join below
    dplyr::rename(
      `timestamp_ms` = !!sym(outer_irbb_nm)
    ) %>% 
    # Add back the original metadata (shared between the beam breaker pairs)
    dplyr::inner_join(
      preproc_data %>%
        dplyr::select(-c("sensor_id")),
      by = "timestamp_ms"
    ) %>%
    # Redo the renaming the outer beam breaker column
    dplyr::rename(
      !!outer_irbb_nm := "timestamp_ms"
    ) %>% 
    # Rename the beam breaker columns once more to remove spaces
    dplyr::rename(
      `Outer_beam_breaker` = !!sym(outer_irbb_nm),
      `Inner_beam_breaker` = !!sym(inner_irbb_nm)
    ) %>% 
    dplyr::select(data_type, chamber_id, year, month, day, Outer_beam_breaker, Inner_beam_breaker, direction_inferred, unique_entranceExit, data_stage, temporal_threshold_s, date_labeled) %>% 
    dplyr::arrange(all_of(outer_irbb_nm), desc = FALSE)
  
  write.csv(ee_df, file.path(path, out_dir, "integrated_beamBreaker_data.csv"), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
