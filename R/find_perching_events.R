#' @title find_perching_events
#' @description Detect perching events in the raw RFID or beam breaker data
#' 
#' @param file_nm A character string. This argument should be the name and extension of the .csv file that contains all of the raw RFID or beam breaker detections combined across dates. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the input data in the subsequent arguments.
#' @param threshold A single numeric value. This argument represents a temporal threshold in seconds that will be used to identify detections that occurred in close succession (e.g. within 1 or 2 seconds) as perching events.
#' @param run_length A single numeric value. This argument indicates the minimum number of consecutive detections used to label a perching event. The default setting is 2. 
#' @param sensor_id_col_nm A character string. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id").
#' @param timestamps_col_nm A character string. This argument is the column name that will be given to a new column containing timestamps for the given sensor in the correct format. These timestamps will be converted to POSIXct or POSIXt format with millisecond resolution (e.g. format = "%Y-%m-%d %H:%M:%OS", see `POSIXct_format` below).
#' @param PIT_tag_col_nm A character string. This argument is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna when RFID data is used as input (e.g. "PIT_tag_ID"). The default is NULL, since the function also accepts beam breaker data that does not contain PIT tag information.
#' @param rfid_label A character string. This argument is the label for the RFID sensor in the sensor_id_col_nm column in pre-processed data (e.g. "RFID"). The default is NULL, since the function also accepts beam breaker data
#' @param outer_irbb_label A character string. This argument is the label for the outer pair of beam breakers in the sensor_id_col_nm column in pre-processed data (e.g. "Outer Beam Breaker"). The default is NULL, since the function also accepts RFID data. This argument must be specified even when IRBB data from 1 pair of beam breakers is used as input.
#' @param inner_irbb_label A character string. This argument is the label for the inner pair of beam breakers in the sensor_id_col_nm column in pre-processed data (e.g. "Inner Beam Breaker"). The default is NULL, since the function also accepts RFID data. This argument must be specified even when IRBB data from 1 pair of beam breakers is used as input.
#' @param general_metadata_cols A character vector. This should be a string of the general metadata column names that will be carried through into the resulting spreadsheet. For this particular function, these metadata columns should be general to the experiment and not specific to individuals or dates. For instance: `c("chamber_id", "sensor_id")`. These columns will be added as the first columns in the resulting spreadsheet, in the same order in which they are provided.
#' @param path A character string. This argument should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022".
#' @param data_dir A character string. This argument should be the name of directory where the combined raw data per sensor type is saved inside the path above. For instance, "raw_combined".
#' @param out_dir A character string. This argument should be the name of a directory inside the path above specifying where the resulting .csv file of perching events should be saved (e.g. "processed"). This folder will be created as a new directory if it doesn't already exist.
#' @param out_file_prefix A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default prefix is "perching_events". The function will automatically add the sensor type of the data used as input to this default file name as a suffix before the .csv extension.
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS6" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#'
#' @details This function parses raw radio frequency identification (RFID) or infrared beam breaker data to identify perching events (e.g. periods of time when an individual was perched in the entrance of the nest container around which the movement sensors are mounted). When RFID data is used as input, this function identifies perching events for each unique passive integrated transponder (PIT) tag and date in the dataset. When beam breaker data is used as input, the function identifies perching events for each pair of beam breakers and date in the dataset. `find_perching_events` identifies runs of sensor detections separated by the given temporal threshold or less. The function then takes the first and last detection of each run and returns these timestamps as the start and end of each perching event.
#' 
#' @return `find_perching_events` returns a .csv file with all metadata columns in the original data frame, as well as the start and end timestamps of each perching period identified in the raw data per sensor type. When beam breaker data is used as input, the resulting spreadsheet contains perching events detected across both pairs of beam breakers. Each row in the resulting spreadsheet is a perching event. The function also returns information about parameters used for this data processing.

find_perching_events <- function(file_nm, threshold, run_length = 2, sensor_id_col_nm, timestamps_col_nm, PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols, path, data_dir, out_dir, out_file_prefix = "perching_events", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Get the user-specified values for each formal argument of the current function
  f_args <- getFunctionParameters()
  
  # Check that arguments that cannot ever be non-NULL are not NULL
  expect_nonNulls <- f_args[grep(paste(paste("^", c("file_nm", "sensor_id_col_nm", "timestamps_col_nm", "general_metadata_cols", "path", "data_dir", "out_dir", "out_file_prefix", "tz", "POSIXct_format"), "$", sep = ""), collapse = "|"), names(f_args))]
  
  invisible(sapply(1:length(expect_nonNulls), function(i){
    check_not_null(names(expect_nonNulls[i]), expect_nonNulls[[i]])
  }))
  
  # Check that the sensor suffix in the input file name is correct
  check_file_nm1(file_nm)
  
  # Check that the formal arguments that should be strings are strings
  expect_numeric <- c("threshold", "run_length")
  
  # Check that the formal arguments that should be NULL under certain conditions are NULL given the current user-specified arguments
  if(grepl("RFID", file_nm)){
    
    expect_null <- c("outer_irbb_label", "inner_irbb_label")
    
  } else if(grepl("IRBB", file_nm)){
    
    expect_null <- c("PIT_tag_col_nm", "rfid_label")
    
  }
  
  expect_nulls <- f_args[grep(paste(paste("^", expect_null, "$", sep = ""), collapse = "|"), names(f_args))]
  
  invisible(sapply(1:length(expect_nulls), function(i){
    check_null(names(expect_nulls[i]), expect_nulls[[i]])
  }))
  
  # Check that all formal arguments that cannot be NULL are not NULL:
  if(grepl("RFID", file_nm)){
    
    expect_nonNull <- c("PIT_tag_col_nm", "rfid_label")
    
  } else if(grepl("IRBB", file_nm)){
    
    expect_nonNull <- c("outer_irbb_label", "inner_irbb_label")
    
  }
  
  expect_nonNulls <- f_args[grep(paste(paste("^", expect_nonNull, "$", sep = ""), collapse = "|"), names(f_args))]
  
  invisible(sapply(1:length(expect_nonNulls), function(i){
    check_not_null(names(expect_nonNulls[i]), expect_nonNulls[[i]])
  }))
  
  expect_strings <- f_args[-grep(paste(paste("^", c(expect_numeric, expect_null), "$", sep = ""), collapse = "|"), names(f_args))]
  
  invisible(sapply(1:length(expect_strings), function(i){
    check_string(names(expect_strings[i]), expect_strings[[i]])
  }))
  
  # Check that the formal arguments that should be numeric are numeric
  invisible(sapply(1:length(expect_numeric), function(i){
    check_numeric(expect_numeric[i], f_args[[grep(paste(paste("^", expect_numeric[i], "$", sep = ""), collapse = "|"), names(f_args))]])
  }))
  
  # Check that each input directory exists
  check_dirs(path, data_dir)
  
  # Check that the input file exists in the input directory
  check_file(file.path(path, data_dir), file_nm)
  
  # Create the directory for saving the data file if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the raw data
  raw_data <- read.csv(file.path(path, data_dir, file_nm)) 
  
  # Check that this object is a data frame
  check_df_class(raw_data)
  
  # Check that the expected columns from formal arguments are found in the data
  expected_cols <- f_args[grep("col", names(f_args))]
  expected_cols <- expected_cols[-grep("time", names(expected_cols))]
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_fArgs_data_cols(expected_cols[[i]], raw_data)
  }))
  
  # Check that the expected columns from formal arguments do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_fArgs_cols_nas(expected_cols[[i]], raw_data)
  }))
  
  # Check that date-related columns are found in the data
  expected_cols <- c("original_timestamp", "year", "month", "day")
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_data_cols(expected_cols[i], raw_data)
  }))
  
  # Check that the date-related columns do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_cols_nas(expected_cols[i], raw_data)
  }))
  
  # Then use the year, month, and day columns to make a new timestamps column in the right format
  raw_data <- raw_data %>% 
    dplyr::mutate(
      timestamp_ms = as.POSIXct(paste(paste(year, month, day, sep = "-"), original_timestamp, sep = " "), tz = tz, format = POSIXct_format)
    ) %>% 
    # Drop columns that aren't needed here
    dplyr::select(-c("original_timestamp", "data_stage", "date_combined"))
  
  # Check that columns with timestamps are in the right format
  tstmps_cols <- f_args[grep("time", names(f_args))]
  
  invisible(sapply(1:length(tstmps_cols), function(i){
    check_tstmps_cols(tstmps_cols[[i]], raw_data, "%Y-%m-%d %H:%M:%OS6")
  }))
  
  # If RFID data is used as input, then group by PIT tag ID and date
  if(!is.null(rfid_label)){
    
    raw_data_grpd <- raw_data %>%
      dplyr::mutate(
        dates = paste(year, month, day, sep = "-")
      ) %>%
      group_by(!!sym(PIT_tag_col_nm), dates) %>% 
      dplyr::arrange(!!sym(timestamps_col_nm), .by_group = TRUE) %>% 
      # Make unique row indices within groups
      dplyr::mutate(
        group_row_id = row_number()
      ) %>% 
      nest()
    
    # If beam breaker data is used as input, then group by beam breaker pair and date
  } else if(!is.null(outer_irbb_label) | !is.null(outer_irbb_label)){
    
    raw_data_grpd <- raw_data %>%
      dplyr::mutate(
        dates = paste(year, month, day, sep = "-")
      ) %>%
      group_by(!!sym(sensor_id_col_nm), dates) %>% 
      dplyr::arrange(!!sym(timestamps_col_nm), .by_group = TRUE) %>% 
      # Make unique row indices within groups
      dplyr::mutate(
        group_row_id = row_number()
      ) %>% 
      nest()
    
  }
  
  # Find clusters of detections in the raw data that represent perching events
  perching_df <- raw_data_grpd %>% 
    dplyr::mutate(
      # Map over the nested data frames
      lags = map(
        .x = data,
        .f = ~ dplyr::mutate(.x,
                             shift = dplyr::lag(!!sym(timestamps_col_nm), default = first(!!sym(timestamps_col_nm)))
        ) %>% 
          # Convert differences to Boolean based on the thinning threshold to remove stretches of detection events very close together
          dplyr::mutate(
            diff = as.numeric(!!sym(timestamps_col_nm) - shift),
            # Taking anything less than or equal to the threshold,. The diff > 0 condition removes the first timestamp compared to itself
            binary_diff = (diff <= threshold & diff > 0)
          ) %>% 
          dplyr::select(all_of(timestamps_col_nm), shift, diff, binary_diff) 
      )
    ) %>% 
    
    # Make a data frame of the first and last indices of each run longer than the given run_length that contain temporal difference values below or equal to the given threshold
    dplyr::mutate(
      # Map over the nested data frames in lags
      lags_runs = map(
        .x = lags,
        .f = ~ dplyr::reframe(.x,
                              first_indices = cumsum(rle(binary_diff)[["lengths"]]) - (rle(binary_diff)[["lengths"]]),
                              last_indices = cumsum(rle(binary_diff)[["lengths"]]),
                              run_values = rle(binary_diff)[["values"]],
                              run_lengths = rle(binary_diff)[["lengths"]],
                              .groups = "keep"
        ) %>% 
          dplyr::filter(run_values & run_lengths >= run_length) %>% 
          ungroup()
      )
    ) %>% 
    # Get the unique perching events
    dplyr::mutate(
      # Map over the nested data frames in lags_runs
      perching = map(
        .x = lags_runs,
        .y = data,
        # For each unique PIT tag and date (RFID data), or each beam breaker pair and date (beam breaker data), retain the first and last indices of detections flagged as perching events
        # Use pmap_dfr to iterate over rows in each nested data frame, in which each row represents a unique perching event
        .f = ~ dplyr::select(.x, first_indices, last_indices) %>% 
          pmap_dfr(., function(first_indices, last_indices){
            
            tmp_perching <- data.frame(
              perching_start = .y[[1]] %>%
                dplyr::filter(group_row_id == first_indices) %>%
                pull(all_of(timestamps_col_nm)),
              perching_end = .y[[1]] %>%
                dplyr::filter(group_row_id == last_indices) %>%
                pull(all_of(timestamps_col_nm))
            ) 
            
            return(tmp_perching)
            
          })
      ) 
    ) %>%
    dplyr::select(-c(data, lags, lags_runs)) %>% 
    unnest(`cols` = c(perching)) %>%
    ungroup() %>% 
    dplyr::select(-c(dates))
  
  if(nrow(perching_df) > 0){
    
    # Add metadata and arrange columns and rows before writing this out
    perching_events <- perching_df %>% 
      dplyr::mutate(
        perching_duration_s = as.numeric(perching_end - perching_start),
        min_perching_run_length = run_length,
        threshold_s = threshold,
        data_stage = "pre-processing",
        date_preprocessed = paste(Sys.Date(), Sys.time(), sep = " ")
      )
    
    # Remove the sensor ID column (applies to beam breaker data)
    if(!is.null(outer_irbb_label) | !is.null(outer_irbb_label) & is.null(rfid_label)){
      
      perching_events <- perching_events %>% 
        dplyr::select(-c(all_of(sensor_id_col_nm))) 
      
    }
    
    # Add back general metadata columns from the original dataset
    perching_events <- perching_events %>% 
      dplyr::inner_join(
        raw_data %>%
          # Rename the timestamps column for the join with perching_start timestamps immediately below
          dplyr::rename(
            `perching_start` = !!sym(timestamps_col_nm)
          ) %>% 
          dplyr::select(all_of(general_metadata_cols), perching_start),
        by = c("perching_start")
      ) %>%
      dplyr::arrange(perching_start, desc = FALSE) %>% 
      rowid_to_column() %>% 
      dplyr::rename(
        `unique_perching_event` = "rowid"
      ) 
    
    # If RFID data is used as input, then add back metadata for the PIT tag IDs
    if(!is.null(rfid_label)){
      
      perching_events <- perching_events %>% 
        dplyr::select(all_of(general_metadata_cols), all_of(sensor_id_col_nm), all_of(PIT_tag_col_nm), perching_start, perching_end, perching_duration_s, unique_perching_event, min_perching_run_length, threshold_s, data_stage, date_preprocessed)
      
    } else if(!is.null(outer_irbb_label) | !is.null(inner_irbb_label)){
      
      perching_events <- perching_events %>% 
        dplyr::select(all_of(general_metadata_cols), all_of(sensor_id_col_nm), perching_start, perching_end, perching_duration_s, unique_perching_event, min_perching_run_length, threshold_s, data_stage, date_preprocessed)
      
    }
    
  } else {
    
    perching_events <- raw_data %>%
      dplyr::select(all_of(general_metadata_cols), year, month, day) %>% 
      distinct() %>% 
      dplyr::mutate(
        perching_start = NA,
        perching_end = NA,
        perching_duration_s = NA,
        unique_perching_event = NA,
        min_perching_run_length = run_length,
        threshold_s = threshold,
        data_stage = "pre-processing",
        date_preprocessed = paste(Sys.Date(), Sys.time(), sep = " ")
      ) %>% 
      dplyr::select(all_of(general_metadata_cols), all_of(sensor_id_col_nm), year, month, day, perching_start, perching_end, perching_duration_s, unique_perching_event, min_perching_run_length, threshold_s, data_stage, date_preprocessed)
    
  }
  
  # Name the output by sensor ID
  sensor_id <- strsplit(file_nm, split = "_")[[1]]
  sensor_id <- gsub(".csv", "", sensor_id[length(sensor_id)])
  
  out_file_nm_tmp <- paste(out_file_prefix, "_", sensor_id, ".csv", sep = "")
  
  write.csv(perching_events, file.path(path, out_dir, out_file_nm_tmp), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
