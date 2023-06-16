#' @title find_rfid_perching_events
#' @description Use the raw radio frequency identification (RFID) data to identify perching events (e.g. periods of time when an individual was perched on the RFID antenna). This function is performed for each unique passive integrated transponder (PIT) tag in the dataset. The function identifies runs of RFID detections separated by the given temporal threshold or less, then takes the first and last detection of each run and returns these timestamps as the start and end of each perching period. Note that unlike the pre-processing function, this function groups the data frame not only by PIT tag ID but also by date to avoid artificially long perching periods.
#' 
#' @param rfid_file_nm A character string. This should be the name of the file that contains all of the pre-processed RFID detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the RFID data in the subsequent arguments
#' #' @param threshold A single numeric value. This represents a temporal threshold in seconds that will be used to identify RFID detections that occurred in close succession (e.g. within 1 or 2 seconds) as perching events
#' @param run_length A single numeric value. This argument indicates the minimum number of consecutive RFID detections used to label a perching event. Default is 2 
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param PIT_tag_col A character value. This is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID")
#' @param general_metadata_cols A character vector. This should be a string of the general metadata column names that will be carried through into the resulting data frame representing the integrated data. For this particular function, these metadata columns should be generally applicable across PIT tag IDs and dates. For instance: cc("chamber_id", "sensor_id"). These columns will be added as the first columns in the integrated data frame, in the same order in which they are provided
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param rfid_dir A character string. This should be the name of directory where the pre-processed RFID data is saved across sensors inside the path above. For instance, "pre-processed"
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param out_file_nm A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default is "perching_events.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information
#'
#' @details TKTK
#' 
#' @return A data frame object with all metadata columns in the original data frame, as well as columns indicating the timestamp of the start of the perching period identified, the end of the given perching period, and the temporal threshold used for pre-processing (in seconds). In other words, each row is a perching period
#' 

find_rfid_perching_events <- function(rfid_file_nm, threshold, run_length = 2, sensor_id_col, timestamps_col, PIT_tag_col, general_metadata_cols, path, rfid_dir, out_dir, out_file_nm = "perching_events.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Get the formal arguments from the current function
  # TKTK try substituting the function name with: match.call()[[1]]
  f_args <- methods::formalArgs(find_rfid_perching_events)
  
  # Check that the formal arguments were all specified
  invisible(sapply(1:length(f_args), function(i){
    check_defined(f_args[i])
  }))
  
  # Check that the formal arguments are not NULL
  invisible(sapply(1:length(f_args), function(i){
    check_null(f_args[i])
  }))
  
  # Check that the formal arguments that should be strings are strings
  expect_numeric <- c("threshold", "run_length")
  expect_strings <- f_args[-grep(paste(paste("^", expect_numeric, "$", sep = ""), collapse = "|"), f_args)]
  
  invisible(sapply(1:length(expect_strings), function(i){
    check_string(expect_strings[i])
  }))
  
  # Check that the formal arguments that should be numeric are numeric
  invisible(sapply(1:length(expect_numeric), function(i){
    check_numeric(expect_numeric[i])
  }))
  
  # Check that each input directory exists
  check_dirs(data_path, rfid_dir)
  
  # Check that the input file exists in the input directory
  check_file(file.path(path, rfid_dir), rfid_file_nm)
  
  # Create the directory for saving the data file if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the pre-processed RFID data
  preproc_rfid <- read.csv(file.path(path, rfid_dir, rfid_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) 
  
  # Check that this object is a data frame
  check_df_class(preproc_rfid)
  
  # Check that the expected columns from formal arguments are found in the data
  expected_cols <- f_args[grep("col", f_args)]
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_fArgs_data_cols(expected_cols[i], preproc_rfid)
  }))
  
  # Check that the expected columns from formal arguments do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_fArgs_cols_nas(expected_cols[i], preproc_rfid)
  }))
  
  # Check that date-related columns are found in the data
  expected_cols <- c("year", "month", "day")
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_data_cols(expected_cols[i], preproc_rfid)
  }))
  
  # Check that the date-related columns do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_cols_nas(expected_cols[i], preproc_rfid)
  }))
  
  # Check that columns with timestamps are in the right format
  tstmps_cols <- f_args[grep("time", f_args)]
  
  invisible(sapply(1:length(tstmps_cols), function(i){
    check_tstmps_cols(tstmps_cols[i], preproc_rfid, "%Y-%m-%d %H:%M:%OS6")
  }))
  
  # Group the RFID data frame by PIT tag ID and day. Otherwise the logic below ends up including the last event of a day and the first of the next day as the start and end indices, which leads to strangely long perching periods sometimes
  perching_df <- preproc_rfid %>%
    dplyr::mutate(
      dates = paste(year, month, day, sep = "-")
    ) %>%
    group_by(!!sym(PIT_tag_col), dates) %>% 
    dplyr::arrange(!!sym(timestamps_col), .by_group = TRUE) %>% 
    # Make unique row indices within groups
    dplyr::mutate(
      group_row_id = row_number()
    ) %>% 
    nest() %>% 
    dplyr::mutate(
      # Map over the data frames nested by PIT tag IDs
      lags = map(
        .x = data,
        .f = ~ dplyr::mutate(.x,
                             shift = dplyr::lag(!!sym(timestamps_col), default = first(!!sym(timestamps_col)))
        ) %>% 
          # Convert differences to Boolean based on the thinning threshold to be able to remove stretches of detection events very close together
          dplyr::mutate(
            diff = as.numeric(floor(!!sym(timestamps_col) - shift)),
            # Taking anything less than or equal to the threshold, see previous RFID pre-processing. The diff > 0 condition should remove the first timestamp compared to itself, which should in turn make it no longer necessary to correct the timestamp indices
            binary_diff = (diff <= threshold & diff > 0)
          ) %>% 
          dplyr::select(all_of(timestamps_col), diff, binary_diff) 
      )
    ) %>% 
    # Make a data frame of the first and last indices of each run longer than the given run_length that contain temporal difference values below or equal to the given threshold
    dplyr::mutate(
      # Map over the data frames nested by PIT tag IDs
      lags_runs = map(
        .x = lags,
        .f = ~ dplyr::summarise(.x,
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
      # Map over the data frames nested by PIT tag IDs
      perching = map(
        .x = lags_runs,
        .y = data,
        # For each unique PIT tag and date, retain the first and last indices of RFID detections flagged as perching events
        # Use pmap_dfr to iterate over rows in each nested data frame, in which each row represents a unique perching event by date
        .f = ~ dplyr::select(.x, first_indices, last_indices) %>% 
          pmap_dfr(., function(first_indices, last_indices){
            
            tmp_perching <- data.frame(
              perching_start = .y[[1]] %>%
                dplyr::filter(group_row_id == first_indices) %>%
                pull(all_of(timestamps_col)),
              perching_end = .y[[1]] %>%
                dplyr::filter(group_row_id == last_indices) %>%
                pull(all_of(timestamps_col))
            ) 
            
            return(tmp_perching)
            
          })
      ) 
    ) %>%
    dplyr::select(-c(data, lags, lags_runs)) %>% 
    unnest(`cols` = c(perching)) %>%
    ungroup() %>% 
    tidyr::separate(
      col = "dates", into = c("year", "month", "day"), sep = "-"
    )
  
  if(nrow(perching_df) > 0){
    
    # Add metadata and arrange columns and rows before writing this out
    perching_events <- perching_df %>% 
      dplyr::mutate(
        perching_duration_s = as.numeric(perching_end - perching_start),
        min_perching_run_length = run_length,
        threshold = threshold,
        data_stage = "pre-processing",
        date_preprocessed = paste(Sys.Date(), Sys.time(), sep = " ")
      ) %>% 
      # Add back general metadata columns from the original RFID dataset
      dplyr::inner_join(
        preproc_rfid %>%
          # Rename the timestamps column for the join with perching_start timestamps immediately below
          dplyr::rename(
            `perching_start` = !!sym(timestamps_col)
          ) %>% 
          dplyr::select(all_of(general_metadata_cols), perching_start),
        by = c("perching_start")
      ) %>%
      dplyr::arrange(perching_start, desc = FALSE) %>% 
      rowid_to_column() %>% 
      dplyr::rename(
        `unique_perching_event` = "rowid"
      ) %>% 
      dplyr::select(all_of(general_metadata_cols), year, month, day, all_of(PIT_tag_col), perching_start, perching_end, perching_duration_s, unique_perching_event, min_perching_run_length, threshold, data_stage, date_preprocessed)
    
  } else {
    
    perching_events <- preproc_rfid %>%
      dplyr::select(all_of(general_metadata_cols), year, month, day, all_of(PIT_tag_col)) %>% 
      distinct() %>% 
      dplyr::mutate(
        perching_start = NA,
        perching_end = NA,
        perching_duration_s = NA,
        unique_perching_event = NA,
        min_perching_run_length = run_length,
        threshold = threshold,
        data_stage = "pre-processing",
        date_preprocessed = paste(Sys.Date(), Sys.time(), sep = " ")
      ) %>% 
      dplyr::select(all_of(general_metadata_cols), year, month, day, all_of(PIT_tag_col), perching_start, perching_end, perching_duration_s, unique_perching_event, min_perching_run_length, threshold, data_stage, date_preprocessed)
    
  }
  
  write.csv(perching_events, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
