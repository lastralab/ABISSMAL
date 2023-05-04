#' @title find_rfid_perching_events
#' @description Use the raw radio frequency identification (RFID) data to identify perching events (e.g. periods of time when an individual was perched on the RFID antenna). This function is performed for each unique passive integrated transponder (PIT) tag in the dataset. The function identifies runs of RFID detections separated by the given temporal threshold or less, then takes the first and last detection of each run and returns these timestamps as the start and end of each perching period. Note that unlike the pre-processing function, this function groups the data frame not only by PIT tag ID but also by date to avoid artificially long perching periods.
#' 
#' @param rfid_file_nm A character string. This should be the name of the file that contains all of the pre-processed RFID detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the RFID data in the subsequent arguments
#' #' @param threshold A single numeric value. This represents a temporal threshold in seconds that will be used to identify RFID detections that occurred in close succession (e.g. within 1 or 2 seconds) as perching events
#' @param run_length A single numeric value. This argument indicates the minimum number of consecutive RFID detections used to label a perching event. Default is 2 
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param PIT_tag_col A character value. This is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID")
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param rfid_dir A character string. This should be the name of directory where the pre-processed RFID data is saved across sensors inside the path above. For instance, "pre-processed"
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param out_file_nm A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default is "perching_events.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information
#'
#' @return A data frame object with all metadata columns in the original data frame, as well as columns indicating the timestamp of the start of the perching period identified, the end of the given perching period, and the temporal threshold used for pre-processing (in seconds). In other words, each row is a perching period

rfid_file_nm <- "pre_processed_data_RFID.csv"
threshold <- 2
run_length <- 2
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
rfid_dir <- "pre_processed"
out_dir <- "pre_processed"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"


find_rfid_perching_events <- function(rfid_file_nm, threshold, run_length = 2, sensor_id_col, timestamps_col, PIT_tag_col, path, rfid_dir, out_dir, out_file_nm = "perching_events.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # TKTK need to check these carefully and across functions too
  # Check that the temporal threshold is a number
  if(!is.numeric(threshold)){
    stop('The temporal threshold needs to be numeric (in seconds)')
  }
  # 
  # # Check that the input dataset has the column of RFID events
  # if(any(is.null(timestamps_col) | !timestamps_col %in% names(df))){
  #   stop('The column specified in `timestamps_col` does not exist')
  # }
  # 
  # # Check that the input dataset has the PIT tag ID column, and does not have NAs
  # if(any(is.null(tag_id_col_nm) | !tag_id_col_nm %in% names(df) | any(is.na(df[[tag_id_col_nm]])))){
  # stop('The column specified in `tag_id_col_nm` does not exist or has NA values')
  # }
  # 
  # # Check that the year, month, and day columns are also present in the data frame, and do not have NAs
  # if(any(!"year" %in% names(df) | !"month" %in% names(df) | !"day" %in% names(df) | any(is.na(df[["year"]])) | any(is.na(df[["month"]])) | any(is.na(df[["day"]])))){
  #   stop('The data frame is missing columns `year`, `month`, or `day`, or there are NA values in one of these columns')
  # }
  # 
  # # Check that the timestamps are in the right format. This conditional also catches NAs in the RFID timestamps
  # if(any(is.na(as.POSIXct(df[[timestamps_col]], format = "%Y-%m-%d %H:%M:%OS6")))){
  #   stop('One or more timestamps are in the wrong format (need to be in POSIXct or POSIXt format, like %Y-%m-%d %H:%M:%OS6')
  # }
  
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
  
  # Look for perching events by PIT tag ID and day. Otherwise the logic below ends up including the last event of a day and the first of the next day as the start and end indices, which leads to strangely long perching periods sometimes
  # Where is the grouping by PIT tag??? I think not grouping by this and adding the tags back later wasn't done well...
  tmp_df <- preproc_rfid %>%
    dplyr::mutate(
      date = paste(year, month, day, sep = " - ")
    ) %>%
    group_by(date) %>%
    dplyr::arrange(!!sym(timestamps_col), .by_group = TRUE) %>% 
    # Make unique row indices within groups
    dplyr::mutate(
      group_row_id = row_number()
    ) 

  # The lags are calculated per group in the grouped data frame
  lags <- tmp_df %>% 
    dplyr::mutate(
      shift = dplyr::lag(!!sym(timestamps_col), default = first(!!sym(timestamps_col)))
    ) %>% 
    # Convert differences to boolean based on a threshold to be able to remove stretches of detection events very close together
    dplyr::mutate(
      diff = floor(!!sym(timestamps_col) - shift),
      diff = as.numeric(diff),
      binary_diff = (diff <= threshold) # Taking anything less than or equal to the threshold, see previous RFID pre-processing
    ) %>% 
    dplyr::select(date, all_of(timestamps_col), diff, binary_diff) %>% 
    dplyr::rename(
      `dates` = "date"
    )
  
  # Make a data frame of the first and last indices of each run longer than 2 events that contain values below or equal to the given threshold
  lags_runs <- lags %>% 
    dplyr::summarise(
      first_indices = cumsum(rle(binary_diff)[["lengths"]]) - (rle(binary_diff)[["lengths"]]),
      last_indices = cumsum(rle(binary_diff)[["lengths"]]),
      run_values = rle(binary_diff)[["values"]],
      run_lengths = rle(binary_diff)[["lengths"]]
    ) %>% 
    dplyr::filter(run_values & run_lengths >= run_length) %>% 
    ungroup() %>% 
    # If a first index is stored as 0, then add 1 to restore this first index
    dplyr::mutate(
      first_indices = ifelse(first_indices == 0, first_indices + 1, first_indices)
    )
  
  if(nrow(lags_runs) > 0){
    
    # Per group, retain the first and last indices of RFID detections flagged as perching events
    # For some reason nesting and subsequent filtering doesn't work with 2 group variables, even when these are pasted together (in an earlier version of the function), so I used pmap_dfr to iterate over rows in lags_runs instead
    filt_df <- lags_runs %>%
      dplyr::select(dates, first_indices, last_indices) %>% 
      pmap_dfr(., function(dates, first_indices, last_indices){
        
        starts <- tmp_df %>%
          ungroup() %>%
          dplyr::filter(date == dates) %>%
          slice(c(first_indices)) %>%
          pull(all_of(timestamps_col))
        
        ends <- tmp_df %>%
          ungroup() %>%
          dplyr::filter(date == dates) %>%
          slice(c(last_indices)) %>%
          pull(all_of(timestamps_col))
        
        return(data.frame(
          dates = dates,
          start = starts,
          end = ends,
          duration_seconds = as.numeric(ends - starts),
          event_type = "perching",
          run_length = run_length
        ))
        
      }) %>% 
      as_tibble() %>% 
      # Make sure to add a column with the temporal threshold used, and add back the tag ID column
      dplyr::mutate(
        temporal_threshold_seconds = threshold,
        PIT_tag_ID = df %>% 
          pull(!!sym(tag_id_col_nm)) %>% 
          unique()
      ) %>% 
      dplyr::select(
        dates,
        PIT_tag_ID,
        start,
        end,
        duration_seconds,
        event_type,
        temporal_threshold_seconds,
        run_length
      )
    
  } else {
    
    filt_df <- data.frame(
      dates = unique(tmp_df$date),
      PIT_tag_ID = df %>% 
        pull(!!sym(tag_id_col_nm)) %>% 
        unique(),
      start = NA,
      end = NA,
      duration_seconds = NA,
      event_type = NA,
      temporal_threshold_seconds = threshold,
      run_length = run_length
    )
    
  }
  
  write.csv(filt_df, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
