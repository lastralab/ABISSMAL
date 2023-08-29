#' @title preprocess_detections
#' @description Pre-process raw data for each movement sensor type
#' 
#' @param sensor A character string. This argument should contain the movement sensor type for which pre-processing will be performed, either "IRBB", "RFID", or "Video". The data for the two beam breaker pairs will be pre-processed separately here (see `group_col_nm` below).
#' @param timestamps_col A character string. This argument is the column name for the columns that contains timestamps for the given sensor. These timestamps must be in POSIXct or POSIXt format with millisecond resolution (e.g. format = "%Y-%m-%d %H:%M:%OS", see `POSIXct_format` below).
#' @param group_col_nm A character string. This argument is the column name for the column that contains values used to group the data before pre-processing. For RFID data, this column should contain the PIT tag identifiers, so that pre-processing is performed for each unique PIT tag. For beam breaker data, this column should be the unique beam breaker labels so that pre-processing is carried out separately for each beam breaker pair. The default is `NULL`, since this argument is not needed to process the video data
#' @param pixel_col_nm A character string. This argument is the column name for the column that contains the number of pixels that triggered each unique video recording event. The default is `NULL`, since this argument is not needed to process the RFID or beam breaker data.
#' @param thin_threshold A single numeric value. This argument is the temporal threshold in seconds that will be used to thin the raw data. The default is `NULL`, since this argument is not needed to pre-process the video data.
#' @param mode A character string. This argument determines how raw detections will be filtered during pre-processing. If set to `retain_first`, then only the first detection per cluster (in which a cluster is a run of detections separated by gaps less than or equal to the temporal threshold) will be retained and all other detections will be dropped. If set to `thin`, then every other detection will be dropped in a thinning approach, which yields a longer sequence of detections per cluster separated by more than the given temporal threshold.
#' @param pixel_threshold A single numeric value. This argument is a numeric threshold (number of pixels) that will be used to filter out video recording events with total pixels that changed below this threshold. The default is `NULL`, since this argument is not needed to process the RFID or beam breaker data.
#' @param path A character string. This should be the path on the local computer or external hard drive specifying where the data is saved across sensors for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the raw data is saved across sensors inside the path above. For instance, "raw_combined".
#' @param out_dir A character string. This should be the name of a directory within the path above specifying where the .csv file of pre-processed data should be saved for each type of movement sensor. For instance, "pre-processed". This folder will be created as a new directory if it doesn't already exist.
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS6" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details Pre-process the raw radio frequency identification (RFID) and beam breaker data by thinning, and pre-process the raw video data by the magnitude of movement detected. For the pre-processing by thinning, events detected by a single sensor are compared to each other (using a temporal offset), and adjacent events that occurred within a specified temporal threshold are filtered out. For RFID data, this data thinning is performed for each unique passive integrated transponder (PIT) tag in the dataset. For infrared beam breakers, the data thinning is carried out for each pair of beam breakers. For video data, this function filters out video recording timestamps with numbers of pixels that changed (e.g. the magnitude of movement) below a given threshold.
#' 
#' @return This function returns a spreadsheet in .csv format with the pre-processed detections per sensor and all metadata columns in the original data frame. Each spreadsheet also contains a column indicating the temporal threshold used for pre-processing by thinning (in seconds) for the RFID and beam breaker data, or a column indicating the threshold used to filter detections by the number of pixels that changed for the video data. Each row of this data frame is a pre-processed detection or movement event from the raw data collected by the given sensor. This function performs pre-processing in 1 of 2 ways: by either retaining only the first detection from a cluster of detections that occurred close in time, or thinning the cluster to return a sequence of detections per cluster separated by more than the given temporal threshold.


# sensor = "RFID"
# timestamps_col = "timestamp_ms"
# group_col_nm = "PIT_tag_ID"
# pixel_col_nm = NULL
# ths <- seq(0.5, 5, by = 0.5)
# x <- 4
# thin_threshold = ths[x]
# mode = "thin"
# pixel_threshold = NULL
# path = path
# data_dir = file.path(data_dir, "raw_combined")
# out_dir = file.path(data_dir, "processed")
# tz = "%Y-%m-%d %H:%M:%OS"
# POSIXct_format = "%Y-%m-%d %H:%M:%OS"

preprocess_detections <- function(sensor, timestamps_col, group_col_nm = NULL, pixel_col_nm = NULL, mode, thin_threshold = NULL, pixel_threshold = NULL, path, data_dir, out_dir, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Get the user-specified values for each formal argument of the current function
  f_args <- getFunctionParameters()
  
  # Check that the formal arguments that should not be NULL are not NULL under the right conditions (e.g. when processing RFID data)
  if(sensor == "RFID"){
    
    expect_null <- c("pixel_col_nm", "pixel_threshold")
    
  } else if(sensor == "IRBB"){
    
    expect_null <- c("group_col_nm", "pixel_col_nm", "pixel_threshold")
    
  } else if(sensor == "Video"){
    
    expect_null <- c("group_col_nm", "thin_threshold")
    
  }
  
  expect_nulls <- f_args[grep(paste(paste("^", expect_null, "$", sep = ""), collapse = "|"), names(f_args))]
  
  invisible(sapply(1:length(expect_nulls), function(i){
    check_null(expect_nulls[[i]])
  }))
  
  # Check that all formal arguments that should not be NULL were all specified
  expect_nonNulls <- f_args[-grep(paste(paste("^", expect_null, "$", sep = ""), collapse = "|"), names(f_args))]
  
  invisible(sapply(1:length(expect_nonNulls), function(i){
    check_defined(expect_nonNulls[[i]])
  }))
  
  # Check that the formal arguments that should be strings are strings
  if(grepl("RFID|IRBB", sensor)){
    
    expect_numeric <- c("thin_threshold")
    
  } else if(grepl("Video", sensor)){
    
    expect_numeric <- c("pixel_threshold")
    
  }
  
  expect_strings <- f_args[-grep(paste(paste("^", expect_numeric, "$", sep = ""), collapse = "|"), names(f_args))]
  
  # Remove any columns that should be NULL
  if(length(expect_null) > 0){
    
    expect_strings <- expect_strings[-grep(paste(paste("^", expect_null, "$", sep = ""), collapse = "|"), names(expect_strings))]
      
  }
  
  invisible(sapply(1:length(expect_strings), function(i){
    check_string(expect_strings[[i]])
  }))
  
  # Check that the formal arguments that should be numeric are numeric
  invisible(sapply(1:length(expect_numeric), function(i){
    check_numeric(f_args[[grep(paste(paste("^", expect_numeric[i], "$", sep = ""), collapse = "|"), names(f_args))]])
  }))
  
  # Check that the sensor argument was written correctly
  check_sensor_spelling(sensor)
  
  # Check that the input directory exists
  check_dirs(path, data_dir)
  
  # Check that the input file exists in the input directory
  check_file(file.path(path, data_dir), paste("combined_raw_data_", sensor, ".csv", sep = ""))
  
  # Create the directory for saving the pre-processed data files if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the combined raw data for the given sensor type
  raw_data <- read.csv(file.path(path, data_dir, paste("combined_raw_data_", sensor, ".csv", sep = ""))) %>% 
    # Also make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(paste(paste(year, month, day, sep = "-"), original_timestamp, sep = " "), tz = tz, format = POSIXct_format)
    ) %>% 
    # Drop columns that aren't needed here
    dplyr::select(-c("original_timestamp", "data_stage", "date_combined"))
  
  # Check that this object is a data frame
  check_df_class(raw_data)
  
  # Check that the expected columns from formal arguments are found in the data
  expected_cols <- f_args[grep("col", names(f_args))]
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_fArgs_data_cols(expected_cols[[i]], raw_data)
  }))
  
  # Check that the expected columns from formal arguments do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_fArgs_cols_nas(expected_cols[[i]], raw_data)
  }))
  
  # Check that date-related columns are found in the data
  expected_cols <- c("year", "month", "day")
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_data_cols(expected_cols[i], raw_data)
  }))
  
  # Check that the date-related columns do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_cols_nas(expected_cols[i], raw_data)
  }))
  
  # Check that columns with timestamps are in the right format
  tstmps_cols <- f_args[grep("time", names(f_args))]
  
  invisible(sapply(1:length(tstmps_cols), function(i){
    check_tstmps_cols(tstmps_cols[[i]], raw_data, "%Y-%m-%d %H:%M:%OS6")
  }))
  
  if(grepl("RFID|IRBB", sensor)){
    
    # Initialize column names that will be shared across sensors, in the order in which they'll appear in the .csv file 
    col_nms <- c("data_type", "chamber_id", "year", "month", "day", timestamps_col, group_col_nm)
    
    if(!is.null(group_col_nm)){
      
      tmp_df <- raw_data %>%
        group_by(!!sym(group_col_nm)) %>%
        dplyr::arrange(!!sym(timestamps_col), .by_group = TRUE) %>% 
        # Make unique row indices within groups
        dplyr::mutate(
          group_row_id = row_number()
        ) %>% 
        dplyr::rename(
          `group_col` = all_of(group_col_nm)
        )
      
    } else if(is.null(group_col_nm)){
      
      tmp_df <- raw_data %>% 
        dplyr::arrange(!!sym(timestamps_col)) %>% 
        # Make unique row indices with the same column name as the group indices above
        dplyr::mutate(
          group_row_id = row_number(),
          group_col = NA
        )
      
    }
    
    # If the group_col is specified, then the lags are calculated per group in the grouped data frame
    lags <- tmp_df %>% 
      dplyr::mutate(
        shift = dplyr::lag(!!sym(timestamps_col), default = first(!!sym(timestamps_col)))
      ) %>% 
      # Convert differences to boolean based on the thinning threshold to be able to remove stretches of detection events very close together
      dplyr::mutate(
        diff = floor(!!sym(timestamps_col) - shift),
        diff = as.numeric(diff),
        binary_diff = (diff > thin_threshold & diff > 0)
      ) 

    if(!is.null(group_col_nm)){
      
      lags <- lags %>% 
        dplyr::select(!!sym(timestamps_col), group_col, diff, binary_diff) 
      
    } else {
      
      lags <- lags %>% 
        dplyr::select(!!sym(timestamps_col), diff, binary_diff) 
      
    }
    
    # Nest by each group, do the rle calculations and removing indices, then recombine
    lags_runs <- lags %>% 
      dplyr::reframe(
        run_indices = cumsum(rle(binary_diff)[["lengths"]]),
        run_values = rle(binary_diff)[["values"]],
        run_lengths = rle(binary_diff)[["lengths"]]
      ) %>% 
      dplyr::filter(!run_values) 
    
    if(!is.null(group_col_nm)){
      
      if(mode == "retain_first"){
       
        lags_runs2 <- lags_runs %>% 
          dplyr::select(group_col, run_values, run_lengths, run_indices) %>% 
          pmap_dfr(., function(group_col, run_values, run_lengths, run_indices){
            # In the runs of FALSE values (e.g. cluster), remove all including the first index. Should work for all runs with length == 1 or > 1
            return(
              data.frame(
                group_col = group_col,
                rem_indices = seq((run_indices - (run_lengths - 1)), run_indices, 1)
              )
            )
          }) %>% 
          # Make sure to drop the first index per group, since these first observations should be retained
          dplyr::filter(rem_indices != 1)
      
      } else if(mode == "thin"){
        
        lags_runs2 <- lags_runs %>% 
          dplyr::select(group_col, run_values, run_lengths, run_indices) %>% 
          pmap_dfr(., function(group_col, run_values, run_lengths, run_indices){
            # For each run of FALSE values (e.g. cluster), retain every other detection (starting with the first detection) in order to thin the cluster
            
            if(run_indices == run_lengths & run_lengths %% 2 != 0){
              
              rem_indices <- seq((run_indices - (run_lengths - 2)), run_indices - 1, 2)
              
            } else if(run_indices == run_lengths & run_lengths %% 2 == 0){
              
              rem_indices <- seq((run_indices - (run_lengths - 2)), run_indices, 2)
              
            } else if(run_indices != run_lengths & length(1:run_indices) %% 2 != 0){
              
              # If the total number of indices is odd, then make sure the last index is flagged for removal
              rem_indices <- seq((run_indices - (run_lengths - 1)), run_indices - 1, 2)
              
            } else if(run_indices != run_lengths & length(1:run_indices) %% 2 == 0){
              
              # If the total number of indices is even, then make sure the last index will be retained
              rem_indices <- seq((run_indices - (run_lengths - 1)), run_indices, 2)
              
            }
            
            return(
              data.frame(
                group_col = group_col,
                rem_indices = rem_indices
              )
            )
          }) %>% 
          # Make sure to drop the first index per group, since these first observations should be retained
          dplyr::filter(rem_indices != 1)
        
      }

      # Per group, remove the indices that represent the detections that are too close together
      filt_df <- tmp_df %>% 
        nest() %>%
        # Filter each nested data frame by the correct row indices per group
        dplyr::mutate(
          filtered = map(.x = data, .f = ~ dplyr::filter(.x, !group_row_id %in% lags_runs2$rem_indices[group_col == lags_runs2$group_col]))
        ) %>% 
        # Return the filtered data frames as a single data frame
        unnest(`cols` = c(filtered)) %>% 
        dplyr::select(-c(data)) %>% 
        ungroup() %>% 
        # Make sure to add a column with the temporal threshold used, as well as data stage and date pre-processed
        dplyr::mutate(
          thin_threshold_s = thin_threshold,
          data_stage = "pre-processed",
          date_pre_processed = paste(Sys.Date(), Sys.time(), sep = " ")
        )
      
      # Rename the group column back to its original name
      names(filt_df)[grep("group_col", names(filt_df))] <- group_col_nm
      
      # Return the shared columns in the order specified above, and any additional metadata columns per sensor afterwards
      filt_df2 <- filt_df %>% 
        dplyr::select(-c("group_row_id")) %>% 
        dplyr::select(
          col_nms, names(.)[-grep(paste(paste("^", col_nms, "$", sep = ""), collapse = "|"), names(.))], "data_stage", "date_pre_processed"
        )
      
    } else {
      
      lags_runs2 <- lags_runs %>% 
        dplyr::select(run_values, run_lengths, run_indices) %>% 
        pmap_dfr(., function(run_values, run_lengths, run_indices){
          
          return(
            data.frame(
              rem_indices = seq((run_indices - (run_lengths - 1)), run_indices, 1)
            )
          )
        }) %>% 
        dplyr::filter(rem_indices != 1)
      
      
      filt_df <- tmp_df %>% 
        # Filter the data frame by the correct row indices
        dplyr::filter(!group_row_id %in% lags_runs2$rem_indices) %>% 
        # Remove the grouping column
        dplyr::select(-c(group_col)) %>% 
        # Make sure to add a column with the temporal threshold used, as well as data stage and date pre-processed
        dplyr::mutate(
          thin_threshold_s = thin_threshold,
          data_stage = "pre-processed",
          date_pre_processed = paste(Sys.Date(), Sys.time(), sep = " ")
        )
      
      # Return the shared columns in the order specified above, and any additional metadata columns per sensor afterwards
      filt_df2 <- filt_df %>% 
        dplyr::select(-c("group_row_id")) %>% 
        dplyr::select(
          col_nms, names(.)[-grep(paste(paste("^", col_nms, "$", sep = ""), collapse = "|"), names(.))], "data_stage", "date_pre_processed"
        )
      
    }
    
  } else if(grepl("Video", sensor)){
    
    # Initialize column names that will be shared across sensors, in the order in which they'll appear in the .csv file 
    col_nms <- c("data_type", "chamber_id", "year", "month", "day", timestamps_col, pixel_col_nm)
    
    filt_df <- raw_data %>%
      dplyr::filter(!!sym(pixel_col_nm) >= pixel_threshold) %>% 
      # Make sure to add a column with the pixel threshold used, as well as data stage and date pre-processed
      dplyr::mutate(
        pixel_threshold = pixel_threshold,
        data_stage = "pre-processed",
        date_pre_processed = paste(Sys.Date(), Sys.time(), sep = " ")
      )
    
    # Return the shared columns in the order specified above, and any additional metadata columns per sensor afterwards
    filt_df2 <- filt_df %>% 
      dplyr::select(
        all_of(col_nms), names(.)[-grep(paste(paste("^", col_nms, "$", sep = ""), collapse = "|"), names(.))], "data_stage", "date_pre_processed"
      )
    
  }
  
  # Save the pre-processed data for each sensor in the given setup
  write.csv(filt_df2, file.path(path, out_dir, paste("pre_processed_data_", sensor, ".csv", sep = "")), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
