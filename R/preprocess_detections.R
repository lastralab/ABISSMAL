#' @title preprocess_detections
#' @description Pre-process raw radio frequency identification (RFID) and beam breaker data by thinning, and pre-process video data by the magnitude of movement. For the pre-processing by thinning, events detected by a single sensor are compared to each other, and adjacent events that occurred within a specified temporal threshold are filtered out. For RFID data, this data thinning is performed for each unique passive integrated transponder (PIT) tag in the dataset. For infrared beam breakers, the data thinning is carried out for each pair of beam breakers. For pre-processing video data, this function can filter out video recording timestamps with numbers of pixels that changed (e.g. the magnitude of movement) below a given threshold.
#' 
#' @param sensor A character vector of length 1. This vector should contain the sensor type for which pre-processing will be performed, either "IRBB", "RFID", or "Video". Note that if there are multiple sensors for a given type (e.g. two pairs of beam breakers), this data will travel together in the .csv file of pre-processed data.
#' 
#' @param detection_col_nm A string with the column name for the detection timestamps collected by the given sensor. These timestamps must be in POSIXct or POSIXt format with millisecond resolution (e.g. format = "%Y-%m-%d %H:%M:%OS")
#' @param group_col_nm A string with the column name that contains values used to group the data before pre-processing. For RFID data, this column should contain the PIT tag identifiers, so that pre-processing is performed for each unique PIT tag. For beam breaker data, this column should be the unique beam breaker labels so that pre-processing is carried out separately for each beam breaker pair. The default is NULL, since this argument is not needed to process the video data
#' @param thin_threshold A single numeric value representing a temporal threshold in seconds that will be used to thin the raw data. The default is NULL, since this argument is not needed to process the video data
#' @param pixel_threshold A single numeric value representing a temporal threshold in seconds that will be used to filter out video recording events with total pixels that changed below this threshold. The default is NULL, since this argument is not needed to process the RFID or beam breaker data
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the raw data is saved across sensors inside the path above. For instance, "raw_combined".
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#'  
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' 
#' @return A data frame object with the pre-processed detections per sensor, all metadata columns in the original data frame, as well as a column indicating the temporal threshold used for pre-processing by thinning (in seconds), or a column indicating the threshold used to filter detections by the number of pixels that changed (video data). Each row of this data frame is a pre-processed detection from the raw data collected by the given sensor.

# I need to iterate over sensors again...

# sensor <- "Video"
# detection_col_nm <- "timestamp_ms"
# group_col_nm <- NULL
# thin_threshold <- 2
# pixel_threshold <- 10000
# pixel_col_nm <- "total_pixels_motionTrigger"
# path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
# data_dir <- "raw_combined"
# out_dir <- "pre_processed"

preprocess_detections <- function(sensor, detection_col_nm, group_col_nm = NULL, pixel_col_nm = NULL, thin_threshold = NULL, pixel_threshold = NULL, path, data_dir, out_dir, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Check that the sensor argument is a string and is 1 of the 3 movement sensors
  if(!grepl("^RFID$|^IRBB$|^Video$", sensor)){
    stop('The sensor argument is not correct, check your spelling or captialization')
  }
  
  # If the thinning temporal threshold should be specified, then check that it is a number
  if(grepl("RFID|IRBB", sensor) & !is.numeric(thin_threshold)){
    stop('The thinning temporal threshold needs to be numeric (in seconds)')
  }
  
  # If the pixel temporal threshold should be specified, then check that it is a number
  if(grepl("Video", sensor) & !is.numeric(pixel_threshold)){
    stop('The pixel temporal threshold needs to be numeric (the number of pixels)')
  }
  
  # Create the directory for saving the pre-processed data files if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Iterate over sensors to read in data across files for each sensor, and write out a .csv file of the combined raw data
  # invisible(lapply(1:length(sensors), function(x){
  
  # Read in the combined raw data for the given sensor type
  raw_data <- read.csv(file.path(path, data_dir, paste("combined_raw_data_", sensor, ".csv", sep = ""))) %>% 
    # Also make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(paste(paste(year, month, day, sep = "-"), original_timestamp, sep = " "), tz = tz, format = POSIXct_format)
    ) %>% 
    # Drop columns that aren't needed here
    dplyr::select(-c("original_timestamp", "data_stage", "date_combined"))
  
  # Check that the raw data is a data frame
  if(!is.data.frame(raw_data)){
    stop('The raw data needs to be a data frame')
  }
  
  # Check that the raw data has the column of detections
  if(any(is.null(detection_col_nm) | !detection_col_nm %in% names(raw_data))){
    stop('The column specified in `detection_col_nm` does not exist')
  }
  
  # If the grouping column is necessary, then check that the raw data has this column
  if(grepl("RFID|IRBB", sensor) & any(is.null(group_col_nm) | !group_col_nm %in% names(raw_data))){
    stop('The column specified in `group_col_nm` does not exist')
  }
  
  # If the grouping column is necessary, then check that this column does not have NAs
  if(grepl("RFID|IRBB", sensor)){
    if(any(is.na(raw_data[[group_col_nm]]))){
      stop('The column specified in `group_col_nm` has NA values') 
    }
  }
  
  # If the pixel column is necessary, then check that the raw data has this column
  if(grepl("Video", sensor) & any(is.null(pixel_col_nm) | !pixel_col_nm %in% names(raw_data))){
    stop('The column specified in `pixel_col_nm` does not exist')
  }
  
  # If the pixel column is necessary, then check that this column does not have NAs
  if(grepl("Video", sensor)){
    if(any(is.na(raw_data[[pixel_col_nm]]))){
      stop('The column specified in `pixel_col_nm` has NA values') 
    }
  }
  
  # Check that the year, month, and day columns are also present in the data frame, and do not have NAs
  if(any(!"year" %in% names(raw_data) | !"month" %in% names(raw_data) | !"day" %in% names(raw_data) | any(is.na(raw_data[["year"]])) | any(is.na(raw_data[["month"]])) | any(is.na(raw_data[["day"]])))){
    stop('The data frame is missing columns `year`, `month`, or `day`, or there are NA values in one of these columns')
  }
  
  # Check that the timestamps are in the right format. This conditional also catches NAs in the RFID timestamps
  if(any(is.na(as.POSIXct(raw_data[[detection_col_nm]], format = "%Y-%m-%d %H:%M:%OS")))){
    stop('One or more timestamps are in the wrong format. These need to be in POSIXct or POSIXt format, like %Y-%m-%d %H:%M:%OS')
  }
  
  if(grepl("RFID|IRBB", sensor)){
    
    # Initialize column names that will be shared across sensors, in the order in which they'll appear in the .csv file 
    col_nms <- c("data_type", "chamber_id", "year", "month", "day", detection_col_nm, group_col_nm)
    
    if(!is.null(group_col_nm)){
      
      tmp_df <- raw_data %>%
        group_by(!!sym(group_col_nm)) %>%
        dplyr::arrange(!!sym(detection_col_nm), .by_group = TRUE) %>% 
        # Make unique row indices within groups
        dplyr::mutate(
          group_row_id = row_number()
        ) %>% 
        dplyr::rename(
          `group_col` = all_of(group_col_nm)
        )
      
    } else if(is.null(group_col_nm)){
      
      tmp_df <- raw_data %>% 
        dplyr::arrange(!!sym(detection_col_nm)) %>% 
        # Make unique row indices with the same column name as the group indices above
        dplyr::mutate(
          group_row_id = row_number()
        )
      
    }
    
    # If the group_col is specified, then the lags are calculated per group in the grouped data frame
    lags <- tmp_df %>% 
      dplyr::mutate(
        shift = dplyr::lag(!!sym(detection_col_nm), default = first(!!sym(detection_col_nm)))
      ) %>% 
      # Convert differences to boolean based on the thinning threshold to be able to remove stretches of detection events very close together
      dplyr::mutate(
        diff = floor(!!sym(detection_col_nm) - shift),
        diff = as.numeric(diff),
        binary_diff = (diff >= thin_threshold)
      ) 
    
    if(!is.null(group_col_nm)){
      lags <- lags %>% 
        dplyr::select(!!sym(detection_col_nm), group_col, diff, binary_diff) 
    } else {
      lags <- lags %>% 
        dplyr::select(!!sym(detection_col_nm), diff, binary_diff) 
    }
    
    # Nest by each group, do the rle calculations and removing indices, then recombine
    lags_runs <- lags %>% 
      dplyr::summarise(
        run_indices = cumsum(rle(binary_diff)[["lengths"]]),
        run_values = rle(binary_diff)[["values"]],
        run_lengths = rle(binary_diff)[["lengths"]]
      ) %>% 
      dplyr::filter(!run_values) 
    
    if(!is.null(group_col_nm)){
      
      lags_runs2 <- lags_runs %>% 
        dplyr::select(group_col, run_values, run_lengths, run_indices) %>% 
        pmap_dfr(., function(group_col, run_values, run_lengths, run_indices){
          # In the runs of FALSE values, remove all including the first index. Should work for all runs with length == 1 or > 1
          return(
            data.frame(
              group_col = group_col,
              rem_indices = seq((run_indices - (run_lengths - 1)), run_indices, 1)
            )
          )
        }) %>% 
        # Make sure to drop the first index per group, since these first observations should be retained
        dplyr::filter(rem_indices != 1)
      
      # Per group, remove the indices that represent the RFID detections which are too close together
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
    col_nms <- c("data_type", "chamber_id", "year", "month", "day", detection_col_nm, pixel_col_nm)
    
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
        col_nms, names(.)[-grep(paste(paste("^", col_nms, "$", sep = ""), collapse = "|"), names(.))], "data_stage", "date_pre_processed"
      )
    
  }
  
  # Save the pre-processed data for each sensor in the given setup
  write.csv(filt_df2, file.path(path, out_dir, paste("pre_processed_data_", sensor, ".csv", sep = "")), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
