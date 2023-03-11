#' @title combine_raw_data_across_sensors
#' @description Combine raw data across files for each sensor
#' 
#' @param sensors A character vector of length 1 or more. This vector should contain the names of the sensors across which the raw data will be combined into a single data frame. The default is all sensors, or c("IRBB", "RFID", "Video", "Temp"). Note that if there are multiple sensors for a given type (e.g. two pairs of beam breakers), this data has already been collected together and will travel together in the .csv file of combined raw data.
#' 
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#'  
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @param data_path A character string. This should be the path specifying where the data is saved across sensors. For instance, "/media/gsvidaurre/Box_01/Data".
#'
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of combined raw data should be saved for each sensor. For instance, "raw_combined". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#'
#' @details This function iterates over sensors and all files collected by each sensor to combine data into a single data frame. This function combines data from all files saved in the tracking system directory for each sensor. In other words, all raw data throughout the course of an experiment will be concatenated into the resulting data frame.
#' 
#' @return A .csv file with the raw data for each sensor, as well as all metadata collected by each sensor. Each row of each .csv file is a detection in the raw data collected by the given sensor

combine_raw_data_per_sensor <- function(sensors = c("IRBB", "RFID", "Video", "Temp"), tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS", data_path, out_dir){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Create the directory for saving the combined raw data files if it doesn't already exist
  if(!dir.exists(file.path(data_path, out_dir))){
    dir.create(file.path(data_path, out_dir))
  }
  
  # Iterate over sensors to read in data across files for each sensor, and write out a .csv file of the combined raw data
  invisible(lapply(1:length(sensors), function(x){

    # Get all the files per the given sensor type (e.g. 1 file per day)
    files <- list.files(file.path(data_path, sensors[x]), pattern = paste(sensors[x], "*", sep = "_"), full.names = TRUE)
    
    # Combine data across files into a single data frame
    raw_data <- rbindlist(lapply(1:length(files), function(i){
      read.csv(files[i], header = TRUE, skipNul = TRUE)
    }))
    
    # Video and Temperature data have different names for the timestamp column, so update these column names
    if(grepl("Video|Temp", sensors[x])){
      names(raw_data)[grep("time", names(raw_data))] <- "timestamp"
    }
    
    # Initialize column names that will be shared across sensors, in the order in which they'll appear in the .csv file 
    col_nms <- c("sensor_id", "data_type", "chamber_id", "year", "month", "day", "original_timestamp", "timestamp_ms")
    
    # Add metadata and convert timestamps to a useful format for downstream pre-processing and analysis
    raw_data2 <- raw_data %>% 
      dplyr::mutate(data_type = sensors[x]) %>% 
      # Make another timestamp column: keep the original timestamps and modify the copy to the useful format
      dplyr::rename(
        `original_timestamp` = "timestamp"
      ) %>%
      # Convert timestamps to the POSIX format with millisecond resolution (e.g. decimal seconds). The digit options above control how many decimal places are printed for visualization
      dplyr::mutate(
        timestamp_ms = as.POSIXct(paste(paste(year, month, day, sep = "-"), original_timestamp, sep = " "), tz = tz, format = POSIXct_format)
      ) %>%
      dplyr::mutate(
        data_stage = "raw_combined",
        date_combined = paste(Sys.Date(), Sys.time(), sep = " ")
      ) %>% 
      # Return the shared columns in the order specified above, and any additional metadata columns per sensor afterwards
      dplyr::select(
        col_nms, names(.)[-grep(paste(paste("^", col_nms, "$", sep = ""), collapse = "|"), names(.))], "data_stage", "date_combined"
      )
    
    # Checking: Does this data frame have the same number of rows as the data read in across files?
    if(nrow(raw_data2) != nrow(raw_data)){
      stop("The combined raw data is missing rows")
    }
    
    # Save the raw combined data for each sensor in the given setup
    write.csv(raw_data2, file.path(file.path(data_path, out_dir), paste("combined_raw_data_", sensors[x], ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  # Reset the current global options
  options(orig_opts)
  
}