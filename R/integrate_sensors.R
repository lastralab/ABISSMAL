

rm(list = ls())

library(tidyverse)
library(data.table)

threshold <- 2
run_length <- 2
file_nms <- c("pre_processed_data_RFID.csv", "pre_processed_data_IRBB.csv", "pre_processed_data_Video.csv")
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
drop_tag <- "01-10-16-B8-7F"
preproc_metadata_cols <- c("thin_threshold_s", "data_stage", "date_pre_processed")
general_metadata_cols <- c("chamber_id", "year", "month", "day")
video_metadata_cols <- c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name")
# path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
path <- "/home/gsvidaurre/Desktop/MANUSCRIPTS/Prep/ParentalCareTracking_MethodsPaper/ABS_2023_Talk"
data_dir <- "processed"
out_dir <- "processed"
out_file_nm <- "detections_across_sensors.csv"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"

# I want this function to return that data frame of output with the sequence of sensor events. I also want this function to automatically read in as many sensors for input data as provided 

integrate_sensors <- function(file_nms, threshold, run_length = 2, sensor_id_col, timestamps_col, PIT_tag_col, drop_tag = "01-10-16-B8-7F", preproc_metadata_cols, general_metadata_cols, video_metadata_cols, path, data_dir, out_dir, out_file_nm = "detections_across_sensors.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Read in files and remove columns in a loop before binding together in a single data frame
  all_sensors <- data.table::rbindlist(lapply(1:length(file_nms), function(x){
    
    tmp <- read.csv(file.path(path, data_dir, file_nms[x])) %>% 
      # Make sure that the timestamps are in the right format
      dplyr::mutate(
        !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Drop extra columns in order to bind together data frames in the list
    cols2drop <- names(tmp)[grep(paste(paste("^", c(preproc_metadata_cols, video_metadata_cols), "$", sep = ""), collapse = "|"), names(tmp))]
    
    tmp <- tmp %>% 
      dplyr::select(-c(all_of(cols2drop)))
    
    # If a given data frame does not have the column PIT_tag_col, then create a column named like this but with NAs
    if(!any(grepl(PIT_tag_col, names(tmp)))){
      
      tmp <- tmp %>% 
        dplyr::mutate(
          !!PIT_tag_col := NA
        )
      
    }
    
    tmp <- tmp %>% 
      dplyr::select(names(.)[-grep(PIT_tag_col, names(.))], all_of(PIT_tag_col))
    
  }))
  
  # glimpse(all_sensors)
  
  # If RFID data is present, then get the unique PIT tag IDs for operations below
  if(any(grepl("RFID", unique(all_sensors$data_type)))){
    
    tag_ids <- all_sensors %>%
      dplyr::filter(sensor_id == "RFID") %>% 
      dplyr::filter(!!sym(PIT_tag_col) != drop_tag) %>% 
      pull(!!sym(PIT_tag_col)) %>%
      unique()
    
  }
  
  # Group the data frame by day, then search for bursts of activity
  detectns <- all_sensors %>%
    dplyr::mutate(
      dates = paste(year, month, day, sep = "-")
    ) %>%
    group_by(dates) %>% 
    dplyr::arrange(!!sym(timestamps_col), .by_group = TRUE) %>% 
    # Make unique row indices within groups
    dplyr::mutate(
      group_row_id = row_number()
    ) %>% 
    nest() %>% 
    dplyr::mutate(
      # Map over the nested data frames
      lags = map(
        .x = data,
        .f = ~ dplyr::mutate(.x,
                             shift = dplyr::lag(!!sym(timestamps_col), default = first(!!sym(timestamps_col)))
        ) %>% 
          # Convert differences to Boolean based on the thinning threshold to find stretches of detection events very close together
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
    # glimpse()
    # Get the unique bouts of detections
    dplyr::mutate(
      # Map over the data frames nested by date
      bouts = map(
        .x = lags_runs,
        .y = data,
        # For each unique date, retain the first and last indices of sensor detections flagged as bouts
        # Use pmap_dfr to iterate over rows in each nested data frame, in which each row represents a unique bout of detections by date
        .f = ~ dplyr::select(.x, first_indices, last_indices) %>% 
          pmap_dfr(., function(first_indices, last_indices){
            
            tmp <- data.frame(
              start = .y[[1]] %>%
                dplyr::filter(group_row_id == first_indices) %>%
                pull(all_of(timestamps_col)),
              end = .y[[1]] %>%
                dplyr::filter(group_row_id == last_indices) %>%
                pull(all_of(timestamps_col))
            ) %>% 
              dplyr::mutate(
                event_seq = .y[[1]] %>%
                  dplyr::filter(group_row_id >= first_indices & group_row_id <= last_indices) %>%
                  pull(sensor_id) %>% 
                  paste(., collapse = "; ")
              )
            
            # If RFID data is present, then add back PIT tag information
            if(any(grepl("RFID", tmp$event_seq))){
              
              PIT_tag_seq <- .y[[1]] %>%
                dplyr::filter(group_row_id >= first_indices & group_row_id <= last_indices) %>%
                pull(!!sym(PIT_tag_col))
              
              if(!all(is.na(PIT_tag_seq))){
                
                total_indiv1_detections <- length(which(PIT_tag_seq == tag_ids[1]))
                total_indiv2_detections <- length(which(PIT_tag_seq == tag_ids[2]))
                individual_initiated <- PIT_tag_seq[1]
                individual_ended <- PIT_tag_seq[length(PIT_tag_seq)]
                
              } else {
                
                total_indiv1_detections <- total_indiv2_detections <- individual_initiated <- individual_ended <- NA
                
              }
              
              tmp <- tmp %>%
                dplyr::mutate(
                  indiv1_id = tag_ids[1],
                  indiv2_id = tag_ids[2],
                  total_indiv1_detections = total_indiv1_detections,
                  total_indiv2_detections = total_indiv2_detections,
                  individual_initiated = individual_initiated,
                  individual_ended = individual_ended
                )
              
            }
            
            return(tmp)
            
          })
      ) 
    ) %>%
    dplyr::select(-c(data, lags, lags_runs)) %>% 
    unnest(`cols` = c(bouts)) %>%
    ungroup() %>% 
    dplyr::select(-c(dates))
  
  # Order the data and add back important metadata before writing it out
  detectns2 <- detectns %>% 
    dplyr::inner_join(
      all_sensors %>% 
        dplyr::select(all_of(general_metadata_cols), all_of(timestamps_col)) %>% 
        distinct(),
      by = c("start" = timestamps_col)
    ) %>% 
    dplyr::mutate(
      threshold_seconds = threshold,
      run_length = run_length,
      data_stage = "integration",
      date_processed = paste(Sys.Date(), Sys.time(), sep = " ")
    ) %>% 
    dplyr::select(all_of(general_metadata_cols), names(.)[-grep(paste(general_metadata_cols, collapse = "|"), names(.))], threshold_seconds, run_length, data_stage, date_processed)
  
  write.csv(detectns2, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}