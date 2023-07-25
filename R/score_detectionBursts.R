library(tidyverse)
library(data.table)

run_length <- 2
file_nm <- "detections_across_sensors.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
general_metadata_cols <- c("chamber_id", "year", "month", "day")
# path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
path <- "/home/gsvidaurre/Desktop/MANUSCRIPTS/Prep/ParentalCareTracking_MethodsPaper/ABS_2023_Talk"
data_dir <- "processed"
out_dir <- "processed"
out_file_nm <- "scored_detectionBursts.csv"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"


# Then I want another function that uses the output of this function as input, and returns which sensors were detected in each bout, all of the edges that were detected (e.g. Camera then RFID, etc) in the sequence, the inferred directionality by comparing all possible pairs of sensors if present in the bout, and the PIT tag ID(s) associated with the given bout.
# Keep the find perching events and bout detection functions. I also want to keep the option to integrate perching events fromn the raw RFID data in this second function

integrate_sensors <- function(file_nm, run_length = 2, sensor_id_col, timestamps_col, PIT_tag_col, general_metadata_cols, path, data_dir, out_dir, out_file_nm = "scored_detectionBursts.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  detectns <- read.csv(file.path(path, data_dir, file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      start = as.POSIXct(format(as.POSIXct(start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      end = as.POSIXct(format(as.POSIXct(end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )

  glimpse(detectns) 
  View(detectns)
  
  
  # I want to find all of the edges that occur in each sequence of sensor triggering events
  detectns %>% 
    rowid_to_column() %>% 
    group_by(rowid) %>% 
    nest() %>% 
    # glimpse()
    # Make a data frame of the indices of edges in the sensor type sequences
    # Get the unique perching events
    dplyr::mutate(
      # Map over the data frames (each nested data frame represents a different detection bout or burst)
      edges = map(
        .x = data,
        # .y = data,
        # Get the edges for each burst of detections
        .f = ~ dplyr::select(.x, start, end, event_seq) %>% 
          pmap_dfr(., function(start, end, event_seq){
            
            sensor_events <- strsplit(event_seq, split = "; ")[[1]]
            # sensor_events
            
            li <- cumsum(rle(sensor_events)[["lengths"]])
            
            # Get the edges using dyads of the last indices
            edges <- unlist(lapply(1:length(li), function(i){
              if(i < length(li)){
                return(paste(test[li[i]], test[li[i + 1]], sep = " - "))
              } else if(i == 1 & length(li) == 1){
                return(NA)
              }
            }))
            
            tmp <- data.frame(
              perching_start = .y[[1]] %>%
                dplyr::filter(group_row_id == first_indices) %>%
                pull(all_of(timestamps_col)),
              perching_end = .y[[1]] %>%
                dplyr::filter(group_row_id == last_indices) %>%
                pull(all_of(timestamps_col))
            ) 
            
            return(tmp)
            
          })
      ) 
    ) %>%
    dplyr::select(-c(data, lags, lags_runs)) %>% 
    unnest(`cols` = c(perching)) %>%
    ungroup() %>% 
    dplyr::select(-c(dates))
    
    
    
    dplyr::mutate(
      # Map over the single nested data frame
      lags_runs = map(
        .x = data,
        .f = ~ dplyr::reframe(.x,
                              first_indices = cumsum(rle(event_seq)[["lengths"]]) - (rle(binary_diff)[["lengths"]]),
                              last_indices = cumsum(rle(event_seq)[["lengths"]]),
                              run_values = rle(event_seq)[["values"]],
                              run_lengths = rle(event_seq)[["lengths"]]
        ) %>% 
          dplyr::filter(run_values & run_lengths >= run_length) %>% 
          ungroup()
      )
    ) 
  
  
}