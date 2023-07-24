
library(tidyverse)

l_th <- 0
u_th <- 2
run_length <- 2
rfid_file_nm <- "pre_processed_data_RFID.csv"
irbb_file_nm <- "pre_processed_data_IRBB.csv"
video_file_nm <- "pre_processed_data_Video.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
outer_irbb_col <- "Outer_beam_breaker"
inner_irbb_col <- "Inner_beam_breaker"
irbb_event_col <- "irbb_direction_inferred"
irbb_unique_col <- "unique_entranceExit"
preproc_metadata_cols <- c("thin_threshold_s", "data_stage", "date_pre_processed")
general_metadata_cols <- c("chamber_id", "year", "month", "day")
video_metadata_cols <- c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name")
# path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
path <- "/home/gsvidaurre/Desktop/MANUSCRIPTS/Prep/ParentalCareTracking_MethodsPaper/ABS_2023_Talk"
data_dir <- "processed"
out_dir <- "integrated"
out_file_nm <- "integrated_sensors.csv"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"


integrate_sensors <- function(rfid_file_nm, irbb_file_nm, video_file_nm, l_th, u_th, run_length = 2, sensor_id_col, timestamps_col, PIT_tag_col, outer_irbb_col, inner_irbb_col, irbb_event_col, irbb_unique_col, preproc_metadata_cols, general_metadata_cols, integrate_perching, path, data_dir, out_dir, out_file_nm = "integrated_rfid_beamBreaker_data.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Read in the pre-processed RFID data
  preproc_rfid <- read.csv(file.path(path, data_dir, rfid_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) 
  
  # Read in the pre-processed IRBB data (no labeling here)
  preproc_irbb <- read.csv(file.path(path, data_dir, irbb_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) 
  
  # Read in the pre-processed video data
  preproc_video <- read.csv(file.path(path, data_dir, video_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) 
  
  # Drop columns that aren't needed here for both datasets
  rfid_cols2drop <- names(preproc_rfid)[grep(paste(paste("^", preproc_metadata_cols, "$", sep = ""), collapse = "|"), names(preproc_rfid))]
  
  irbb_cols2drop <- names(preproc_irbb)[grep(paste(paste("^", preproc_metadata_cols, "$", sep = ""), collapse = "|"), names(preproc_irbb))]
  
  video_cols2drop <- names(preproc_video)[grep(paste(paste("^", c(preproc_metadata_cols, video_metadata_cols), "$", sep = ""), collapse = "|"), names(preproc_video))]
  
  preproc_rfid2 <- preproc_rfid %>% 
    dplyr::select(-c(all_of(rfid_cols2drop), all_of(PIT_tag_col)))
  
  preproc_irbb2 <- preproc_irbb %>% 
    dplyr::select(-c(all_of(irbb_cols2drop)))
  
  preproc_video2 <- preproc_video %>% 
    dplyr::select(-c(all_of(video_cols2drop)))
  
  glimpse(preproc_rfid2)
  glimpse(preproc_irbb2)
  glimpse(preproc_video2)
  
  # Bind the data frames together
  all_sensors <- preproc_rfid2 %>% 
    bind_rows(
      preproc_irbb2
    ) %>% 
    bind_rows(
      preproc_video2
    )
  
  glimpse(all_sensors)
  
  
  # Group the data frame by day, then search for bursts of activity
  detectns_df <- all_sensors %>%
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
      # Map over the data frames nested by PIT tag IDs
      lags = map(
        .x = data,
        .f = ~ dplyr::mutate(.x,
                             shift = dplyr::lag(!!sym(timestamps_col), default = first(!!sym(timestamps_col)))
        ) %>% 
          # Convert differences to Boolean based on the thinning threshold to find stretches of detection events very close together
          dplyr::mutate(
            diff = as.numeric(floor(!!sym(timestamps_col) - shift)),
            # Taking anything less than or equal to the threshold, see previous RFID pre-processing. The diff > 0 condition should remove the first timestamp compared to itself, which should in turn make it no longer necessary to correct the timestamp indices
            binary_diff = (diff <= u_th & diff > l_th)
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
            
            return(tmp)
            
          })
      ) 
    ) %>%
    dplyr::select(-c(data, lags, lags_runs)) %>% 
    unnest(`cols` = c(bouts)) %>%
    ungroup() %>% 
    dplyr::select(-c(dates))
  
  glimpse(detectns_df)
  View(detectns_df)
  
  length(unique(detectns_df$event_seq))
  
  # Then TKTK the next things to do are use the sequence of sensor events to infer directionality, and assign PIT tags to all movements with RFID
  
  
}