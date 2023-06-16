#' @title detect_Bouts
#' @description Use lags between RFID detections in an integrated dataset to detect bouts of behavioral activities for one or both individuals (including possible synchronized activities between individuals).
#' 
#' @param integrated_file_nm A character string. This should be the name of the file that contains all of the pre-processed RFID detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the RFID data in the subsequent arguments
#' @param threshold A single numeric value. This represents a temporal threshold in seconds that will be used to identify RFID detections that occurred in close succession (e.g. within 1 or 2 seconds) as detection bouts, and possibly synchronized activities between 2 individuals
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param PIT_tag_col A character value. This is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID")
#' @param preproc_metadata_cols A character vector. This should be a string of the metadata column names from pre-processing that should be dropped from either or both data frames. For instance, c("thin_threshold_s", "data_stage", "date_pre_processed", "lower_threshold_s", "upper_threshold_s", "date_labeled")
#' @param general_metadata_cols A character vector. This should be a string of the general metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("chamber_id", "year", "month", "day"). These columns will be added as the first columns in the integrated data frame, in the same order in which they are provided
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the pre-processed RFID data is saved across sensors inside the path above. For instance, "pre-processed"
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist
#' @param out_file_nm A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default is "activity_bouts.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details # This function finds periods of time with RFID detections that occur close together (determined by the temporal threshold), and then uses the PIT tag information to determine whether two PIT tags were detected or not. This function was wrriten to operate on integrated datasets with RFID detections (the RFID detections have already been thinned during pre-processing). This function could be updated to detect bouts of activities in integrated datasets without RFID detections as well, although the output would not contain information about individual identity. This function must be executed across experimental setups if the tracking system was used to collect data for serial or parallel experimental replicates.
#' 
#' @return TKTK
#' 

# TKTK Across all functions with lead and lag calculations, I need to check whether the sensor that has the first timestamp changes the logic used for pre-processing and integration. If so, then I'll need to generalize all these functions even more to make sure the conditionals used for integration are written correctly

library(tidyverse)

integrated_file_nm <- "integrated_rfid_beamBreaker_data.csv"
# l_th <- 0
# u_th <- 5
threshold <- 30
run_length <- 2
timestamps_col <- "RFID"
PIT_tag_col <- "PIT_tag_ID"
preproc_metadata_cols <- c("Outer_beam_breaker", "Inner_beam_breaker", "irbb_direction_inferred", "unique_entranceExit", "outer_rfid_diffs", "rfid_irbb_assignmnt_type", "rfid_irbb_lower_threshold_s", "rfid_irbb_upper_threshold_s", "data_stage", "date_integrated")
general_metadata_cols <- c("chamber_id", "year", "month", "day")
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
data_dir <- "integrated"
out_dir <- "integrated"
out_file_nm = "activity_bouts.csv"
tz <- "America/New York"
POSIXct_format = "%Y-%m-%d %H:%M:%OS"

detect_synchronizedEvents <- function(integrated_file_nm, threshold, run_length, sensor_id_col, timestamps_col, PIT_tag_col, preproc_metadata_cols, general_metadata_cols, path, data_dir, out_dir, out_file_nm = "inferred_synchonrized_events.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Check that the temporal threshold is numeric
  if(!is.numeric(threshold)){
    stop('The temporal threshold needs to be numeric')
  }
  
  # Create the directory for saving the integrated data files if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the integrated dataset
  integ_df <- read.csv(file.path(path, data_dir, integrated_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Drop columns that aren't needed here
  cols2drop <- names(integ_df)[grep(paste(paste("^", preproc_metadata_cols, "$", sep = ""), collapse = "|"), names(integ_df))]
  
  integ_df2 <- integ_df %>% 
    dplyr::select(-c(all_of(cols2drop)))
  
  # Check that the input data is a data frame
  if(!is.data.frame(integ_df2)){
    stop('The integrated data needs to be a data frame')
  }
  
  # Get the unique PIT tag IDs, which will be used below
  tag_ids <- integ_df2 %>%
    pull(!!sym(PIT_tag_col)) %>%
    unique()
  
  bouts_df <- integ_df2 %>%
    dplyr::mutate(
      dates = paste(year, month, day, sep = "-")
    ) %>%
    # Drop general metadata columns
    dplyr::select(-c(all_of(general_metadata_cols))) %>% 
    # Group the data frame by day to find bouts of RFID detections. Note that grouping is not done by PIT tag here
    group_by(dates) %>% 
    dplyr::arrange(!!sym(timestamps_col), .by_group = TRUE) %>% 
    # Make unique row indices within groups
    dplyr::mutate(
      group_row_id = row_number()
    ) %>% 
    nest() %>%
    dplyr::mutate(
      # Map over the data frames nested by date
      lags = map(
        .x = data,
        .f = ~ dplyr::mutate(.x,
                             shift = dplyr::lag(!!sym(timestamps_col), default = first(!!sym(timestamps_col)))
        ) %>% 
          # Convert differences to Boolean values to be able to identify stretches of detections very close together (the closeness is determine by the temporal threshold)
          dplyr::mutate(
            diff = as.numeric(floor(!!sym(timestamps_col) - shift)),
            # Flag any differences less than or equal to the threshold (e.g. identify these events that occurred within this threshold as bouts of RFID detections). Also added that the difference must be greater than 1 to remove detections compared to themselves
            binary_diff = (diff <= threshold & diff > 0)
          ) %>% 
          dplyr::select(all_of(timestamps_col), diff, binary_diff) 
      )
    ) %>% 
    # Make a data frame of the first and last indices of each run longer than the given run_length that contain temporal differences below or equal to the given threshold (e.g. the first and last indices of each calling bout)
    dplyr::mutate(
      # Map over the nested data frames
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
    # Get the unique calling bout events by mapping over the nested data frames again
    dplyr::mutate(
      detectionBouts = map(
        .x = lags_runs,
        .y = lags,
        # For each unique bout, retain the first and last indices as the timestamps of these RFID detection bouts
        # Use pmap_dfr to iterate over rows in each nested data frame, in which each row represents a unique detection bout by date
        .f = ~ dplyr::select(.x, first_indices, last_indices) %>% 
          pmap_dfr(., function(first_indices, last_indices){
            
            # Get the right indices for summarizing the gaps in time among detections within the bout
            # If the bout was only two detections, then the start and end index should be the same as the end index (...TKTK I need to still check this again). But if the bout had more detections, then I need to make sure I remove the first difference calculation, since the difference calculations are offset by 1 (e.g. the first difference calculation included by the indices is the difference between the first timestamp in the bout and the timestamp immediately before it)
            if(first_indices != last_indices){
              
              ind_s <- first_indices + 1
              
            } else {
              
              ind_s <- first_indices
              
            }
            
            # Also update the last indices accordingly for filtering timestamps
            # I did check this for call data but TKTK need to check again for RFID detections
            ind_e <- last_indices - 1
            
            tmp_bout <- data.frame(
              bout_start = .y[[1]] %>%
                slice(first_indices) %>% 
                pull(all_of(timestamps_col)),
              bout_end = .y[[1]] %>%
                slice(last_indices) %>% 
                pull(all_of(timestamps_col)),
              mean_gap_s = .y[[1]] %>%
                slice(ind_s:last_indices) %>% 
                pull(diff) %>% 
                mean() %>% 
                round(., 4),
              min_gap_s = .y[[1]] %>%
                slice(ind_s:last_indices) %>%
                pull(diff) %>% 
                min(),
              max_gap_s = .y[[1]] %>%
                slice(ind_s:last_indices) %>%
                pull(diff) %>% 
                max()
            ) 
            
            return(tmp_bout)
            
          }) %>% 
          rowid_to_column() %>% 
          dplyr::rename(
            `unique_bout` = "rowid" 
          )
      ) 
    ) %>%
    dplyr::select(-c(data, lags, lags_runs)) %>% 
    unnest(`cols` = c(detectionBouts)) %>%
    ungroup()
  
  # Use the temporal coordinates in bouts_df to figure out whether the given calling bouts were synchronized, and if so, pull out useful summary statistics
  if(nrow(bouts_df) > 0){
    
    sum_bouts_df <- bouts_df %>% 
      dplyr::select(bout_start, bout_end, mean_gap_s, min_gap_s, max_gap_s) %>%
      pmap_dfr(., function(bout_start, bout_end, mean_gap_s, min_gap_s, max_gap_s){
        
        tmp_df <- integ_df2 %>% 
          dplyr::filter(!!sym(timestamps_col) >= bout_start & !!sym(timestamps_col) <= bout_end)
        
        # Use information about the PIT tag IDs to return information about whether or not the bout was synchronized between two individuals, as well as information about perching events
        res_df <- data.frame(
          bout_start = bout_start,
          bout_end = bout_end,
          bout_duration = round(bout_end - bout_start, 4),
          total_detections = nrow(tmp_df),
          indiv1_id = tag_ids[1],
          indiv2_id = tag_ids[2],
          total_indiv1_detections = tmp_df %>% 
            dplyr::filter(!!sym(PIT_tag_col) == tag_ids[1]) %>% 
            nrow(),
          total_indiv2_detections = tmp_df %>% 
            dplyr::filter(!!sym(PIT_tag_col) == tag_ids[2]) %>% 
            nrow(),
          individual_initiated = tmp_df %>% 
            slice(1) %>% 
            pull(!!sym(PIT_tag_col)),
          individual_ended = tmp_df %>% 
            slice(nrow(.)) %>% 
            pull(!!sym(PIT_tag_col)),
          mean_gap_s = mean_gap_s, 
          min_gap_s = min_gap_s,
          max_gap_s = max_gap_s
        ) %>% 
          dplyr::mutate(
            synchronized_bout = ifelse(all(tag_ids %in% unique(tmp_df[[PIT_tag_col]])), "yes", "no")
          ) %>% 
          dplyr::mutate(
            number_perchingEvents = tmp_df %>% 
              dplyr::filter(!is.na(perching_start)) %>% 
              nrow(.)
          )
        
        return(res_df)
        
      }) %>% 
      # Add metadata and arrange columns and rows before writing this out
      dplyr::mutate(
        threshold_s = threshold,
        min_detections_per_bout = run_length, 
        date_processed = paste(Sys.Date(), Sys.time(), sep = " ")
      ) %>% 
      # Add back general metadata columns from the raw dataset
      dplyr::inner_join(
        integ_df2 %>%
          # Rename the timestamps column for the join with the start timestamps immediately below
          dplyr::rename(
            `bout_start` = !!sym(timestamps_col)
          ) %>% 
          dplyr::select(all_of(general_metadata_cols), bout_start),
        by = c("bout_start")
      ) %>%
      dplyr::arrange(bout_start, desc = FALSE) %>% 
      rowid_to_column() %>% 
      dplyr::rename(
        `unique_bout_id` = "rowid"
      ) %>% 
      dplyr::select(all_of(general_metadata_cols), bout_start, bout_end, bout_duration, total_detections, indiv1_id, indiv2_id, total_indiv1_detections, total_indiv2_detections, individual_initiated, individual_ended, synchronized_bout, number_perchingEvents, unique_bout_id, mean_gap_s, min_gap_s, max_gap_s, threshold_s, min_detections_per_bout, date_processed)
    
  } else {
    
    sum_bouts_df <- integ_df2 %>%
      dplyr::select(all_of(general_metadata_cols)) %>% 
      distinct() %>% 
      dplyr::mutate(
        bout_start = NA,
        bout_end = NA,
        bout_duration = NA,
        total_calls = NA,
        indiv1_id = tag_ids[1],
        indiv2_id = tag_ids[2],
        total_indiv1_calls = NA,
        total_indiv2_calls = NA,
        individual_initiated = NA,
        individual_ended = NA,
        mean_gap_s = NA, 
        min_gap_s = NA,
        max_gap_s = NA, 
        unique_bout_id = NA,
        synchronized_bout = NA,
        number_perchingEvents = NA,
        threshold_s = threshold,
        min_detections_per_bout = run_length, 
        date_processed = paste(Sys.Date(), Sys.time(), sep = " ")
      ) %>% 
      dplyr::select(all_of(general_metadata_cols), bout_start, bout_end, bout_duration, total_calls, indiv1_id, indiv2_id, total_indiv1_calls, total_indiv2_calls, individual_initiated, individual_ended, synchronized_bout, number_perchingEvents, unique_bout_id, mean_gap_s, min_gap_s, max_gap_s, threshold_s, min_detections_per_bout, date_processed)
    
  }
  
  write.csv(sum_bouts_df, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
