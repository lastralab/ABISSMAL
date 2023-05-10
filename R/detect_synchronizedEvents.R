#' @title detect_synchronizedEvents
#' @description Use lags between RFID detections in an integrated dataset to detect possible synchronized activities between individuals.
#' 
#' @param integrated_file_nm A character string. This should be the name of the file that contains all of the pre-processed RFID detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the RFID data in the subsequent arguments
#' @param l_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify beam breaker and video events that are close enough together for integration. The default is NULL, but this must be a numeric value when `method` is set to "temporal". This lower temporal threshold is used to infer beam breaker detections and video recording detections that represented the original set of movements that triggered both sensors
#' @param u_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify beam breaker and video events that are close enough together for integration. The default is NULL, but this must be a numeric value when `method` is set to "temporal". This lower temporal threshold is used to infer beam breaker detections and video recording detections that represented the original set of movements that triggered both sensors.
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param PIT_tag_col A character value. This is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID")
#' @param preproc_metadata_cols A character vector. This should be a string of the metadata column names from pre-processing that should be dropped from either or both data frames. For instance, c("thin_threshold_s", "data_stage", "date_pre_processed", "lower_threshold_s", "upper_threshold_s", "date_labeled")
#' @param general_metadata_cols A character vector. This should be a string of the general metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("chamber_id", "year", "month", "day"). These columns will be added as the first columns in the integrated data frame, in the same order in which they are provided
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the pre-processed RFID data is saved across sensors inside the path above. For instance, "pre-processed"
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist
#' @param out_file_nm A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default is "inferred_synchonrized_events.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details TKTK
#' 
#' @return TKTK
#' 

# TKTK Across all functions with lead and lag calculations, I need to check whether the sensor that has the first timestamp changes the logic used for pre-processing and integration. If so, then I'll need to generalize all these functions even more to make sure the conditionals used for integration are written correctly

library(tidyverse)

integrated_file_nm <- "integrated_rfid_beamBreaker_data.csv"
l_th <- 0
u_th <- 5
run_length <- 1
timestamps_col <- "RFID"
PIT_tag_col <- "PIT_tag_ID"
preproc_metadata_cols <- c("Outer_beam_breaker", "Inner_beam_breaker", "irbb_direction_inferred", "unique_entranceExit", "outer_rfid_diffs", "rfid_irbb_assignmnt_type", "rfid_irbb_lower_threshold_s", "rfid_irbb_upper_threshold_s", "data_stage", "date_integrated")
general_metadata_cols <- c("chamber_id", "year", "month", "day")
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
data_dir <- "integrated"
out_dir <- "integrated"
out_file_nm = "inferred_synchonrized_events.csv"
tz <- "America/New York"
POSIXct_format = "%Y-%m-%d %H:%M:%OS"


detect_synchronizedEvents <- function(integrated_file_nm, l_th = NULL, u_th = NULL, run_length, sensor_id_col, timestamps_col, PIT_tag_col, preproc_metadata_cols, general_metadata_cols, path, data_dir, out_dir, out_file_nm = "inferred_synchonrized_events.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Check that all temporal thresholds are each numeric
  if(!is.numeric(l_th)){
    stop('The lower temporal threshold needs to be numeric')
  }
  
  if(!is.numeric(u_th)){
    stop('The upper temporal threshold needs to be numeric')
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
  
  glimpse(integ_df)
  
  # Drop columns that aren't needed here
  cols2drop <- names(integ_df)[grep(paste(paste("^", preproc_metadata_cols, "$", sep = ""), collapse = "|"), names(integ_df))]
  
  integ_df2 <- integ_df %>% 
    dplyr::select(-c(all_of(cols2drop)))
  
  glimpse(integ_df2)
  
  # Check that the input data is a data frame
  if(!is.data.frame(integ_df2)){
    stop('The integrated data needs to be a data frame')
  }
  
  # Get the unique PIT tag IDs, which will be column names below
  rfid_cols <- integ_df2 %>% 
    pull(!!sym(PIT_tag_col)) %>% 
    unique()
  
  # Widen the data frame so each PIT tag is a separate column
  integ_df_tmp <- integ_df2 %>%
    # Make unique row IDs per PIT tag to facilitate widening correctly
    group_by(!!sym(PIT_tag_col)) %>% 
    rowid_to_column() %>% 
    ungroup() %>%
    dplyr::rename(
      `group_rowid` = "rowid"
    ) %>% 
    pivot_wider(
      names_from = !!sym(PIT_tag_col),
      values_from = !!sym(timestamps_col)
    )
  
  glimpse(integ_df_tmp)
  
  # Do the timestamp difference calculations
  lags_df <- integ_df_tmp %>% 
    # The lead() call moves the timestamps for the given column one row index up, so that the difference calculation can be performed with the timestamps for the other column
    # Make a leading and lagging version of timestamps for the first PIT tag ID
    dplyr::mutate(
      leading_ts = lead(!!sym(rfid_cols[1]), default = first(!!sym(rfid_cols[1]))),
      lagging_ts = lag(!!sym(rfid_cols[1]), default = first(!!sym(rfid_cols[1])))
    ) %>% 
    # glimpse()
    dplyr::mutate(
      # For both the lead and lag calculations, positive differences mean that the second PIT tag was detected first, and negative differences mean that the first PIT tag was detected first
      lead_diffs = round(as.numeric(leading_ts - !!sym(rfid_cols[2])), 2),
      lag_diffs = round(as.numeric(lagging_ts - !!sym(rfid_cols[2])), 2)
    ) %>% 
    # View()
    # Convert these differences to boolean based on the given threshold (in seconds)
    dplyr::mutate(
      # To search for events in which the first PIT tag was detected first, look for detections of the first tag that came within the given threshold BEFORE timestamps for the second tag. Set up these conditionals for both the lead and lag calculations
      binary_lead_tag1 = (
        # First PIT tag before, so negative lead differences
        lead_diffs <= -l_th & lead_diffs >= -u_th
      ),
      binary_lag_tag1 = (
        # First PIT tag before, so negative lag differences
        lag_diffs <= -l_th & lag_diffs >= -u_th
      ),
      # To search for events in which the second PIT tag was detected first, look for detections of the first tag that came within the given threshold AFTER timestamps for the second tag. Set up these conditionals for both the lead and lag calculations
      binary_lead_tag2 = (
        # First PIT tag after, so positive lead differences
        lead_diffs >= l_th & lead_diffs <= u_th
      ),
      binary_lag_tag2 = (
        # First PIT tag after, so positive lag differences
        lag_diffs >= l_th & lag_diffs <= u_th
      )
    ) 
  
  runs_df <- lags_df %>% 
    dplyr::summarise(
      # First tag, lead diffs
      first_indices_lead_tag1 = cumsum(rle(binary_lead_tag1)[["lengths"]]) - (rle(binary_lead_tag1)[["lengths"]]),
      last_indices_lead_tag1 = cumsum(rle(binary_lead_tag1)[["lengths"]]),
      run_values_lead_tag1 = rle(binary_lead_tag1)[["values"]],
      run_lengths_lead_tag1 = rle(binary_lead_tag1)[["lengths"]],
      # First tag, lag diffs
      first_indices_lag_tag1 = cumsum(rle(binary_lag_tag1)[["lengths"]]) - (rle(binary_lag_tag1)[["lengths"]]),
      last_indices_lag_tag1 = cumsum(rle(binary_lag_tag1)[["lengths"]]),
      run_values_lag_tag1 = rle(binary_lag_tag1)[["values"]],
      run_lengths_lag_tag1 = rle(binary_lag_tag1)[["lengths"]],
      # Second tag, lead diffs
      first_indices_lead_tag2 = cumsum(rle(binary_lead_tag2)[["lengths"]]) - (rle(binary_lead_tag2)[["lengths"]]),
      last_indices_lead_tag2 = cumsum(rle(binary_lead_tag2)[["lengths"]]),
      run_values_lead_tag2 = rle(binary_lead_tag2)[["values"]],
      run_lengths_lead_tag2 = rle(binary_lead_tag2)[["lengths"]],
      # Second tag, lag diffs
      first_indices_lag_tag2 = cumsum(rle(binary_lag_tag2)[["lengths"]]) - (rle(binary_lag_tag2)[["lengths"]]),
      last_indices_lag_tag2 = cumsum(rle(binary_lag_tag2)[["lengths"]]),
      run_values_lag_tag2 = rle(binary_lag_tag2)[["values"]],
      run_lengths_lag_tag2 = rle(binary_lag_tag2)[["lengths"]]
    ) 
  # %>% 
  # glimpse()
  
  
  
  conditnal_lead_tag1 <- "run_values_lead_tag1 & run_lengths_lead_tag1 >= run_length"
  conditnal_lead_tag2 <- "run_values_lead_tag2 & run_lengths_lead_tag2 >= run_length"
  
  conditnal_lag_tag1 <- "run_values_lag_tag1 & run_lengths_lag_tag1 >= run_length"
  conditnal_lag_tag2 <- "run_values_lag_tag2 & run_lengths_lag_tag2 >= run_length"
  
  
  
  
  
  
  glimpse(lags_df)
  glimpse(runs_df)
  
  lags_df %>% 
    slice(1280:1281) %>% 
    View()
  
  lags_df %>% 
    slice(1594:1595) %>% 
    View()
  
  # From lags_df
  # lead_diffs = round(as.numeric(leading_ts - !!sym(rfid_cols[2])), 2),
  # lag_diffs = round(as.numeric(lagging_ts - !!sym(rfid_cols[2])), 2)
  
  # Filter on all of these columns to identify runs that fit the logic for synchronized events
  sync_df <- runs_df %>% 
    # First PIT tag first, lead differences
    dplyr::filter(
      !!rlang::parse_expr(conditnal_lead_tag1)
    ) %>% 
    # glimpse()
    # If a first index is stored as 0, then add 1 to restore this first index
    dplyr::mutate(
      first_indices_lead_tag1 = ifelse(first_indices_lead_tag1 == 0, first_indices_lead_tag1 + 1, first_indices_lead_tag1)
    ) %>% 
    # TKTK need to check this logic...
  dplyr::mutate(
    # The start timestamps need to come from !!sym(rfid_cols[1]), since for the 1st PIT tag to be detected first, there should have been a negative difference in timestamps
    start = lags_df %>% 
      slice(first_indices_lead_tag1) %>% 
      pull(!!sym(rfid_cols[1])),
    # The end timestamps need to come from !!sym(rfid_cols[2]), since for the 1st PIT tag to be detected first, there should have been a negative difference in timestamps
    end = lags_df %>% 
      slice(last_indices_lead_tag1) %>% 
      pull(!!sym(rfid_cols[2])),
    first_PIT_tag = rfid_cols[1],
    total_detections = run_lengths_lead_tag1
  ) %>% 
    dplyr::select(start, end, first_PIT_tag, total_detections) %>% 
    # glimpse()
    # First PIT tag first, lag differences
    bind_rows(
      runs_df %>% 
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lag_tag1)
        ) %>% 
        # If a first index is stored as 0, then add 1 to restore this first index
        dplyr::mutate(
          first_indices_lag_tag1 = ifelse(first_indices_lag_tag1 == 0, first_indices_lag_tag1 + 1, first_indices_lag_tag1)
        ) %>% 
        dplyr::mutate(
          # The start timestamps need to come from !!sym(rfid_cols[1]), since for the 1st PIT tag to be detected first, there should have been a negative difference in timestamps
          start = lags_df %>% 
            slice(first_indices_lag_tag1) %>% 
            pull(!!sym(rfid_cols[1])),
          # The end timestamps need to come from !!sym(rfid_cols[2]), since for the 1st PIT tag to be detected first, there should have been a negative difference in timestamps
          end = lags_df %>% 
            slice(last_indices_lag_tag1) %>% 
            pull(!!sym(rfid_cols[2])),
          first_PIT_tag = rfid_cols[1],
          total_detections = run_lengths_lag_tag1
        ) %>% 
        dplyr::select(start, end, first_PIT_tag, total_detections)
    ) %>% 
    # Second PIT tag first, lead differences
    bind_rows(
      runs_df %>% 
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lead_tag2)
        ) %>% 
        # View()
        # If a first index is stored as 0, then add 1 to restore this first index
        dplyr::mutate(
          first_indices_lead_tag2 = ifelse(first_indices_lead_tag2 == 0, first_indices_lead_tag2 + 1, first_indices_lead_tag2)
        ) %>% 
        dplyr::mutate(
          # The start timestamps need to come from !!sym(rfid_cols[2]), since for the 2nd PIT tag to be detected first, there should have been a positive difference in timestamps
          start = lags_df %>% 
            slice(first_indices_lead_tag2) %>% 
            pull(!!sym(rfid_cols[2])),
          # The end timestamps need to come from !!sym(rfid_cols[1])
          end = lags_df %>% 
            slice(last_indices_lead_tag2) %>% 
            pull(!!sym(rfid_cols[1])),
          first_PIT_tag = rfid_cols[2],
          total_detections = run_lengths_lead_tag2
        ) %>% 
        dplyr::select(start, end, first_PIT_tag, total_detections)
    ) %>% 
    # Second PIT tag first, lag differences
    bind_rows(
      runs_df %>% 
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lag_tag2)
        ) %>% 
        # If a first index is stored as 0, then add 1 to restore this first index
        dplyr::mutate(
          first_indices_lag_tag2 = ifelse(first_indices_lag_tag2 == 0, first_indices_lag_tag2 + 1, first_indices_lag_tag2)
        ) %>% 
        dplyr::mutate(
          # The start timestamps need to come from !!sym(rfid_cols[2]), since for the 2nd PIT tag to be detected first, there should have been a positive difference in timestamps
          start = lags_df %>% 
            slice(first_indices_lag_tag2) %>% 
            pull(!!sym(rfid_cols[2])),
          # The end timestamps need to come from !!sym(rfid_cols[1])
          end = lags_df %>% 
            slice(last_indices_lag_tag2) %>% 
            pull(!!sym(rfid_cols[1])),
          first_PIT_tag = rfid_cols[2],
          total_detections = run_lengths_lag_tag2
        ) %>% 
        dplyr::select(start, end, first_PIT_tag, total_detections)
    ) 
  
  # TKTK I need to sort out the logic of which indices are the start versus end timestamps...not sure how to handle runs of length 1 versus potential longer runs. For uns of length 1, it appears I should only be using the last index for the leading/lagging of the first PIT tag, and then the last index for the second PIT tag. But not sure that this same logic applies for longer runs...yep I'm right about this. I think it would be good to have a pmap_dfr with an embedded conditional to use the last index for the first and last when run length is 1, and will need to build a test dataset for run lengths longer than 1
  sync_df
  # %>% 
    # glimpse()
    
  # What should the output be? A data frame in which each row is a synchronized event, with the start timestamp, the end timestamp, the ID of the PIT tag detected first, the number of detections
    
    
    # Then filter for beam breaker events that match the logic for entrances
    dplyr::filter(
      !!rlang::parse_expr(conditnal_lead_ent)
    ) %>%
    dplyr::mutate(
      irbb_direction_inferred = "entrance",
      irbb_assignmnt_type = "lead"
    ) %>% 
    dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm), diffs, irbb_direction_inferred, irbb_assignmnt_type) 
  
 
  
  
  # Make sure to add metadata columns for this integration step
  dplyr::mutate(
    data_stage = "integration",
    rfid_video_lower_threshold_s = l_th,
    rfid_video_upper_threshold_s = u_th,
    video_recording_duration_s = video_rec_dur,
    date_integrated = paste(Sys.Date(), Sys.time(), sep = " ")
  ) %>%
    dplyr::rename(
      !!PIT_tag_col := `group_col`,
      # Had to rename this column for the join below
      !!timestamps_col := !!sym(video_col)
    ) %>% 
    # Add back metadata about the video recording events and general metadata
    dplyr::inner_join(
      preproc_video %>% 
        dplyr::select(all_of(general_metadata_cols), all_of(video_metadata_cols), all_of(timestamps_col)),
      by = all_of(timestamps_col)
    ) %>%
    dplyr::rename(
      !!video_col := !!sym(timestamps_col)
    )
  
  if(!is.na(extra_cols2drop)){
    
    integr8d_df <- integr8d_df %>% 
      # Add back extra metadata columns dropped from the RFID dataset
      dplyr::rename(
        # Had to rename this column for the join below
        !!timestamps_col := !!sym(rfid_col)
      ) %>% 
      dplyr::inner_join(
        preproc_rfid %>% 
          dplyr::select(all_of(extra_cols2drop), all_of(timestamps_col)),
        by = all_of(timestamps_col)
      ) %>%
      dplyr::rename(
        !!rfid_col := !!sym(timestamps_col)
      ) %>% 
      dplyr::select(all_of(general_metadata_cols), all_of(rfid_col), all_of(video_col), all_of(PIT_tag_col), rfid_video_direction_inferred, rfid_video_diffs, rfid_video_assignmnt_type, rfid_video_movement_inference, all_of(video_metadata_cols), all_of(extra_cols2drop), rfid_video_lower_threshold_s, rfid_video_upper_threshold_s, video_recording_duration_s, data_stage, date_integrated) %>% 
      dplyr::arrange(!!sym(rfid_col), desc = FALSE)
    
  } else {
    
    integr8d_df <- integr8d_df %>% 
      dplyr::select(all_of(general_metadata_cols), all_of(rfid_col), all_of(video_col), all_of(PIT_tag_col), rfid_video_direction_inferred, rfid_video_diffs, rfid_video_assignmnt_type, rfid_video_movement_inference, all_of(video_metadata_cols), rfid_video_lower_threshold_s, rfid_video_upper_threshold_s, video_recording_duration_s, data_stage, date_integrated) %>% 
      dplyr::arrange(!!sym(rfid_col), desc = FALSE)
    
  }
  
  #### Handle duplicates
  
  # The same RFID timestamp should NOT be assigned to multiple video timestamps, regardless of whether this was motion or post-motion assignment
  # The same video timestamp CAN be assigned to different RFID timestamps, since some RFID detections may have happened after the original movement that triggered the video recording
  # To remove RFID duplicate assignments, I need to use a temporal rule to retain the RFID and video timestamps closest together in time. And drop the other matches as duplicates. This should be applied regardless of whether or not the assignment is motion trigger or post-motion trigger. Even for post motion trigger, the RFID and video timestamps closest together in time should be retained
  
  dup_inds <- which(duplicated(integr8d_df[[rfid_col]]))
  
  if(length(dup_inds) > 0){
    
    # Return the rows to retain
    tmp_df <- data.table::rbindlist(lapply(1:length(dup_inds), function(i){
      
      # For each RFID timestamp that is present more than once, retain the integrated event that represents the closest match (e.g. the smallest temporal difference) between the RFID and video timestamps
      tmp_dup <- integr8d_df %>% 
        slice(dup_inds[i]) %>% 
        pull(!!sym(rfid_col))
      
      return(
        integr8d_df %>% 
          dplyr::filter(
            !!sym(rfid_col) == tmp_dup
          ) %>% 
          dplyr::arrange(-desc(abs(rfid_video_diffs))) %>% 
          slice(1)
      )
      
    }))
    
    # Get the indices of all of the duplicated rows
    all_dup_inds <- unlist(lapply(1:length(dup_inds), function(i){
      
      tmp_dup <- integr8d_df %>% 
        slice(dup_inds[i]) %>% 
        pull(!!sym(rfid_col))
      
      return(
        integr8d_df %>% 
          rowid_to_column() %>% 
          dplyr::filter(
            !!sym(rfid_col) == tmp_dup
          ) %>% 
          pull(rowid)
      )
      
    }))
    
    # Remove all of the duplicated rows, then add back the rows to retain
    integr8d_df_noDups <- integr8d_df %>% 
      slice(-c(all_dup_inds)) %>% 
      bind_rows(
        tmp_df
      ) %>% 
      dplyr::arrange(!!sym(rfid_col), desc = FALSE)
    
  } else {
    integr8d_df_noDups <- integr8d_df
  }
  
  # Integrate perching events if specified
  if(integrate_perching){
    
    perch_df <- read.csv(file.path(path, rfid_dir, "perching_events.csv"))  %>% 
      # Make sure that the timestamps are in the right format
      dplyr::mutate(
        perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
        perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    if(nrow(perch_df) > 0){
      
      # For each integrated detection, figure out whether it occurred during a perching event and add that perching event to the integrated dataset
      # Rename the column of RFID timestamps in the integrated dataset so that the column name can be passed to the function call in pmap_dfr
      integr8d_df_noDups <- integr8d_df_noDups %>% 
        dplyr::rename(
          `rfid_col` = !!sym(rfid_col)
        )
      
      tmp_df <- integr8d_df_noDups %>% 
        dplyr::select(rfid_col) %>% 
        pmap_dfr(., function(rfid_col){
          
          tmp_perching <- perch_df %>% 
            dplyr::filter(
              rfid_col >= perching_start & rfid_col <= perching_end 
            ) %>% 
            dplyr::mutate(
              rfid_col = rfid_col
            ) %>% 
            dplyr::select(rfid_col, all_of(PIT_tag_col), perching_start, perching_end, perching_duration_s, unique_perching_event)
          
          return(tmp_perching)
          
        })
      
      # Then join this data frame of perching event assignments with the integrated dataset
      integr8d_df_noDups_p <- integr8d_df_noDups %>% 
        dplyr::full_join(
          tmp_df,
          by = c("rfid_col", all_of(PIT_tag_col))
        ) %>% 
        # Rename the RFID timestamps column
        dplyr::rename(
          !!rfid_col := "rfid_col"
        ) %>% 
        dplyr::select(names(.)[-grep("^data_stage$|^date_integrated$", names(.))], "data_stage", "date_integrated")
      
    } else {
      
      warning("The perching events dataset was empty; skipping integration of perching events")
      integr8d_df_noDups_p <- integr8d_df_noDups
      
    }
    
  } else {
    
    integr8d_df_noDups_p <- integr8d_df_noDups
    
  }
  
  write.csv(integr8d_df_noDups_p, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
