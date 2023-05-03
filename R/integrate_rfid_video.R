#' @title integrate_rfid_video
#' @description Use lags between the pre-processed radio frequency identification (RFID) data and the pre-processed video recording events to integrate these two datasets. Each RFID detection that remains must be accompanied by a video recording event (e.g. the RFID detection occurred within a certain time before the recording onset or during a video recording)
#' 
#' @param rfid_file_nm A character string. This should be the name of the file that contains all of the pre-processed RFID detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the RFID data in the subsequent arguments
#' @param video_file_nm A character string. This should be the name of the file that contains all of the pre-processed video detections. Each row is a unique detection event. This data frame must contain all the columns specified for the video data in the subsequent arguments
#' @param l_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify beam breaker and video events that are close enough together for integration. The default is NULL, but this must be a numeric value when `method` is set to "temporal". This lower temporal threshold is used to infer beam breaker detections and video recording detections that represented the original set of movements that triggered both sensors
#' @param u_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify beam breaker and video events that are close enough together for integration. The default is NULL, but this must be a numeric value when `method` is set to "temporal". This lower temporal threshold is used to infer beam breaker detections and video recording detections that represented the original set of movements that triggered both sensors.
#' @param video_rec_dur A numeric argument. This represents the duration of video recording (post-motion detection) in seconds. This argument, along with the upper temporal threshold above (u_th) is used to identify beam breaker events that occurred during video recording, but are inferred to not represent the original movements that triggered the movement sensors. This argument facilitates retaining beam breaker events that occurred during the span of video recording and therefore did not trigger a separate video recording event. The default is NULL, but this must be a numeric value when `method` is set to "temporal"
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param PIT_tag_col A character value. This is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID")
#' @param preproc_metadata_cols A character vector. This should be a string of the metadata column names from pre-processing that should be dropped. For instance, c("thin_threshold_s", "data_stage", "date_pre_processed")
#' @param general_metadata_cols A character vector. This should be a string of the general metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("chamber_id", "year", "month", "day"). These columns will be added as the first columns in the integrated data frame, in the same order in which they are provided
#' @param video_metadata_cols A character vector. This should be a string of the video metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"). These columns will be added as later columns in the integrated data frame, in the same order in which they are provided
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param rfid_dir A character string. This should be the name of directory where the pre-processed RFID data is saved across sensors inside the path above. For instance, "pre-processed"
#' @param video_dir A character string. This should be the name of directory where the pre-processed video data is saved across sensors inside the path above. For instance, "pre-processed"
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist
#' @param out_file_nm A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default is "integrated_rfid_video_data.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details This RFID and video integration is a separate function because the way in which the sensors are set up to detect movement determines how the lag calculations and integration should be performed. In other words, it is difficult to make a general function to integrate data collected across any two types of sensors used in the tracking system. This function was written to integrate data across 1 RFID antenna and 1 camera mounted on a nest container that was designed for zebra finches. The RFID antenna sits in the middle of a circular entrance, and the camera records into the center of the nest container from above. This function integrates detections across these 2 sensor types regardless of whether or not these detections occurred during perching events captured by the RFID antenna (see `find_rfid_perching_events`). The reason for this is that some perching events may have started or ended as entrance or exit events, and it's important to retain those events at this stage. If it becomes important later to remove behavioral events that were associated with longer perching events, then this can be done by filtering out detections from the integrated dataset that overlap in time with perching events. This function uses the direction of temporal differences between the RFID and video timestamps to make inferences about entrances and exits, which can be compared to entrances and exits detected by the beam breakers. The direction of movement is "none" for RFID timestamps that occurred after the onset of video recording but were inferred to not be part of the original movement that triggered video recording
#' 
#' @return A .csv file with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the RFID antenna, the video recording events, and information about the given data processing stage. Each row in the .csv file is an RFID detection that was integrated with a video recording event. Information about the temporal thresholds used for the integration and the date that the data was integrated is also contained in this spreadsheet.
#' 

# TKTK Across all functions with lead and lag calculations, I need to check whether the sensor that has the first timestamp changes the logic used for pre-processing and integration. If so, then I'll need to generalize all these functions even more to make sure the conditionals used for integration are written correctly
# rfid_file_nm = "integrated_rfid_beamBreaker_data.csv"
# video_file_nm
# l_th = l2_th
# u_th = u2_th
# video_rec_dur
# sensor_id_col
# timestamps_col = timestamps_col
# PIT_tag_col
# preproc_metadata_cols = c("data_stage", "date_integrated")
# general_metadata_cols = c("chamber_id", "year", "month", "day")
# extra_cols2drop = c("Outer_beam_breaker", "Inner_beam_breaker", "irbb_direction_inferred", "unique_entranceExit", "outer_rfid_diffs", "rfid_irbb_assignmnt_type", "rfid_irbb_lower_threshold_s", "rfid_irbb_upper_threshold_s") # need to update the latter two col names to have irbb specified
# video_metadata_cols
# path
# rfid_dir = "tmp"
# video_dir
# out_dir = "tmp"
# tz
# POSIXct_format = "%Y-%m-%d %H:%M:%OS"

integrate_rfid_video <- function(rfid_file_nm, video_file_nm, l_th = NULL, u_th = NULL, video_rec_dur = NULL, sensor_id_col, timestamps_col, PIT_tag_col, preproc_metadata_cols, general_metadata_cols, video_metadata_cols, extra_cols2drop, path, rfid_dir, video_dir, out_dir, out_file_nm, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
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
  
  if(!is.numeric(video_rec_dur)){
    stop('The video recording duration needs to be numeric (in seconds)')
  }
  
  # Create the directory for saving the integrated data files if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the pre-processed RFID data
  preproc_rfid <- read.csv(file.path(path, rfid_dir, rfid_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    # Drop pre-processing columns that aren't needed here
    dplyr::select(-all_of(preproc_metadata_cols))
  
  # Read in the pre-processed video data
  preproc_video <- read.csv(file.path(path, video_dir, video_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    # Make sure to retain rows for the pre_trigger videos only, to avoid duplicating timestamps and metadata
    dplyr::filter(
      grepl("pre_trigger", video_file_name)
    )
  
  # Drop columns that aren't needed for the integration
  preproc_video2 <- preproc_video %>% 
    dplyr::select(-c(all_of(video_metadata_cols), "data_stage", "date_pre_processed"))
  
  # Check that the input data are both data frames
  if(!is.data.frame(preproc_rfid)){
    stop('The RFID data needs to be a data frame')
  }
  
  if(!is.data.frame(preproc_video2)){
    stop('The Video data needs to be a data frame')
  }
  
  # Group the RFID data frame
  rfid_df_tmp <- preproc_rfid %>%
    # Drop additional metadata columns here
    dplyr::select(-c(all_of(extra_cols2drop))) %>% 
    group_by(!!sym(PIT_tag_col)) %>% 
    dplyr::rename(
      `group_col` = all_of(PIT_tag_col)
    ) %>%
    nest()
  
  # Get the sensor ID value for the RFID data, which will be a column name below
  rfid_col <- preproc_rfid %>% 
    pull(sensor_id) %>% 
    unique()
  
  # Make sure that the video data has the same columns in the same order as the RFID data for the row binding and calculations below
  video_df_tmp <- preproc_video2 %>% 
    dplyr::mutate(
      group_col = NA
    ) %>%
    dplyr::select(names(rfid_df_tmp$data[[1]]))
  
  # Get the sensor ID value for the video data, which will be a column name below
  video_col <- preproc_video2 %>% 
    pull(sensor_id) %>% 
    unique()
  
  # Do the timestamp difference calculations
  lags_grpd <- rfid_df_tmp %>% 
    dplyr::mutate(
      # Here the mapping structure sets up running the following code for each group (PIT taq ID) in the RFID data frame
      lags = map(
        .x = data,
        .y = video_df_tmp,
        .f = ~ bind_rows(.x, .y) %>%
          as_tibble() %>%
          # Order timestamps within each data type
          group_by(!!sym(sensor_id_col)) %>% 
          dplyr::arrange(!!sym(timestamps_col), desc = FALSE) %>%
          # Make unique row indices within groups
          dplyr::mutate(
            group_row_id = row_number()
          ) %>%
          ungroup() %>% 
          pivot_wider(
            names_from = all_of(sensor_id_col),
            values_from = all_of(timestamps_col)
          ) %>% 
          # Make a leading and lagging RFID column for calculations and filtering below. Lead() moves the RFID timestamps one row index up, and lag() moves these timestamps one row index down
          dplyr::mutate(
            leading_RFID = lead(!!sym(rfid_col), default = first(!!sym(rfid_col))),
            lagging_RFID = lag(!!sym(rfid_col), default = first(!!sym(rfid_col)))
          ) %>% 
          # Calculate the differences between the relevant pairs of timestamps: RFID compared to each video event. These differences are calculated per group in the grouped data frame
          dplyr::mutate(
            # For the leading calculations, positive differences mean that the camera triggered first, while negative differences mean that the RFID triggered first
            # For the lagging calculations, negative differences mean that the RFID triggered first, while positive differences mean the camera triggered first
            rfid_video_lead_diffs = round(as.numeric(leading_RFID - !!sym(video_col) ), 2),
            rfid_video_lag_diffs = round(as.numeric(lagging_RFID - !!sym(video_col)), 2)
          ) %>% 
          # Convert differences to Boolean based on a threshold (in seconds)
          # Although I can't account for directionality directly in either of these datasets alone, I still need to be able to capture entrance and exit movements in the integration done here
          # To search for entrances, look RFID detections that came within the given l_th or u_th BEFORE a video timestamp. Set up these conditionals for both the lead and lag calculations
          dplyr::mutate(
            binary_lead_ent = (
              rfid_video_lead_diffs <= -l_th & rfid_video_lead_diffs >= -u_th
            ),
            binary_lag_ent = (
              rfid_video_lag_diffs <= -l_th & rfid_video_lag_diffs >= -u_th
            ),
            # Then to search for exits, look for RFID detections that came within the given l_th or u_th AFTER a video timestamp. Again, set up these conditionals for both the lead and lag calculations
            binary_lead_exi = (
              rfid_video_lead_diffs >= l_th & rfid_video_lead_diffs <= u_th
            ),
            binary_lag_exi = (
              rfid_video_lag_diffs >= l_th & rfid_video_lag_diffs <= u_th
            ),
            # Then catch RFID timestamps that happened beyond the upper temporal threshold but still within the duration of video recording. Looking for lead and lag positive differences, since these are events in which the camera triggered first
            binary_lead_withinVideo = (
              rfid_video_lead_diffs >= u_th & rfid_video_lead_diffs <= video_rec_dur
            ),
            binary_lag_withinVideo = (
              rfid_video_lag_diffs >= u_th & rfid_video_lag_diffs <= video_rec_dur
            )
          ) %>% 
          # Drop all rows with NA values across these binary columns
          dplyr::filter(
            !dplyr::if_all(
              c(
                binary_lead_ent,
                binary_lag_ent,
                binary_lead_exi,
                binary_lag_exi,
                binary_lead_withinVideo,
                binary_lag_withinVideo
              ), 
              is.na
            )
          )
      )
    )
  
  # View(lags_grpd$lags[[1]])
  
  conditnal_lead_ent <- "binary_lead_ent & !is.na(binary_lead_ent)"
  conditnal_lead_exi <- "binary_lead_exi & !is.na(binary_lead_exi)"
  
  conditnal_lag_ent <- "binary_lag_ent & !is.na(binary_lag_ent)"
  conditnal_lag_exi <- "binary_lag_exi & !is.na(binary_lag_exi)"
  
  conditnal_lead_withn <- "binary_lead_withinVideo & !is.na(binary_lead_withinVideo)"
  conditnal_lag_withn <- "binary_lag_withinVideo & !is.na(binary_lag_withinVideo)"
  
  # This works here, so why not below??
  # Entrances, lag differences
  lags_grpd$lags[[1]] %>% 
    dplyr::mutate(
      !!rfid_col := leading_RFID,
      rfid_video_diffs = rfid_video_lead_diffs,
      rfid_video_assignmnt_type = "lead",
      movement_inference = "motion_trigger"
    ) %>% 
    # glimpse()
    # Then filter for video detections that match the RFID detections
    dplyr::filter(
      !!rlang::parse_expr(conditnal_lag_ent)
    ) %>% 
    # glimpse()
    dplyr::mutate(
      rfid_video_direction_inferred = "entrance"
    ) %>% 
    dplyr::select(all_of(rfid_col), all_of(video_col), rfid_video_diffs, rfid_video_direction_inferred, rfid_video_assignmnt_type, movement_inference) %>% 
    glimpse()
  
  # Do more mapping to perform the integration per PIT tag depending on the given temporal thresholds
  integr8d_df <- lags_grpd %>% 
    dplyr::mutate(
      # Entrances, lead differences
      matched_rfid_video = map(
        .x = lags, 
        .f = ~ dplyr::mutate(
          .x,
          !!rfid_col := leading_RFID,
          rfid_video_diffs = rfid_video_lead_diffs,
          rfid_video_assignmnt_type = "lead",
          movement_inference = "motion_trigger"
        ) %>% 
          # Then filter for video detections that match the RFID detections
          dplyr::filter(
            !!rlang::parse_expr(conditnal_lead_ent)
          ) %>% 
          dplyr::mutate(
            rfid_video_direction_inferred = "entrance"
          ) %>% 
          dplyr::select(all_of(rfid_col), all_of(video_col), rfid_video_diffs, rfid_video_direction_inferred, rfid_video_assignmnt_type, movement_inference) 
        %>% 
          # Entrances, lag differences
          bind_rows(
            .x %>% 
              dplyr::mutate(
                !!rfid_col := lagging_RFID,
                rfid_video_diffs = rfid_video_lag_diffs,
                rfid_video_assignmnt_type = "lag",
                movement_inference = "motion_trigger"
              ) %>% 
              # Then filter for video detections that match the RFID detections
              dplyr::filter(
                !!rlang::parse_expr(conditnal_lag_ent)
              ) %>% 
              dplyr::mutate(
                rfid_video_direction_inferred = "entrance"
              ) %>% 
              dplyr::select(all_of(rfid_col), all_of(video_col), rfid_video_diffs, rfid_video_direction_inferred, rfid_video_assignmnt_type, movement_inference)
          ) %>% 
          # Exits, lead differences
          bind_rows(
            .x %>% 
              dplyr::mutate(
                !!rfid_col := leading_RFID,
                rfid_video_diffs = rfid_video_lead_diffs,
                rfid_video_assignmnt_type = "lead",
                movement_inference = "motion_trigger"
              ) %>% 
              # Then filter for video detections that match the RFID detections
              dplyr::filter(
                !!rlang::parse_expr(conditnal_lead_exi)
              ) %>% 
              dplyr::mutate(
                rfid_video_direction_inferred = "exit"
              ) %>% 
              dplyr::select(all_of(rfid_col), all_of(video_col), rfid_video_diffs, rfid_video_direction_inferred, rfid_video_assignmnt_type, movement_inference) 
          ) %>% 
          # Exits, lag differences
          bind_rows(
            .x %>% 
              dplyr::mutate(
                !!rfid_col := lagging_RFID,
                rfid_video_diffs = rfid_video_lag_diffs,
                rfid_video_assignmnt_type = "lag",
                movement_inference = "motion_trigger"
              ) %>% 
              # Then filter for video detections that match the RFID detections
              dplyr::filter(
                !!rlang::parse_expr(conditnal_lag_exi)
              ) %>% 
              dplyr::mutate(
                rfid_video_direction_inferred = "exit"
              ) %>%
              dplyr::select(all_of(rfid_col), all_of(video_col), rfid_video_diffs, rfid_video_direction_inferred, rfid_video_assignmnt_type, movement_inference) 
          ) %>% 
          # Within video assignments, lead differences
          bind_rows(
            .x %>% 
              dplyr::mutate(
                !!rfid_col := leading_RFID,
                rfid_video_diffs = rfid_video_lead_diffs,
                rfid_video_assignmnt_type = "lead",
                movement_inference = "post_motion_trigger"
              ) %>% 
              # Then filter for video detections that match the RFID detections
              dplyr::filter(
                !!rlang::parse_expr(conditnal_lead_withn)
              ) %>% 
              dplyr::mutate(
                rfid_video_direction_inferred = "none"
              ) %>%
              dplyr::select(all_of(rfid_col), all_of(video_col), rfid_video_diffs, rfid_video_direction_inferred, rfid_video_assignmnt_type, movement_inference)
          ) %>% 
          # Within video assignments, lag differences
          bind_rows(
            .x %>% 
              dplyr::mutate(
                !!rfid_col := lagging_RFID,
                rfid_video_diffs = rfid_video_lag_diffs,
                rfid_video_assignmnt_type = "lag",
                movement_inference = "post_motion_trigger"
              ) %>% 
              # Then filter for video detections that match the RFID detections
              dplyr::filter(
                !!rlang::parse_expr(conditnal_lag_withn)
              ) %>% 
              dplyr::mutate(
                rfid_video_direction_inferred = "none"
              ) %>%
              dplyr::select(all_of(rfid_col), all_of(video_col), rfid_video_diffs, rfid_video_direction_inferred, rfid_video_assignmnt_type, movement_inference)
          )
      )
    ) %>% 
    dplyr::select(-c(data, lags)) %>% 
    unnest(`cols` = c(matched_rfid_video)) %>%
    ungroup() %>%
    # glimpse()
    # Make sure to add metadata columns for this integration step
    dplyr::mutate(
      data_stage = "integration",
      rfid_video_lower_threshold_s = l_th,
      rfid_video_upper_threshold_s = u_th,
      video_recording_duration_s = video_rec_dur,
      date_integrated = paste(Sys.Date(), Sys.time(), sep = " ")
    ) %>%
    # glimpse()
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
    # glimpse()
    dplyr::rename(
      !!video_col := !!sym(timestamps_col)
    ) %>% 
    # glimpse()
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
    # glimpse()
    dplyr::rename(
      !!rfid_col := !!sym(timestamps_col)
    ) %>% 
    dplyr::select(all_of(general_metadata_cols), all_of(rfid_col), all_of(video_col), all_of(PIT_tag_col), rfid_video_direction_inferred, rfid_video_diffs, rfid_video_assignmnt_type, movement_inference, all_of(video_metadata_cols), all_of(extra_cols2drop), data_stage, rfid_video_lower_threshold_s, rfid_video_upper_threshold_s, video_recording_duration_s, date_integrated) %>% 
    dplyr::arrange(!!sym(rfid_col), desc = FALSE)
  
  # glimpse(integr8d_df)
  
  
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
  
  # Save the pre-processed data for each sensor in the given setup
  write.csv(integr8d_df_noDups, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
