#' @title integrate_beamBreakers_video
#' @description Use lags between the pre-processed, labeled infrared beam breaker (IRBB) data and the pre-processed video data to integrate these two datasets. Each beam breaker event (an entrance or an exit) that remains must be accompanied by a detection from the video dataset (e.g. the onset of a video recording event triggered by motion detection)
#' 
#' @param irbb_file_nm A character string. This should be the name of the file that contains all of the pre-processed and labeled beam breaker detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the beam breaker data in the subsequent arguments
#' @param video_file_nm A character string. This should be the name of the file that contains all of the pre-processed video detections. Each row is a unique detection event. This data frame must contain all the columns specified for the video data in the subsequent arguments
#' @param method A character string. This argument should be set to "temporal" or "sign", in order to integrate beam breaker and video detections using temporal differences between timestamps that fall within the bounds of lower and upper temporal thresholds, or instead to use the sign of differences between timestamps, respectively. See `Details` for more information on expected outcomes by either method
#' @param l_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify beam breaker and video events that are close enough together for integration. The default is NULL, but this must be a numeric value when `method` is set to "temporal". This lower temporal threshold is used to infer beam breaker detections and video recording detections that represented the original set of movements that triggered both sensors
#' @param u_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify beam breaker and video events that are close enough together for integration. The default is NULL, but this must be a numeric value when `method` is set to "temporal". This lower temporal threshold is used to infer beam breaker detections and video recording detections that represented the original set of movements that triggered both sensors.
#' @param video_rec_dur A numeric argument. This represents the duration of video recording (post-motion detection) in seconds. This argument, along with the upper temporal threshold above (u_th) is used to identify beam breaker events that occurred during video recording, but are inferred to not represent the original movements that triggered the movement sensors. This argument facilitates retaining beam breaker events that occurred during the span of video recording and therefore did not trigger a separate video recording event. The default is NULL, but this must be a numeric value when `method` is set to "temporal"
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param outer_irbb_col A character value. The column name that contains timestamps for the outer pair of beam breakers (e.g. the first pair of beam breakers that an animal encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param inner_irbb_col A character value. The column name that contains timestamps for the inner pair of beam breakers (e.g. the second pair of beam breakers that an individual encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param irbb_event_col A character value. The name of column that contains the type of beam breaker event (e.g. "entrance" or "exit)
#' @param irbb_unique_col A character value. The name of column that contains the unique numeric identifier for each beam breaker event
#' #' @param general_metadata_cols A character vector. This should be a string of the general metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("chamber_id", "year", "month", "day"). These columns will be added as the first columns in the integrated data frame, in the same order in which they are provided
#' @param video_metadata_cols A character vector. This should be a string of the video metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"). These columns will be added as later columns in the integrated data frame, in the same order in which they are provided
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the raw data is saved across sensors inside the path above. For instance, "raw_combined".
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details This beam breaker and video integration is a separate function because the way in which the sensors are set up to detect movement determines how the lag calculations and integration should be performed. In other words, it is difficult to make a general function to integrate data collected across any two types of sensors used in the tracking system. This function was written to integrate data across 2 pairs of beam breakers and 1 camera mounted around the entrance of a nest container that was designed for zebra finches. This function integrates detections across these 2 sensor types regardless of whether or not these detections occurred during perching events captured by the RFID antenna (see `find_rfid_perching_events`). The reason for this is that some perching events may have started or ended as entrance or exit events, and it's important to retain those events at this stage. If it becomes important later to remove behavioral events that were associated with longer perching events, then this can be done by filtering out detections from the integrated dataset that overlap in time with perching events. Other things to note are that the assignment of beam breaker events to video events will lead to duplicated assignments when using the `temporal` method. This is the expected behavior, and most duplicates should arise from assigning beam breaker events to the remainder of the video recording duration after the "original" movement that triggered video recording (e.g. `post-motion_trigger` in the movement_inference column). When using the `sign` method, expect to see many more duplicated assignments. This method may be more useful for figuring out the distribution of temporal differences between events from both sensor types, which can then be used to inform decisions about temporal thresholds to use for the `temporal` method.
#' 
#' @return A .csv file with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the lead and rear beam breaker pairs, the timestamps of the video recording events, a unique label for the given event (e.g. entrance or exit), a unique numeric identifier for the given event, and information about the given data processing stage. Each row in the .csv file is a labeled event across the outer and inner beam breaker pairs that was integrated with video recording events. Information about the temporal thresholds used for the integration and the date that the data was integrated is also contained in this spreadsheet.
#' 

integrate_beamBreakers_video <- function(irbb_file_nm, video_file_nm, method, l_th = NULL, u_th = NULL, video_rec_dur = NULL, sensor_id_col, timestamps_col, PIT_tag_col, outer_irbb_col, inner_irbb_col, irbb_event_col, irbb_unique_col, general_metadata_cols, video_metadata_cols, path, data_dir, out_dir, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Check that the lower and upper temporal thresholds are each numeric when using the temporal method
  if(grepl("temporal", method) & !is.numeric(l_th)){
    stop('The pre-video recording lower temporal threshold needs to be numeric (in seconds)')
  }
  
  if(grepl("temporal", method) & !is.numeric(u_th)){
    stop('The pre-video recording upper temporal threshold needs to be numeric (in seconds)')
  }
  
  if(grepl("temporal", method) & !is.numeric(video_rec_dur)){
    stop('The post-video recording temporal threshold needs to be numeric (in seconds)')
  }
  
  # Create the directory for saving the integrated data files if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the pre-processed and labeled beam breaker data
  labeled_irbb <- read.csv(file.path(path, out_dir, irbb_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!outer_irbb_col := as.POSIXct(format(as.POSIXct(!!sym(outer_irbb_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      !!inner_irbb_col := as.POSIXct(format(as.POSIXct(!!sym(inner_irbb_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    # Drop columns that aren't needed here
    dplyr::select(-c("data_stage", "temporal_threshold_s", "date_labeled"))
  
  # Read in the pre-processed video data
  preproc_video <- read.csv(file.path(path, data_dir, video_file_nm)) %>% 
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
  if(!is.data.frame(labeled_irbb)){
    stop('The IRBB data needs to be a data frame')
  }
  
  if(!is.data.frame(preproc_video2)){
    stop('The Video data needs to be a data frame')
  }
  
  # Lengthen the IRBB data frame 
  irbb_df_tmp <- labeled_irbb %>% 
    pivot_longer(
      cols = c(all_of(outer_irbb_col), all_of(inner_irbb_col)),
      names_to = sensor_id_col,
      values_to = timestamps_col
    ) %>% 
    dplyr::select(-c(all_of(irbb_event_col), all_of(irbb_unique_col)))
  
  # Make sure that the video data has the same columns in the same order as the beam breaker data for the row binding and calculations below
  video_df_tmp <- preproc_video2 %>% 
    dplyr::select(names(irbb_df_tmp))
  
  # Get the sensor ID value for the video data, which will be a column name below
  video_col <- preproc_video2 %>% 
    pull(sensor_id) %>% 
    unique()
  
  # Do the timestamp difference calculations
  # Here I'm interested in aligning to the inner pair of beam breakers only, since the camera sits last/in the sequence of sensors that should trigger for entrances/exits, and right after/before the inner pair of beam breakers
  lags_df <- video_df_tmp %>% 
    bind_rows(irbb_df_tmp %>%
                dplyr::filter(!!sym(sensor_id_col) == inner_irbb_col)) %>%
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
    # Make a leading and lagging video column for calculations and filtering below. Lead() moves the video timestamps one row index up, and lag() moves these timestamps one row index down
    dplyr::mutate(
      leading_inner_irbb = lead(!!sym(inner_irbb_col), default = first(!!sym(inner_irbb_col))),
      lagging_inner_irbb = lag(!!sym(inner_irbb_col), default = first(!!sym(inner_irbb_col)))
    ) %>% 
    # Calculate the temporal differences between the relevant pairs of timestamps: Inner beam breaker timestamps compared to video to find entrances and exits
    dplyr::mutate(
      # TKTK 26 April: For the leading calculations, negative differences mean that the camera triggered first, while positive differences mean that the inner beam breaker triggered first. For the lagging calculations, negative differences mean that the inner beam breaker triggered first, while positive differences mean that the camera triggered first
      # TKTK 27 April: Not sure why this logic keeps changing..for the leading calculations, positive differences mean that camera triggered first, while negative differences mean that the beam breakers triggered first. For the lagging calculations, positive differences mean that the camera triggered first, and negative differences mean that the inner beam breaker triggered first
      inner_video_lead_diffs = round(as.numeric(leading_inner_irbb - !!sym(video_col)), 2),
      inner_video_lag_diffs = round(as.numeric(lagging_inner_irbb - !!sym(video_col)), 2),
      ## Here positive differences mean the camera triggered first
      ## inner_post_video_diffs = round(as.numeric(leading_RFID - !!sym(video_col)), 2)
    ) %>%
    # Convert these differences to Boolean values based on a threshold (in seconds)
    dplyr::mutate(
      # To search for entrances, look video detections that came within the given l_th or u_th AFTER an inner beam breaker timestamp. Set up these conditionals for both the lead and lag calculations
      binary_lead_inner_ent = (
        # Video after, so negative lead differences
        inner_video_lead_diffs <= -l_th & inner_video_lead_diffs <= -u_th
      ),
      binary_lag_inner_ent = (
        # Video after, so negative lag differences
        inner_video_lag_diffs <= -l_th & inner_video_lag_diffs >= -u_th
      ),
      # Then to search for exits, look for video detections that came within the given l_th or u_th BEFORE an inner beam breaker timestamp. Again, set up these conditionals for both the lead and lag calculations
      binary_lead_inner_exi = (
        # Video before, so positive lead differences
        inner_video_lead_diffs >= l_th & inner_video_lead_diffs <= u_th
      ),
      binary_lag_inner_exi = (
        # Video before, so positive lag differences
        inner_video_lag_diffs >= l_th & inner_video_lag_diffs <= u_th
      ),
      # Then catch inner beam breaker timestamps that happened beyond the upper temporal threshold but still within the duration of video recording. Looking for lead and lag positive differences, since these are events in which the camera triggered first
      binary_lead_inner_withinVideo = (
        inner_video_lead_diffs >= u_th & inner_video_lead_diffs <= video_rec_dur
      ),
      binary_lag_inner_withinVideo = (
        inner_video_lag_diffs >= u_th & inner_video_lag_diffs <= video_rec_dur
      )
    ) %>% 
    # Drop all rows with NA values across these binary columns
    dplyr::filter(
      !dplyr::if_all(
        c(
          binary_lead_inner_ent,
          binary_lag_inner_ent,
          binary_lead_inner_exi,
          binary_lag_inner_exi,
          binary_lead_inner_withinVideo,
          binary_lag_inner_withinVideo
        ), 
        is.na
      )
    ) 

  if(method == "temporal"){
    
    conditnal_lead_ent <- "binary_lead_inner_ent & !is.na(binary_lead_inner_ent)"
    conditnal_lead_exi <- "binary_lead_inner_exi & !is.na(binary_lead_inner_exi)"
    
    conditnal_lag_ent <- "binary_lag_inner_ent & !is.na(binary_lag_inner_ent)"
    conditnal_lag_exi <- "binary_lag_inner_exi & !is.na(binary_lag_inner_exi)"
    
    conditnal_lead_withn <- "binary_lead_inner_withinVideo & !is.na(binary_lead_inner_withinVideo)"
    conditnal_lag_withn <- "binary_lag_inner_withinVideo & !is.na(binary_lag_inner_withinVideo)"
    
  } else if(method == "sign"){
    
    # As above:
    
    # TKTK 26 April: For the leading calculations, negative differences mean that the camera triggered first, while positive differences mean that the inner beam breaker triggered first. For the lagging calculations, negative differences mean that the inner beam breaker triggered first, while positive differences mean that the camera triggered first
    # TKTK 27 April: Not sure why this logic keeps changing..for the leading calculations, positive differences mean that camera triggered first, while negative differences mean that the beam breakers triggered first. For the lagging calculations, positive differences mean that the camera triggered first, and negative differences mean that the inner beam breaker triggered first
    
    # To search for entrances, look video detections that came within the given l_th or u_th AFTER an inner beam breaker timestamp. Set up these conditionals for both the lead and lag calculations
    
    # Then to search for exits, look for video detections that came within the given l_th or u_th BEFORE an inner beam breaker timestamp. Again, set up these conditionals for both the lead and lag calculations
    
    # Then catch inner beam breaker timestamps that happened beyond the upper temporal threshold but still within the duration of video recording. Looking for lead and lag positive differences, since these are events in which the camera triggered first
    
    conditnal_lead_ent <- "inner_video_lead_diffs <= 0 & !is.na(inner_video_lead_diffs)"
    conditnal_lead_exi <- "inner_video_lead_diffs > 0 & !is.na(inner_video_lead_diffs)"
    
    conditnal_lag_ent <- "inner_video_lag_diffs <= 0 & !is.na(inner_video_lag_diffs)"
    conditnal_lag_exi <- "inner_video_lag_diffs > 0 & !is.na(inner_video_lag_diffs)"
    
    # TKTK this will lead to a LOT of assignments...consider dropping this type of matching for the sign method entirely?? But see integrate_rfid_video, looks like the output is the same between methods once duplicates are accounted for
    conditnal_lead_withn <- "inner_video_lead_diffs > 0 & !is.na(inner_video_lead_diffs)"
    conditnal_lag_withn <- "inner_video_lag_diffs > 0 & !is.na(inner_video_lag_diffs)"
    
  }
  
  # Do more mapping to perform the integration depending on the given lower and upper temporal thresholds
  # The integration is done separately for entrances and exits from each of the leading and lagging calculations, as well as the within video assignments
  integr8d_df <- lags_df %>%
    # Entrances, lead differences
    dplyr::mutate(
      !!inner_irbb_col := leading_inner_irbb,
      inner_video_diffs = inner_video_lead_diffs,
      assignmnt_type = "lead",
      movement_inference = "motion_trigger"
    ) %>% 
    # Add back metadata about the beam breaker event labels
    dplyr::inner_join(
      labeled_irbb %>%
        dplyr::select(all_of(inner_irbb_col), all_of(irbb_event_col), all_of(irbb_unique_col)),
      by = c(all_of(inner_irbb_col))
    ) %>%
    # Filter for entrances among the labeled beam breaker events 
    dplyr::filter(
      !!sym(irbb_event_col) == "entrance"
    ) %>% 
    # Then filter for video detections that match these beam breaker events
    dplyr::filter(
      !!rlang::parse_expr(conditnal_lead_ent)
    ) %>% 
    dplyr::select(all_of(inner_irbb_col), all_of(video_col), all_of(irbb_event_col), inner_video_diffs, assignmnt_type, movement_inference) %>%
    # Entrances, lag differences
    bind_rows(
      lags_df %>%
        dplyr::mutate(
          !!inner_irbb_col := lagging_inner_irbb,
          inner_video_diffs = inner_video_lag_diffs,
          assignmnt_type = "lag",
          movement_inference = "motion_trigger"
        ) %>%
        # Add back metadata about the beam breaker event labels
        dplyr::inner_join(
          labeled_irbb %>%
            dplyr::select(all_of(inner_irbb_col), all_of(irbb_event_col), all_of(irbb_unique_col)),
          by = c(all_of(inner_irbb_col))
        ) %>%
        # Filter for entrances among the labeled beam breaker events 
        dplyr::filter(
          !!sym(irbb_event_col) == "entrance"
        ) %>% 
        # Then filter for video detections that match these beam breaker events
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lag_ent)
        ) %>%
        dplyr::select(all_of(inner_irbb_col), all_of(video_col), all_of(irbb_event_col), inner_video_diffs, assignmnt_type, movement_inference)
    ) %>%
    # Exits, lead differences
    bind_rows(
      lags_df %>%
        dplyr::mutate(
          !!inner_irbb_col := leading_inner_irbb,
          inner_video_diffs = inner_video_lead_diffs,
          assignmnt_type = "lead",
          movement_inference = "motion_trigger"
        ) %>%
        # Add back metadata about the beam breaker event labels
        dplyr::inner_join(
          labeled_irbb %>%
            dplyr::select(all_of(inner_irbb_col), all_of(irbb_event_col), all_of(irbb_unique_col)),
          by = c(all_of(inner_irbb_col))
        ) %>%
        # Filter for exits among the labeled beam breaker events 
        dplyr::filter(
          !!sym(irbb_event_col) == "exit"
        ) %>% 
        # Then filter for video detections that match these beam breaker events
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lead_exi)
        ) %>%
        dplyr::select(all_of(inner_irbb_col), all_of(video_col), all_of(irbb_event_col), inner_video_diffs, assignmnt_type, movement_inference)
    ) %>%
    # Exits, lag differences
    bind_rows(
      lags_df %>%
        dplyr::mutate(
          !!inner_irbb_col := lagging_inner_irbb,
          inner_video_diffs = inner_video_lag_diffs,
          assignmnt_type = "lag",
          movement_inference = "motion_trigger"
        ) %>%
        # Add back metadata about the beam breaker event labels
        dplyr::inner_join(
          labeled_irbb %>%
            dplyr::select(all_of(inner_irbb_col), all_of(irbb_event_col), all_of(irbb_unique_col)),
          by = c(all_of(inner_irbb_col))
        ) %>%
        # Filter for exits among the labeled beam breaker events 
        dplyr::filter(
          !!sym(irbb_event_col) == "exit"
        ) %>% 
        # Then filter for video detections that match these beam breaker events
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lag_exi)
        ) %>%
        dplyr::select(all_of(inner_irbb_col), all_of(video_col), all_of(irbb_event_col), inner_video_diffs, assignmnt_type, movement_inference)
    ) %>% 
    # Within video assignments, lead differences
    bind_rows(
      lags_df %>%
        dplyr::mutate(
          !!inner_irbb_col := leading_inner_irbb,
          inner_video_diffs = inner_video_lead_diffs,
          assignmnt_type = "lead",
          movement_inference = "post-motion_trigger"
        ) %>%
        # Add back metadata about the beam breaker event labels
        dplyr::inner_join(
          labeled_irbb %>%
            dplyr::select(all_of(inner_irbb_col), all_of(irbb_event_col), all_of(irbb_unique_col)),
          by = c(all_of(inner_irbb_col))
        ) %>%
        # Then filter for video detections that match these beam breaker events
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lead_withn)
        ) %>%
        dplyr::select(all_of(inner_irbb_col), all_of(video_col), all_of(irbb_event_col), inner_video_diffs, assignmnt_type, movement_inference)
    ) %>%
    # Within video assignments, lag differences
    bind_rows(
      lags_df %>%
        dplyr::mutate(
          !!inner_irbb_col := lagging_inner_irbb,
          inner_video_diffs = inner_video_lag_diffs,
          assignmnt_type = "lag",
          movement_inference = "post-motion_trigger"
        ) %>%
        # Add back metadata about the beam breaker event labels
        dplyr::inner_join(
          labeled_irbb %>%
            dplyr::select(all_of(inner_irbb_col), all_of(irbb_event_col), all_of(irbb_unique_col)),
          by = c(all_of(inner_irbb_col))
        ) %>%
        # Then filter for video detections that match these beam breaker events
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lead_withn)
        ) %>%
        dplyr::select(all_of(inner_irbb_col), all_of(video_col), all_of(irbb_event_col), inner_video_diffs, assignmnt_type, movement_inference)
    ) %>% 
    # Make sure to add metadata columns for this integration step
    dplyr::mutate(
      integration_method = method,
      data_stage = "integration",
      lower_threshold_s = l_th,
      upper_threshold_s = u_th,
      video_recording_duration_s = video_rec_dur,
      date_integrated = paste(Sys.Date(), Sys.time(), sep = " ")
    ) %>% 
    # Add back general metadata from the beam breaker data
    dplyr::inner_join(
      labeled_irbb %>%
        dplyr::select(all_of(general_metadata_cols), all_of(inner_irbb_col), all_of(outer_irbb_col), all_of(irbb_unique_col)),
      by = c(all_of(inner_irbb_col))
    ) %>%
    dplyr::rename(
      # Had to rename this column for the join below
      !!timestamps_col := !!sym(video_col)
    ) %>%
    # Add back metadata about the video recording events
    dplyr::inner_join(
      preproc_video %>%
        dplyr::select(all_of(video_metadata_cols), timestamps_col),
      by = timestamps_col
    ) %>% 
    dplyr::rename(
      !!video_col := !!sym(timestamps_col)
    ) %>% 
    dplyr::select(all_of(general_metadata_cols), all_of(outer_irbb_col), all_of(inner_irbb_col), all_of(video_col), all_of(irbb_event_col), all_of(irbb_unique_col), inner_video_diffs, assignmnt_type, movement_inference, integration_method, all_of(video_metadata_cols), data_stage, lower_threshold_s, upper_threshold_s, video_recording_duration_s, date_integrated) %>% 
    dplyr::arrange(!!sym(inner_irbb_col), desc = FALSE)
  
  #### Handle duplicates
  
  # The same inner beam breaker timestamp should NOT be assigned to multiple video timestamps, regardless of whether this was motion or post-motion assignment
  # The same video timestamp CAN be assigned to different inner beam breaker timestamps, since some inner beam breaker detections may have happened after the original movement that triggered the video recording
  # To remove inner beam breaker duplicate assignments, I need to use a temporal rule to retain the inner beam breaker and video timestamps closest together in time. And drop the other matches as duplicates. This should be applied regardless of whether or not the assignment is motion trigger or post-motion trigger. Even for post motion trigger, the inner beam breaker and video timestamps closest together in time should be retained
  
  dup_inds <- which(duplicated(integr8d_df[[inner_irbb_col]]))
  
  if(length(dup_inds) > 0){
    
    # Return the rows to retain
    tmp_df <- data.table::rbindlist(lapply(1:length(dup_inds), function(i){
      
      # For each inner beam breaker timestamp that is present more than once, retain the integrated event that represents the closest match (e.g. the smallest temporal difference) between the inner beam breaker and video timestamps
      tmp_dup <- integr8d_df %>% 
        slice(dup_inds[i]) %>% 
        pull(!!sym(inner_irbb_col))
      
      return(
        integr8d_df %>% 
          dplyr::filter(
            !!sym(inner_irbb_col) == tmp_dup
          ) %>% 
          dplyr::arrange(-desc(abs(inner_video_diffs))) %>% 
          slice(1)
      )
      
    }))
    
    # Get the indices of all of the duplicated rows
    all_dup_inds <- unlist(lapply(1:length(dup_inds), function(i){
      
      tmp_dup <- integr8d_df %>% 
        slice(dup_inds[i]) %>% 
        pull(!!sym(inner_irbb_col))
      
      return(
        integr8d_df %>% 
          rowid_to_column() %>% 
          dplyr::filter(
            !!sym(inner_irbb_col) == tmp_dup
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
      dplyr::arrange(!!sym(inner_irbb_col), desc = FALSE)
    
  } else {
    integr8d_df_noDups <- integr8d_df
  }
  
  write.csv(integr8d_df, file.path(path, out_dir, "integrated_beamBreaker_video_data.csv"), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
