#' @title integrate_rfid_video
#' @description Use lags between the pre-processed radio frequency identification (RFID) data and the pre-processed video recording events to integrate these two datasets. Each RFID detection that remains must be accompanied by a video recording event (e.g. the RFID detection occurred within a certain time before the recording onset or during a video recording)
#' 
#' @param rfid_file_nm A character string. This should be the name of the file that contains all of the pre-processed RFID detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the RFID data in the subsequent arguments
#' @param irbb_file_nm A character string. This should be the name of the file that contains all of the pre-processed video detections. Each row is a unique detection event. This data frame must contain all the columns specified for the video data in the subsequent arguments
#' @param l_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify RFID and video recording events that are close enough together for integration. This argument is used to search for RFID detections that occurred before a given video recording event
#' @param u_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify RFID and video recording events that are close enough together for integration. This argument is used to search for RFID detections that occurred before a given video recording event
#' @param p_th A numeric argument. This represents a temporal threshold in seconds to identify RFID events that occurred just after video recording onset, but still during the duration of the video recording, and can be integrated with the given video recording event. This third temporal threshold facilitates integration of RFID events that may have occurred during the span of video recording and therefore did not trigger a separate video recording event
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param PIT_tag_col A character value. This is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID")
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the raw data is saved across sensors inside the path above. For instance, "raw_combined".
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details This RFID and video integration is a separate function because the way in which the sensors are set up to detect movement determines how the lag calculations and integration should be performed. In other words, it is difficult to make a general function to integrate data collected across any two types of sensors used in the tracking system. This function was written to integrate data across 1 RFID antenna and 1 camera mounted on a nest container that was designed for zebra finches. The RFID antenna sits in the middle of a circular entrance, and the camera records into the center of the nest container from above. This function integrates detections across these 2 sensor types regardless of whether or not these detections occurred during perching events captured by the RFID antenna (see `find_rfid_perching_events`). The reason for this is that some perching events may have started or ended as entrance or exit events, and it's important to retain those events at this stage. If it becomes important later to remove behavioral events that were associated with longer perching events, then this can be done by filtering out detections from the integrated dataset that overlap in time with perching events.
#' 
#' @return A .csv file with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the RFID antenna, the video recording events, and information about the given data processing stage. Each row in the .csv file is an RFID detection that was integrated with a video recording event. Information about the temporal thresholds used for the integration and the date that the data was integrated is also contained in this spreadsheet.
#' 

integrate_rfid_video <- function(rfid_file_nm, video_file_nm, l_th, u_th, p_th, sensor_id_col, timestamps_col, PIT_tag_col, path, data_dir, out_dir, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Check that all temporal thresholds are each numeric
  if(!is.numeric(l_th)){
    stop('The pre-video recording lower temporal threshold needs to be numeric (in seconds)')
  }
  
  if(!is.numeric(u_th)){
    stop('The pre-video recording upper temporal threshold needs to be numeric (in seconds)')
  }
  
  if(!is.numeric(p_th)){
    stop('The post-video recording temporal threshold needs to be numeric (in seconds)')
  }
  
  # Create the directory for saving the integrated data files if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the pre-processed RFID data
  preproc_rfid <- read.csv(file.path(path, data_dir, rfid_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    # Drop columns that aren't needed here
    dplyr::select(-c("thin_threshold_s", "data_stage", "date_pre_processed"))
  
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
    dplyr::select(-c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name", "data_stage", "date_pre_processed"))
  
  # Check that the input data are both data frames
  if(!is.data.frame(preproc_rfid)){
    stop('The RFID data needs to be a data frame')
  }
  
  if(!is.data.frame(preproc_video2)){
    stop('The Video data needs to be a data frame')
  }
  
  # Group the RFID data frame
  rfid_df_tmp <- preproc_rfid %>% 
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
  
  # Get the sensor ID value for the RFID data, which will be a column name below
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
          # Calculate the differences between the relevant pairs of timestamps: RFID compared to each video event
          # The lags are calculated per group in the grouped data frame
          dplyr::mutate(
            # Here negative differences mean the RFID antenna triggered first
            rfid_pre_video_diffs = round(as.numeric(lagging_RFID - !!sym(video_col) ), 2),
            # Here positive differences mean the camera triggered first
            rfid_post_video_diffs = round(as.numeric(leading_RFID - !!sym(video_col)), 2)
          ) %>% 
          # Convert differences to boolean based on a threshold (in seconds)
          dplyr::mutate(
            binary_vals_pre = (
              rfid_pre_video_diffs <= -l_th & 
                rfid_pre_video_diffs >= -u_th
            ),
            binary_vals_post = (
              rfid_post_video_diffs >= 0 & 
                rfid_post_video_diffs <= p_th
            )
          )
      )
    )
  
  # Do more mapping to perform the integration per PIT tag depending on the given temporal thresholds
  integr8d_df <- lags_grpd %>% 
    dplyr::mutate(
      # Pre-video lags
      matched_rfid_video = map(
        .x = lags, 
        .f = ~ dplyr::mutate(
          .x,
          !!rfid_col := lagging_RFID,
          rfid_video_diffs = rfid_pre_video_diffs,
          type = "pre-video_recordingOnset"
        ) %>% 
          dplyr::filter(
            binary_vals_pre & !is.na(binary_vals_pre)
          ) %>% 
          dplyr::select(rfid_col, video_col, rfid_video_diffs, type) 
        %>% 
          # Post-video lags
          bind_rows(
            .x %>% 
              dplyr::mutate(
                !!rfid_col := leading_RFID,
                rfid_video_diffs = rfid_post_video_diffs,
                type = "post-video_recordingOnset"
              ) %>% 
              dplyr::filter(
                binary_vals_post & !is.na(binary_vals_post)
              ) %>% 
              dplyr::select(rfid_col, video_col, rfid_video_diffs, type)
          )
      )
    ) %>% 
    dplyr::select(-c(data, lags)) %>% 
    unnest(`cols` = c(matched_rfid_video)) %>%
    ungroup() %>%
    # Make sure to add metadata columns for this integration step
    dplyr::mutate(
      data_stage = "integration",
      lower_preVideo_threshold_s = l_th,
      upper_preVideo_threshold_s = u_th,
      postVideo_threshold_s = p_th,
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
        dplyr::select("chamber_id", "year", "month", "day", timestamps_col, "total_pixels_motionTrigger", "pixel_threshold", "video_file_name"),
      by = timestamps_col
    ) %>% 
    dplyr::rename(
      !!video_col := !!sym(timestamps_col)
    ) %>% 
    dplyr::select("chamber_id", "year", "month", "day", rfid_col, video_col, PIT_tag_col, "rfid_video_diffs", "type", "data_stage", "lower_preVideo_threshold_s", "upper_preVideo_threshold_s", "postVideo_threshold_s", "date_integrated") %>% 
    dplyr::arrange(!!sym(rfid_col), desc = FALSE)
  
  # There may be duplicated timestamps if RFID detections were assigned to pre and post video recording events. Given the way the lags were calculated, it isn't possible to find these duplicates using the columns of binary values. Find these duplicates and retain only the pre-video recording event
  dup_inds <- which(duplicated(integr8d_df$RFID))
  
  if(length(dup_inds) > 0){
    
    # Return the rows to retain
    tmp_df <- data.table::rbindlist(lapply(1:length(dup_inds), function(i){
      
      tmp_dup <- integr8d_df %>% 
        slice(dup_inds[i]) %>% 
        pull(RFID)
      
      return(
        integr8d_df %>% 
          dplyr::filter(
            RFID == tmp_dup
          ) %>% 
          dplyr::filter(type == "pre-video_recordingOnset")
      )
      
    }))
    
    # Get the indices of all of the duplicated rows
    all_dup_inds <- unlist(lapply(1:length(dup_inds), function(i){
      
      tmp_dup <- integr8d_df %>% 
        slice(dup_inds[i]) %>% 
        pull(RFID)
      
      return(
        integr8d_df %>% 
          rowid_to_column() %>% 
          dplyr::filter(
            RFID == tmp_dup
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
    
    # Checking, looks good
    # any(duplicated(integr8d_df_noDups$RFID))
    # (nrow(integr8d_df) - nrow(integr8d_df_noDups)) == length(dup_inds)
    
  } else {
    integr8d_df_noDups <- integr8d_df
  }
  
  # Save the pre-processed data for each sensor in the given setup
  write.csv(integr8d_df_noDups, file.path(path, out_dir, "integrated_rfid_video_data.csv"), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
