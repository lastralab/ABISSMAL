#' @title integrate_rfid_beamBreakers_video
#' @description Use lags between the pre-processed radio frequency identification (RFID) data, the pre-processed, labeled beam breaker data, and the pre-processed video data to integrate these three datasets. Each RFID detection that remains must be accompanied by a labeled event from the beam breaker dataset (e.g. an entrance or exit event) and the video dataset
#' 
#' @param rfid_file_nm A character string. This should be the name of the file that contains all of the pre-processed RFID detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the RFID data in the subsequent arguments
#' @param irbb_file_nm A character string. This should be the name of the file that contains all of the pre-processed infrared beam breaker (IRBB) detections. Each row is a unique detection event. This data frame must contain all the columns specified for the IRBB data in the subsequent arguments
#' @param video_file_nm A character string. This should be the name of the file that contains all of the pre-processed video detections. Each row is a unique detection event. This data frame must contain all the columns specified for the video data in the subsequent arguments
#' @param second_integration A character string. This argument should be set to "rfid-video" or "irbb-video", to denote how the video data will be integrated. If "rfid-video" is specified, then the video data will be integrated by identifying RFID and video detections that occurred within the given temporal thresholds l2_th and u2_th. If "irbb-video" is specified, then the video data will be integrated by identifying inner beam breaker and video detections that occurred within these temporal thresholds
#' @param integrate_perching Boolean. If TRUE, then the perching events identified using `find_perching_events` will be part of the second integration. When the second integration is set to "rfid-video", then this perching events are integrated by finding RFID timestamps that occurred within the duration of a perching event. When the second integration is set to "irbb-video", then this perching events are integrated by finding inner beam breaker timestamps that occurred within the duration of a perching event. If FALSE, then perching events will not be integrated.
#' @param l1_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify RFID and beam breaker events that are close enough together for integration
#' @param u1_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify RFID and beam breaker events that are close enough together for integration
#' #' @param l2_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify RFID and video events, or inner beam breaker and video, events that are close enough together for integration. This threshold should be specific to how the integration step will be performed (see the argument `second_integration`)
#' @param u2_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify RFID and video events, or inner beam breaker and video, events that are close enough together for integration. This threshold should be specific to how the integration step will be performed (see the argument `second_integration`)
#' @param video_rec_dur A numeric argument. This represents the duration of video recording (post-motion detection) in seconds. This argument, along with the upper temporal threshold above (u_th) is used to identify beam breaker events that occurred during video recording, but are inferred to not represent the original movements that triggered the movement sensors. This argument facilitates retaining beam breaker events that occurred during the span of video recording and therefore did not trigger a separate video recording event. The default is NULL, but this must be a numeric value when `method` is set to "temporal"
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param PIT_tag_col A character value. This is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID")
#' @param outer_irbb_col A character value. The column name that contains timestamps for the outer pair of beam breakers (e.g. the first pair of beam breakers that an animal encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param inner_irbb_col A character value. The column name that contains timestamps for the inner pair of beam breakers (e.g. the second pair of beam breakers that an individual encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param irbb_event_col A character value. The name of column that contains the type of beam breaker event (e.g. "entrance" or "exit)
#' @param irbb_unique_col A character value. The name of column that contains the unique numeric identifier for each beam breaker event
#' @param preproc_metadata_cols A character vector. This should be a string of the metadata column names from pre-processing that should be dropped from either or both data frames. For instance, c("thin_threshold_s", "data_stage", "date_pre_processed", "lower_threshold_s", "upper_threshold_s", "date_labeled")
#' @param general_metadata_cols A character vector. This should be a string of the general metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("chamber_id", "year", "month", "day"). These columns will be added as the first columns in the integrated data frame, in the same order in which they are provided
#' @param video_metadata_cols A character vector. This should be a string of the video metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"). These columns will be added as later columns in the integrated data frame, in the same order in which they are provided
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param rfid_dir A character string. This should be the name of directory where the pre-processed RFID data is saved across sensors inside the path above. For instance, "pre-processed"
#' #' @param irbb_dir A character string. This should be the name of directory where the pre-processed and labeled beam breaker data is saved across sensors inside the path above. For instance, "pre-processed"
#' @param video_dir A character string. This should be the name of directory where the pre-processed video data is saved across sensors inside the path above. For instance, "pre-processed"
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param out_file_nm A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default is "integrated_rfid_beamBreakers_video_data.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details This RFID, beam breaker, and video integration is a separate function because the way in which the sensors are set up to detect movement determines how the lag calculations and integration should be performed. In other words, it is difficult to make a general function to integrate data collected across any two types of sensors used in the tracking system. This function was written to integrate data across 1 RFID antenna and the outer pair of beam breakers mounted around the entrance of a nest container that was designed for zebra finches. For nest container entrance events, the integration is done by finding RFID timestamps that occurred within the lower to upper thresholds after the outer beam breaker. For nest container exit events, the integration is done by finding RFID timestamps that occurred within the lower to upper thresholds before the outer beam breaker. This matching is less strict than trying to find sequences of events in which the outer beam breakers, RFID antenna, and inner beam breakers triggered in that exact order (given that the way in which birds arrive or perch in the entrance can lead to variation in this expected sequence). In other words, these events will not represent perfect sequences of outer beam breakers, then RFID, then inner beam breakers and video recording triggering, but rather, RFID detections that occurred within the expected thresholds and before or after an outer beam breaker event that was already matched to an inner beam breaker event, and then inner beam breaker events matched to a video recording event.
#' This function integrates detections across these 2 sensor types regardless of whether or not these detections occurred during perching events captured by the RFID antenna (see `find_rfid_perching_events`). The reason for this is that some perching events may have started or ended as entrance or exit events, and it's important to retain those events at this stage. If it becomes important later to remove behavioral events that were associated with longer perching events, then this can be done by filtering out detections from the integrated dataset that overlap in time with perching events.
#' 
#' @return A .csv file with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the RFID antenna, the lead and rear beam breaker pairs, a unique label for the given event (e.g. entrance or exit), a unique numeric identifier for the given event, and information about the given data processing stage. Each row in the .csv file is an RFID detection that was integrated with a labeled event across the outer and inner beam breaker pairs. Information about the temporal thresholds used for the integration and the date that the data was integrated is also contained in this spreadsheet.
#' 

integrate_rfid_beamBreakers_video <- function(rfid_file_nm, irbb_file_nm, video_file_nm, second_integration, integrate_perching, l1_th, u1_th, l2_th, u2_th, video_rec_dur, sensor_id_col, timestamps_col, PIT_tag_col, outer_irbb_col, inner_irbb_col, irbb_event_col, irbb_unique_col, preproc_metadata_cols, general_metadata_cols, video_metadata_cols, path, data_dir, out_dir, out_file_nm = "integrated_rfid_beamBreakers_video_data.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Check that all temporal thresholds are numeric
  if(!is.numeric(l1_th) | !is.numeric(l2_th)){
    stop('One or both of the lower temporal thresholds is not numeric')
  }
  
  if(!is.numeric(u1_th) | !is.numeric(u2_th)){
    stop('One or both of the upper temporal thresholds is not numeric)')
  }
  
  if(!is.numeric(video_rec_dur)){
    stop('The post-video recording temporal threshold needs to be numeric (in seconds)')
  }
  
  # First do the RFID to outer beam breaker integration
  tmp_file <- "tmp_rfid_irbb_integration.csv"
  
  integrate_rfid_beamBreakers(
    rfid_file_nm, 
    irbb_file_nm, 
    l_th = l1_th, 
    u_th = u1_th, 
    sensor_id_col, 
    timestamps_col, 
    PIT_tag_col, 
    outer_irbb_col, 
    inner_irbb_col, 
    irbb_event_col, 
    irbb_unique_col, 
    preproc_metadata_cols, 
    integrate_perching = FALSE,
    path, 
    rfid_dir, 
    irbb_dir, 
    out_dir = "tmp", 
    out_file_nm = tmp_file, 
    tz, 
    POSIXct_format = "%Y-%m-%d %H:%M:%OS"
  )

  # Then feed this RFID beam breaker integration data to either the rfid + video or beam breakers + video functions
  if(grepl("rfid-video", second_integration)){
    
    # Change the name of the RFID column to timestamps_col, and add a sensor_id column with "RFID" to this .csv to facilitate the video integration below
    read.csv(file.path(path, "tmp", tmp_file), header = TRUE) %>% 
      dplyr::mutate(
        sensor_id = "RFID"
      ) %>% 
      dplyr::rename(
        !!timestamps_col := "RFID"
      ) %>% 
      write.csv(file.path(path, "tmp", tmp_file), row.names = FALSE)
    
    # Then do the RFID + video integration
    tmp_out_file <- "tmp_rfid_irbb_video_integration.csv"
    
    integrate_rfid_video(
      rfid_file_nm = tmp_file, 
      video_file_nm,
      l_th = l2_th, 
      u_th = u2_th, 
      video_rec_dur, 
      sensor_id_col, 
      timestamps_col, 
      PIT_tag_col, 
      preproc_metadata_cols = c("data_stage", "date_integrated"),
      general_metadata_cols = c("chamber_id", "year", "month", "day"),
      extra_cols2drop = c("Outer_beam_breaker", "Inner_beam_breaker", "outer_rfid_diffs", "rfid_irbb_assignmnt_type", "rfid_irbb_lower_threshold_s", "rfid_irbb_upper_threshold_s"), 
      video_metadata_cols,
      integrate_perching,
      path, 
      rfid_dir = "tmp", 
      video_dir, 
      out_dir = "tmp", 
      out_file_nm = tmp_out_file,
      tz, 
      POSIXct_format = "%Y-%m-%d %H:%M:%OS"
    )
    
    # Rearrange columns and move to the integrated directory with a different name
    read.csv(file.path(path, "tmp", tmp_out_file ), header = TRUE) %>%
      dplyr::select(
        all_of(general_metadata_cols), 
        RFID, 
        all_of(outer_irbb_col),
        all_of(inner_irbb_col),
        Camera, 
        all_of(PIT_tag_col),
        all_of(irbb_event_col),
        all_of(irbb_unique_col),
        rfid_video_direction_inferred,
        rfid_video_movement_inference,
        all_of(video_metadata_cols),
        outer_rfid_diffs,
        rfid_video_diffs,
        rfid_irbb_assignmnt_type,
        rfid_video_assignmnt_type,
        rfid_irbb_lower_threshold_s,
        rfid_irbb_upper_threshold_s,
        rfid_video_lower_threshold_s,
        rfid_video_upper_threshold_s,
        video_recording_duration_s,
        data_stage,
        date_integrated
      ) %>% 
      write.csv(file.path(path, out_dir, out_file_nm), row.names = FALSE)
    
  } else if(grepl("irbb-video", second_integration)){
    
    # Do the beam breaker + video integration
    integrate_beamBreakers_video(
      irbb_file_nm = tmp_file, 
      video_file_nm,
      l_th = l2_th, 
      u_th = u2_th, 
      video_rec_dur, 
      sensor_id_col, 
      timestamps_col, 
      PIT_tag_col, 
      outer_irbb_col, 
      inner_irbb_col, 
      irbb_event_col, 
      irbb_unique_col, 
      preproc_metadata_cols = c("data_stage", "date_integrated"),
      general_metadata_cols = c("chamber_id", "year", "month", "day"),
      extra_cols2drop = c("RFID", "PIT_tag_ID", "outer_rfid_diffs", "rfid_irbb_assignmnt_type", "rfid_irbb_lower_threshold_s", "rfid_irbb_upper_threshold_s"), 
      video_metadata_cols,
      integrate_perching,
      path, 
      irbb_dir = "tmp", 
      video_dir, 
      out_dir = "tmp", 
      out_file_nm = "tmp_rfid_irbb_video_integration.csv",
      tz, 
      POSIXct_format = "%Y-%m-%d %H:%M:%OS"
    )
    
    # Rearrange columns and move to the integrated directory with a different name
    read.csv(file.path(path, "tmp", tmp_out_file), header = TRUE) %>%
      dplyr::select(
        all_of(general_metadata_cols), 
        RFID, 
        all_of(outer_irbb_col),
        all_of(inner_irbb_col),
        Camera, 
        all_of(PIT_tag_col),
        all_of(irbb_event_col),
        all_of(irbb_unique_col),
        rfid_video_movement_inference,
        all_of(video_metadata_cols),
        outer_rfid_diffs,
        inner_video_diffs,
        rfid_irbb_assignmnt_type,
        irbb_video_assignmnt_type,
        rfid_irbb_lower_threshold_s,
        rfid_irbb_upper_threshold_s,
        irbb_video_lower_threshold_s,
        irbb_video_upper_threshold_s,
        video_recording_duration_s,
        data_stage,
        date_integrated
      ) %>% 
      write.csv(file.path(path, out_dir, out_file_nm), row.names = FALSE)
    
  }

  # Then delete the tmp directory and all temporary files within this directory
  # This ensures that each integration will be performed from scratch (e.g. avoid mistakenly using old temporary files from previous integrations)
  unlink(file.path(path, "tmp"), recursive = TRUE)
  
  # Reset the current global options
  options(orig_opts)
  
}

