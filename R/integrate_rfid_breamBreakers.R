#' @title integrate_rfid_beamBreakers
#' @description Use lags between the pre-processed radio frequency identification (RFID) data and the pre-processed, labeled beam breaker data to integrate these two datasets. Each RFID detection that remains must be accompanied by a labeled event from the beam breaker dataset (e.g. an entrance or exit event)
#' 
#' @param rfid_file_nm A character string. This should be the name of the file that contains all of the pre-processed RFID detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the RFID data in the subsequent arguments
#' @param irbb_file_nm A character string. This should be the name of the file that contains all of the pre-processed infrared beam breaker (IRBB) detections. Each row is a unique detection event. This data frame must contain all the columns specified for the IRBB data in the subsequent arguments
#' @param l_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify RFID and beam breaker events that are close enough together for integration. The default is NULL, but this must be a numeric value when `method` is set to "temporal"
#' @param u_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify RFID and beam breaker events that are close enough together for integration. The default is NULL, but this must be a numeric value when `method` is set to "temporal"
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param PIT_tag_col A character value. This is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID")
#' @param outer_irbb_col A character value. The column name that contains timestamps for the outer pair of beam breakers (e.g. the first pair of beam breakers that an animal encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param inner_irbb_col A character value. The column name that contains timestamps for the inner pair of beam breakers (e.g. the second pair of beam breakers that an individual encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param irbb_event_col A character value. The name of column that contains the type of beam breaker event (e.g. "entrance" or "exit)
#' @param irbb_unique_col A character value. The name of column that contains the unique numeric identifier for each beam breaker event
#' @param preproc_metadata_cols A character vector. This should be a string of the metadata column names from pre-processing that should be dropped. For instance, c("thin_threshold_s", "data_stage", "date_pre_processed")
#' @param general_metadata_cols A character vector. This should be a string of the general metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("chamber_id", "year", "month", "day"). These columns will be added as the first columns in the integrated data frame, in the same order in which they are provided
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param rfid_dir A character string. This should be the name of directory where the pre-processed RFID data is saved across sensors inside the path above. For instance, "pre-processed"
#' #' @param irbb_dir A character string. This should be the name of directory where the pre-processed and labeled beam breaker data is saved across sensors inside the path above. For instance, "pre-processed"
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param out_file_nm A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default is "integrated_rfid_beamBreaker_data.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details This RFID and beam breaker integration is a separate function because the way in which the sensors are set up to detect movement determines how the lag calculations and integration should be performed. In other words, it is difficult to make a general function to integrate data collected across any two types of sensors used in the tracking system. This function was written to integrate data across 1 RFID antenna and the outer pair of beam breakers mounted around the entrance of a nest container that was designed for zebra finches. For nest container entrance events, the integration is done by finding RFID timestamps that occurred within the lower to upper thresholds after the outer beam breaker. For nest container exit events, the integration is done by finding RFID timestamps that occurred within the lower to upper thresholds before the outer beam breaker. This matching is less strict than trying to find sequences of events in which the outer beam breakers, RFID antenna, and inner beam breakers triggered in that exact order (given that the way in which birds arrive or perch in the entrance can lead to variation in this expected sequence). In other words, these events will not represent perfect sequences of outer beam breakers, then RFID, then inner beam breakers triggering, but rather, RFID detections that occurred within the expected thresholds and before or after an outer beam breaker event that was already matched to an inner beam breaker event.
#' This function integrates detections across these 2 sensor types regardless of whether or not these detections occurred during perching events captured by the RFID antenna (see `find_rfid_perching_events`). The reason for this is that some perching events may have started or ended as entrance or exit events, and it's important to retain those events at this stage. If it becomes important later to remove behavioral events that were associated with longer perching events, then this can be done by filtering out detections from the integrated dataset that overlap in time with perching events.
#' 
#' @return A .csv file with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the RFID antenna, the lead and rear beam breaker pairs, a unique label for the given event (e.g. entrance or exit), a unique numeric identifier for the given event, and information about the given data processing stage. Each row in the .csv file is an RFID detection that was integrated with a labeled event across the outer and inner beam breaker pairs. Information about the temporal thresholds used for the integration and the date that the data was integrated is also contained in this spreadsheet.
#' 

integrate_rfid_beamBreakers <- function(rfid_file_nm, irbb_file_nm, l_th = NULL, u_th = NULL, sensor_id_col, timestamps_col, PIT_tag_col, outer_irbb_col, inner_irbb_col, irbb_event_col, irbb_unique_col, preproc_metadata_cols, path, rfid_dir, irbb_dir, out_dir, out_file_nm = "integrated_rfid_beamBreaker_data.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Check that the lower and upper temporal thresholds are each numeric when using the temporal method
  if(!is.numeric(l_th)){
    stop('The lower temporal threshold needs to be numeric (in seconds)')
  }
  
  if(!is.numeric(u_th)){
    stop('The upper temporal threshold needs to be numeric (in seconds)')
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
    # Drop columns that aren't needed here
    dplyr::select(-c(all_of(preproc_metadata_cols)))
  
  # Read in the pre-processed and labeled beam breaker data
  labeled_irbb <- read.csv(file.path(path, irbb_dir, irbb_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!outer_irbb_col := as.POSIXct(format(as.POSIXct(!!sym(outer_irbb_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      !!inner_irbb_col := as.POSIXct(format(as.POSIXct(!!sym(inner_irbb_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    # Drop columns that aren't needed here
    dplyr::select(-c("data_stage", "temporal_threshold_s", "date_labeled"))
  
  # Check that the input data are both data frames
  if(!is.data.frame(preproc_rfid)){
    stop('The RFID data needs to be a data frame')
  }
  
  if(!is.data.frame(labeled_irbb)){
    stop('The IRBB data needs to be a data frame')
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
  
  # Lengthen the IRBB data frame. Make sure this has the same columns in the same order as the RFID data for the row binding and calculations below
  irbb_df_tmp <- labeled_irbb %>% 
    pivot_longer(
      cols = c(all_of(outer_irbb_col), all_of(inner_irbb_col)),
      names_to = sensor_id_col,
      values_to = timestamps_col
    ) %>% 
    dplyr::mutate(
      group_col = NA
    ) %>% 
    dplyr::select(names(rfid_df_tmp$data[[1]]))
  
  # Do the timestamp difference calculations
  lags_grpd <- rfid_df_tmp %>% 
    dplyr::mutate(
      # Here the mapping structure sets up running the following code for each group (PIT taq ID) in the RFID data frame
      lags = map(
        .x = data,
        # Here I'm interested in aligning to the outer pair of beam breakers only
        .y = irbb_df_tmp %>%
          dplyr::filter(!!sym(sensor_id_col) == outer_irbb_col),
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
          # Calculate the differences between the relevant pairs of timestamps: RFID compared to the outer beam breaker pair to find entrances and exits
          # The lags are calculated per group in the grouped data frame
          dplyr::mutate(
            # For both the lagging and leading calculations, negative differences mean that the RFID antenna triggered first, while positive differences mean that outer beam breaker pair triggered first
            outer_rfid_lead_diffs = round(as.numeric(leading_RFID - !!sym(outer_irbb_col)), 2),
            outer_rfid_lag_diffs = round(as.numeric(lagging_RFID - !!sym(outer_irbb_col)), 2)
          ) %>%
          # Convert these differences to Boolean values based on a threshold (in seconds). Inverting these conditionals to match the negative differences that should indicate an entrance event
          dplyr::mutate(
            # To search for entrances, look for RFID detections that came within the given l_th or u_th AFTER an outer beam breaker timestamp. Set up these conditionals for both the lead and lag calculations
            binary_lead_outer_ent = (
              # RFID after, so positive differences
              outer_rfid_lead_diffs >= l_th & outer_rfid_lead_diffs <= u_th
            ),
            binary_lag_outer_ent = (
              # RFID after, so positive differences
              outer_rfid_lag_diffs >= l_th & outer_rfid_lag_diffs <= u_th
            ),
            # Then to search for exits, look for RFID detections that came within the given l_th or u_th BEFORE an outer beam breaker timestamp. Again, set up these conditionals for both the lead and lag calculations
            binary_lead_outer_exi = (
              # RFID before, so negative differences
              outer_rfid_lead_diffs <= -l_th & outer_rfid_lead_diffs >= -u_th
            ),
            binary_lag_outer_exi = (
              # RFID before, so negative differences
              outer_rfid_lag_diffs <= -l_th & outer_rfid_lag_diffs >= -u_th
            )
          ) %>% 
          # Drop all rows with NA values across these binary columns
          dplyr::filter(
            !dplyr::if_all(
              c(
                binary_lead_outer_ent,
                binary_lag_outer_ent,
                binary_lead_outer_exi,
                binary_lag_outer_exi
              ), 
              is.na
            )
          ) %>% 
          # Add back metadata about the beam breaker event labels
          dplyr::inner_join(
            labeled_irbb %>%
              dplyr::select(all_of(outer_irbb_col), all_of(irbb_event_col)),
            by = c(all_of(outer_irbb_col))
          )
      )
    )
  
  conditnal_lead_ent <- "binary_lead_outer_ent & !is.na(binary_lead_outer_ent)"
  conditnal_lead_exi <- "binary_lead_outer_exi & !is.na(binary_lead_outer_exi)"
  
  conditnal_lag_ent <- "binary_lag_outer_ent & !is.na(binary_lag_outer_ent)"
  conditnal_lag_exi <- "binary_lag_outer_exi & !is.na(binary_lag_outer_exi)"
  
  # Do more mapping to perform the integration depending on the given lower and upper temporal thresholds
  # This is done per PIT tag, and the integration is done separately for entrances and exits from each of the leading and lagging calculations
  integr8d_df <- lags_grpd %>% 
    dplyr::mutate(
      # Entrances, lead differences
      matched_irbb_rfid = map(
        .x = lags, 
        .f = ~ dplyr::mutate(
          .x,
          !!rfid_col := leading_RFID,
          outer_rfid_diffs = outer_rfid_lead_diffs,
          rfid_irbb_assignmnt_type = "lead"
        ) %>% 
          # Filter for entrances among the labeled beam breaker events 
          dplyr::filter(
            !!sym(irbb_event_col) == "entrance"
          ) %>% 
          # Then filter for RFID detections that match these beam breaker events
          dplyr::filter(
            !!rlang::parse_expr(conditnal_lead_ent)
          ) %>% 
          dplyr::select(all_of(outer_irbb_col), all_of(rfid_col), outer_rfid_diffs, rfid_irbb_assignmnt_type)
        %>%
          # Entrances, lag differences
          bind_rows(
            .x %>%
              dplyr::mutate(
                !!rfid_col := lagging_RFID,
                outer_rfid_diffs = outer_rfid_lag_diffs,
                rfid_irbb_assignmnt_type = "lag"
              ) %>%
              # Filter for entrances among the labeled beam breaker events 
              dplyr::filter(
                !!sym(irbb_event_col) == "entrance"
              ) %>% 
              # Then filter for RFID detections that match these beam breaker events
              dplyr::filter(
                !!rlang::parse_expr(conditnal_lag_ent)
              ) %>%
              dplyr::select(all_of(outer_irbb_col), all_of(rfid_col), outer_rfid_diffs, rfid_irbb_assignmnt_type)
          ) %>%
          # Exits, lead differences
          bind_rows(
            .x %>%
              dplyr::mutate(
                !!rfid_col := leading_RFID,
                outer_rfid_diffs = outer_rfid_lead_diffs,
                rfid_irbb_assignmnt_type = "lead"
              ) %>%
              # Filter for exits among the labeled beam breaker events 
              dplyr::filter(
                !!sym(irbb_event_col) == "exit"
              ) %>% 
              # Then filter for RFID detections that match these beam breaker events
              dplyr::filter(
                !!rlang::parse_expr(conditnal_lead_exi)
              ) %>%
              dplyr::select(all_of(outer_irbb_col), all_of(rfid_col), outer_rfid_diffs, rfid_irbb_assignmnt_type)
          ) %>%
          # Exits, lag differences
          bind_rows(
            .x %>%
              dplyr::mutate(
                !!rfid_col := lagging_RFID,
                outer_rfid_diffs = outer_rfid_lag_diffs,
                rfid_irbb_assignmnt_type = "lag"
              ) %>%
              # Filter for exits among the labeled beam breaker events 
              dplyr::filter(
                !!sym(irbb_event_col) == "exit"
              ) %>% 
              # Then filter for RFID detections that match these beam breaker events
              dplyr::filter(
                !!rlang::parse_expr(conditnal_lag_exi)
              ) %>%
              dplyr::select(all_of(outer_irbb_col), all_of(rfid_col), outer_rfid_diffs, rfid_irbb_assignmnt_type)
          )
      )
    ) %>% 
    dplyr::select(-c(data, lags)) %>% 
    unnest(`cols` = c(matched_irbb_rfid)) %>%
    ungroup() %>% 
    # Make sure to add metadata columns for this integration step
    dplyr::mutate(
      data_stage = "integration",
      rfid_irbb_lower_threshold_s = l_th,
      rfid_irbb_upper_threshold_s = u_th,
      date_integrated = paste(Sys.Date(), Sys.time(), sep = " ")
    ) %>% 
    dplyr::rename(
      !!PIT_tag_col := `group_col`
    ) %>% 
    # Add back metadata about the beam breaker events and general metadata
    dplyr::inner_join(
      labeled_irbb %>%
        dplyr::select(-c("data_type")),
      by = c(all_of(outer_irbb_col))
    ) %>%
    dplyr::select(chamber_id, year, month, day, all_of(rfid_col), all_of(outer_irbb_col), all_of(inner_irbb_col), all_of(PIT_tag_col), all_of(irbb_event_col), all_of(irbb_unique_col), outer_rfid_diffs, rfid_irbb_assignmnt_type, data_stage, rfid_irbb_lower_threshold_s, rfid_irbb_upper_threshold_s, date_integrated) %>% 
    dplyr::arrange(!!sym(rfid_col), desc = FALSE)
  
  # There may be duplicated timestamps if RFID detections were assigned to pre and post video recording events. Given the way the lags were calculated, it isn't possible to find these duplicates using the columns of binary values. Find these duplicates and retain only the pre-video recording event
  dup_inds <- which(duplicated(integr8d_df[[rfid_col]]))
  
  if(length(dup_inds) > 0){
    
    # Return the rows to retain
    tmp_df <- data.table::rbindlist(lapply(1:length(dup_inds), function(i){
      
      # For each RFID timestamp that is present more than once, retain the integrated event that represents the closest match (e.g. the smallest temporal difference) between the RFID and outer beam breaker timestamps
      tmp_dup <- integr8d_df %>% 
        slice(dup_inds[i]) %>% 
        pull(!!sym(rfid_col))
      
      return(
        integr8d_df %>% 
          dplyr::filter(
            !!sym(rfid_col) == tmp_dup
          ) %>% 
          dplyr::arrange(-desc(abs(outer_rfid_diffs))) %>% 
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
    
    # Checking, looks good
    # any(duplicated(integr8d_df_noDups$RFID))
    # (nrow(integr8d_df) - nrow(integr8d_df_noDups)) == length(dup_inds)
    
  } else {
    integr8d_df_noDups <- integr8d_df
  }
  
  write.csv(integr8d_df_noDups, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
