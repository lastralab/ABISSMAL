#' @title integrate_rfid_beamBreakers
#' @description Use lags between the pre-processed radio frequency identification (RFID) data and the pre-processed, labeled beam breaker data to integrate these two datasets. Each RFID detection that remains must be accompanied by a labeled event from the beam breaker dataset (e.g. an entrance or exit event)
#' 
#' @param rfid_file_nm A character string. This should be the name of the file that contains all of the pre-processed RFID detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the RFID data in the subsequent arguments
#' @param irbb_file_nm A character string. This should be the name of the file that contains all of the pre-processed infrared beam breaker (IRBB) detections. Each row is a unique detection event. This data frame must contain all the columns specified for the IRBB data in the subsequent arguments
#' @param l_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify RFID and beam breaker events that are close enough together for integration
#' @param u_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify RFID and beam breaker events that are close enough together for integration
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param PIT_tag_col A character value. This is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID")
#' @param outer_irbb_col A character value. The column name that contains timestamps for the outer pair of beam breakers (e.g. the first pair of beam breakers that an animal encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param inner_irbb_col A character value. The column name that contains timestamps for the inner pair of beam breakers (e.g. the second pair of beam breakers that an individual encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param irbb_event_col A character value. The name of column that contains the type of beam breaker event (e.g. "entrance" or "exit)
#' @param irbb_unique_col A character value. The name of column that contains the unique numeric identifier for each beam breaker event
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the raw data is saved across sensors inside the path above. For instance, "raw_combined".
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details This RFID and beam breaker integration is a separate function because the way in which the sensors are set up to detect movement determines how the lag calculations and integration should be performed. In other words, it is difficult to make a general function to integrate data collected across any two types of sensors used in the tracking system. This function was written to integrate data across 1 RFID antenna and 2 pairs of beam breakers mounted around the entrance of a nest container that was designed for zebra finches. The RFID antenna sits in the middle of a circular entrance. One beam breaker pair sits in front of the RFID antenna (outside of the container, the "outer" pair), and the other sits behind the RFID antenna (mounted for detections inside of the container, the "inner" pair). This function integrates detections across these 2 sensor types regardless of whether or not these detections occurred during perching events captured by the RFID antenna (see `find_rfid_perching_events`). The reason for this is that some perching events may have started or ended as entrance or exit events, and it's important to retain those events at this stage. If it becomes important later to remove behavioral events that were associated with longer perching events, then this can be done by filtering out detections from the integrated dataset that overlap in time with perching events.
#' 
#' @return A .csv file with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the RFID antenna, the lead and rear beam breaker pairs, a unique label for the given event (e.g. entrance or exit), a unique numeric identifier for the given event, and information about the given data processing stage. Each row in the .csv file is an RFID detection that was integrated with a labeled event across the outer and inner beam breaker pairs. Information about the temporal thresholds used for the integration and the date that the data was integrated is also contained in this spreadsheet.

integrate_rfid_beamBreakers <- function(rfid_file_nm, irbb_file_nm, l_th, u_th, sensor_id_col, timestamps_col, PIT_tag_col, outer_irbb_col, inner_irbb_col, irbb_event_col, irbb_unique_col, path, data_dir, out_dir, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Check that the lower and upper temporal thresholds are each numeric
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
  preproc_rfid <- read.csv(file.path(path, data_dir, rfid_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    # Drop columns that aren't needed here
    dplyr::select(-c("thin_threshold_s", "data_stage", "date_pre_processed"))
  
  # Read in the pre-processed and labeled beam breaker data
  labeled_irbb <- read.csv(file.path(path, data_dir, irbb_file_nm)) %>% 
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
  # This code is set up to match RFID detections to either the outer or inner beam breaker timestamp for a given entrance or exit event. Consider an additional option that is more strict, in which the RFID timestamp must be matched to both the outer and inner beam breaker timestamps for any given entrance or exit event
  irbb_df_tmp <- labeled_irbb %>% 
    pivot_longer(
      cols = c(outer_irbb_col, inner_irbb_col),
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
        .y = irbb_df_tmp,
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
          # Calculate the differences between the relevant pairs of timestamps: RFID compared to each beam breaker to find entrances and exits
          # The lags are calculated per group in the grouped data frame
          dplyr::mutate(
            # Negative differences mean one of the beam breaker pairs triggered first
            outer_rfid_diffs_ent = round(!!sym(outer_irbb_col) - leading_RFID, 2),
            inner_rfid_diffs_ent = round(!!sym(inner_irbb_col) - leading_RFID, 2),
            # Negative differences mean the RFID antenna triggered first
            outer_rfid_diffs_exi = round(lagging_RFID - !!sym(outer_irbb_col), 2),
            inner_rfid_diffs_exi = round(lagging_RFID - !!sym(inner_irbb_col), 2)
          ) %>%
          # Convert these differences to boolean values based on a threshold (in seconds)
          dplyr::mutate(
            binary_outer_ent = (
              outer_rfid_diffs_ent >= l_th & 
                outer_rfid_diffs_ent <= u_th
            ),
            binary_inner_ent = (
              inner_rfid_diffs_ent >= l_th & 
                inner_rfid_diffs_ent <= u_th
            ),
            binary_outer_exi = (
              outer_rfid_diffs_exi >= l_th & 
                outer_rfid_diffs_exi <= u_th
            ),
            binary_inner_exi = (
              inner_rfid_diffs_exi >= l_th & 
                inner_rfid_diffs_exi <= u_th
            )
          )
      )
    )
    
    # Do more mapping to perform the integration depending on the given lower and upper temporal thresholds
    # This is done per PIT tag, and the integration is done separately for entrances and exits
  integr8d_df <- lags_grpd %>% 
    dplyr::mutate(
      # Entrances
      matched_irbb_rfid = map(
        .x = lags, 
        .f = ~ dplyr::mutate(
          .x,
          !!rfid_col := leading_RFID,
          outer_rfid_diffs = outer_rfid_diffs_ent,
          inner_rfid_diffs = inner_rfid_diffs_ent
        ) %>% 
          dplyr::filter(
            binary_outer_ent | binary_inner_ent
          ) %>% 
          dplyr::select(outer_irbb_col, inner_irbb_col, rfid_col, outer_rfid_diffs, inner_rfid_diffs) 
        %>% 
          # Exits
          bind_rows(
            .x %>% 
              dplyr::mutate(
                !!rfid_col := lagging_RFID,
                outer_rfid_diffs = outer_rfid_diffs_exi,
                inner_rfid_diffs = inner_rfid_diffs_exi
              ) %>% 
              dplyr::filter(
                binary_outer_exi | binary_inner_exi
              ) %>% 
              dplyr::select(outer_irbb_col, inner_irbb_col, rfid_col, outer_rfid_diffs, inner_rfid_diffs)
          )
      )
    ) %>% 
    dplyr::select(-c(data, lags)) %>% 
    unnest(`cols` = c(matched_irbb_rfid)) %>%
    ungroup() %>% 
    # Make sure to add metadata columns for this integration step
    dplyr::mutate(
      data_stage = "integration",
      lower_threshold_s = l_th,
      upper_threshold_s = u_th,
      date_integrated = paste(Sys.Date(), Sys.time(), sep = " ")
    ) %>% 
    dplyr::rename(
      !!PIT_tag_col := `group_col`
    ) %>% 
    # Add back metadata about the beam breaker events and general metadata
    dplyr::inner_join(
      labeled_irbb %>% 
        dplyr::select(-c("data_type")),
      by = c(outer_irbb_col, inner_irbb_col)
    ) %>% 
    dplyr::select("chamber_id", "year", "month", "day", rfid_col, outer_irbb_col, inner_irbb_col, PIT_tag_col, irbb_event_col, irbb_unique_col, "outer_rfid_diffs", "inner_rfid_diffs", "data_stage", "lower_threshold_s", "upper_threshold_s", "date_integrated") %>% 
    dplyr::arrange(!!sym(rfid_col), desc = FALSE)
  
  # Save the pre-processed data for each sensor in the given setup
  write.csv(integr8d_df, file.path(path, out_dir, "integrated_rfid_beamBreaker_data.csv"), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
