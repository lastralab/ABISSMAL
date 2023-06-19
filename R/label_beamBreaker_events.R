#' @title label_beamBreaker_events
#' @description Label the direction of pre-processed beam breaker events
#' 
#' @param irbb_file_nm A character string. This argument should be the name and extension of the .csv file that contains all of the pre-processed beam breaker detections (from both pairs of beam breakers). Each row is a unique detection event. This spreadsheet must contain all the columns specified for the beam breaker data in the subsequent arguments.
#' @param l_th A numeric value. This argument is the lower or minimum temporal threshold in seconds to identify beam breaker events between the outer and inner pair that are close enough together for integration into entrance or exit events. This must be a numeric value.
#' @param u_th A numeric value. This argument represents an upper or maximum temporal threshold in seconds to identify beam breaker events between the outer and inner pair that are close enough together for integration into entrance or exit events. This must be a numeric value.
#' @param sensor_id_col A character string. This argument is the name of the metadata column that contains information about the data type (e.g. "sensor_id").
#' @param timestamps_col A character string. This argument is the name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms").
#' @param outer_irbb_nm A character string. This argument is the label used for the outer pair of beam breakers (e.g. the first pair of beam breakers that an animal encounters when moving into a nest container or area). This should be a value inside the column named `sensor_id_col`.
#' @param inner_irbb_nm A character string. This argument is the label used for the inner pair of beam breakers (e.g. the second pair of beam breakers that an individual encounters when moving into a nest container or area). This should be a value inside the column named `sensor_id_col`.
#' @param path A character string. This argument is the path on the local computer or external hard drive specifying where the data is saved across sensors for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This argument should be the name of directory where the pre-processed data is saved across sensors inside the path above. For instance, "pre_processed" for this function in order to label the pre-processed beam breaker detections.
#' @param out_dir A character string. This argument should be the name of a directory inside the path above specifying where the .csv file of labeled data should be saved. For instance, "pre-processed". This folder will be created as a new directory if it doesn't already exist.
#' @param out_file_nm A character string. This argument is the name (plus extension) of the resulting file that will be written to out_dir. The default is "labeled_beamBreaker_data.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS6" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details Use pre-processed data from two pairs of beam breakers to label entrance and exit movements (in or out of a nest container or nesting area). This function uses temporal lags between the two beam breaker pairs and the given temporal thresholds to infer directionality of unique movement events by an animal entering or leaving a nest container or area. The outer pair of beam breakers is the first pair that should trigger when an animal enters a nest container with the 2 beam breakers mounted around the entrance. The inner pair of beam breakers should be the second pair that triggers when an animal enters, but the first pair to trigger when an animal leaves.
#'
#' @return This function returns a spreadsheet in .csv format with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the lead and rear beam breaker pairs. The spreadsheet will also contain a unique label for the given event (e.g. entrance or exit), a unique numeric identifier for the given event, and information about the given data processing stage. Each row in the .csv file is a labeled event across the outer and inner beam breaker pairs that was inferred as a unique movement with directionality using the specified temporal thresholds.
#' 

# Make a function to integrate between beam breaker pairs to infer entrance and exit movements
label_beamBreaker_events <- function(irbb_file_nm, l_th, u_th, sensor_id_col, timestamps_col, outer_irbb_nm, inner_irbb_nm, path, data_dir, out_dir, out_file_nm = "labeled_beamBreaker_data.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Get the formal arguments from the current function
  # TKTK try substituting the function name with: match.call()[[1]]
  f_args <- methods::formalArgs(label_beamBreaker_events)
  
  # Check that the formal arguments were all specified
  invisible(sapply(1:length(f_args), function(i){
    check_defined(f_args[i])
  }))
  
  # Check that the formal arguments that should not be NULL
  invisible(sapply(1:length(f_args), function(i){
    check_null(f_args[i])
  }))
  
  # Check that the formal arguments that should be strings are strings
  expect_numeric <- c("l_th", "u_th")

  expect_strings <- f_args[-grep(paste(paste("^", expect_numeric, "$", sep = ""), collapse = "|"), f_args)]
  
  invisible(sapply(1:length(expect_strings), function(i){
    check_string(expect_strings[i])
  }))
  
  # Check that the formal arguments that should be numeric are numeric
  invisible(sapply(1:length(expect_numeric), function(i){
    check_numeric(expect_numeric[i])
  }))
  
  # Check that the input directory exists
  check_dirs(path, data_dir)
  
  # Check that the input file exists in the input directory
  check_file(file.path(path, data_dir), irbb_file_nm)
  
  # Create the directory for saving the labeled data files (if it doesn't exist)
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the pre-processed beam breaker data
  preproc_data <- read.csv(file.path(path, data_dir, irbb_file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    # Drop columns that aren't needed here
    dplyr::select(-c("thin_threshold_s", "data_stage", "date_pre_processed"))
  
  # Check that this object is a data frame
  check_df_class(preproc_data)
  
  # Check that the expected columns from formal arguments are found in the data
  expected_cols <- f_args[grep("col", f_args)]
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_fArgs_data_cols(expected_cols[i], preproc_data)
  }))
  
  # Check that the expected columns from formal arguments do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_fArgs_cols_nas(expected_cols[i], preproc_data)
  }))
  
  # Check that date-related columns are found in the data
  expected_cols <- c("year", "month", "day")
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_data_cols(expected_cols[i], preproc_data)
  }))
  
  # Check that the date-related columns do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_cols_nas(expected_cols[i], preproc_data)
  }))
  
  # Check that columns with timestamps are in the right format
  tstmps_cols <- f_args[grep("time", f_args)]
  
  invisible(sapply(1:length(tstmps_cols), function(i){
    check_tstmps_cols(tstmps_cols[i], preproc_data, "%Y-%m-%d %H:%M:%OS6")
  }))
  
  # Check that the sensor ID column has the expected values
  check_col_values(sensor_id_col, preproc_data, c(outer_irbb_nm, inner_irbb_nm))

  # Ensure the timestamps are ordered, then calculate the time lags between the two beam breaker pairs. Here this is done using the leading differences between the beam breaker pairs to identify possible entrances and exits, respectively
  preproc_data2 <- preproc_data %>% 
    dplyr::arrange(timestamp_ms, desc = FALSE) %>%
    # Add unique row IDs to facilitate widening the data frame
    rowid_to_column() %>% 
    pivot_wider(
      names_from = !!sym(sensor_id_col),
      values_from = !!sym(timestamps_col)
    ) 
  
  diffs_df <- preproc_data2 %>%
    dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm)) %>% 
    # The lead() call moves the timestamps for the given column one row index up, so that the difference calculation can be performed with the timestamps for the other column
    dplyr::mutate(
      leading_outer = lead(!!sym(outer_irbb_nm), default = first(!!sym(outer_irbb_nm))),
      lagging_outer = lag(!!sym(outer_irbb_nm), default = first(!!sym(outer_irbb_nm)))
    ) %>% 
    dplyr::mutate(
      # For both the lead and lag calculations, positive differences mean that the inner beam breakers triggered first, and negative differences mean that the outer pair triggered first
      outer_inner_lead_diffs = round(as.numeric(leading_outer - !!sym(inner_irbb_nm)), 2),
      outer_inner_lag_diffs = round(as.numeric(lagging_outer - !!sym(inner_irbb_nm)), 2)
    ) %>% 
    # Convert these differences to boolean based on the given threshold (in seconds)
    dplyr::mutate(
      # To search for entrances, look for outer beam breakers detections that came within the given threshold BEFORE an inner beam breaker timestamp. Set up these conditionals for both the lead and lag calculations
      binary_lead_ent = (
        # Outer beam breaker before, so negative lead differences
        outer_inner_lead_diffs <= -l_th & outer_inner_lead_diffs >= -u_th
        ),
      binary_lag_ent = (
        # Outer beam breaker before, so negative lag differences
        outer_inner_lag_diffs <= -l_th & outer_inner_lag_diffs >= -u_th
      ),
      # To search for exits, look for outer beam breakers detections that came within the given threshold AFTER an inner beam breaker timestamp. Set up these conditionals for both the lead and lag calculations
      binary_lead_exi = (
        # Outer beam breaker after, so positive lead differences
        outer_inner_lead_diffs >= l_th & outer_inner_lead_diffs <= u_th
      ),
      binary_lag_exi = (
        # Outer beam breaker after, so positive lag differences
        outer_inner_lag_diffs >= l_th & outer_inner_lag_diffs <= u_th
      )
    )
  
  conditnal_lead_ent <- "binary_lead_ent & !is.na(binary_lead_ent)"
  conditnal_lead_exi <- "binary_lead_exi & !is.na(binary_lead_exi)"
  
  conditnal_lag_ent <- "binary_lag_ent & !is.na(binary_lag_ent)"
  conditnal_lag_exi <- "binary_lag_exi & !is.na(binary_lag_exi)"
  
  # Get the entrance and exit events identified using the given temporal thresholds
  integr8d_df <- diffs_df %>%
    # Entrances, lead differences
    dplyr::mutate(
      !!outer_irbb_nm := leading_outer,
      diffs = outer_inner_lead_diffs
    ) %>% 
    # Then filter for beam breaker events that match the logic for entrances
    dplyr::filter(
      !!rlang::parse_expr(conditnal_lead_ent)
    ) %>%
    dplyr::mutate(
      irbb_direction_inferred = "entrance",
      irbb_assignmnt_type = "lead"
    ) %>% 
    dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm), diffs, irbb_direction_inferred, irbb_assignmnt_type) %>% 
    # Entrances, lag differences
    bind_rows(
      diffs_df %>%
        dplyr::mutate(
          !!outer_irbb_nm := lagging_outer,
          diffs = outer_inner_lag_diffs
        ) %>% 
        # Then filter for beam breaker events that match the logic for entrances
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lag_ent)
        ) %>% 
        dplyr::mutate(
          irbb_direction_inferred = "entrance",
          irbb_assignmnt_type = "lag"
        ) %>% 
        dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm), diffs, irbb_direction_inferred, irbb_assignmnt_type) 
    ) %>% 
    # Exits, lead differences
    bind_rows(
      diffs_df %>%
        dplyr::mutate(
          !!outer_irbb_nm := leading_outer,
          diffs = outer_inner_lead_diffs
        ) %>% 
        # Then filter for beam breaker events that match the logic for exits
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lead_exi)
        ) %>% 
        dplyr::mutate(
          irbb_direction_inferred = "exit",
          irbb_assignmnt_type = "lead"
        ) %>% 
        dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm), diffs, irbb_direction_inferred, irbb_assignmnt_type) 
    ) %>% 
    # Exits, lag differences
    bind_rows(
      diffs_df %>%
        dplyr::mutate(
          !!outer_irbb_nm := lagging_outer,
          diffs = outer_inner_lag_diffs
        ) %>% 
        # Then filter for beam breaker events that match the logic for exits
        dplyr::filter(
          !!rlang::parse_expr(conditnal_lag_exi)
        ) %>% 
        dplyr::mutate(
          irbb_direction_inferred = "exit",
          irbb_assignmnt_type = "lag"
        ) %>% 
        dplyr::select(all_of(outer_irbb_nm), all_of(inner_irbb_nm), diffs, irbb_direction_inferred, irbb_assignmnt_type) 
    ) %>% 
    dplyr::mutate(
      data_stage = "pre-processed",
      lower_threshold_s = l_th,
      upper_threshold_s = u_th,
      date_labeled = paste(Sys.Date(), Sys.time(), sep = " ")
    ) %>% 
    # Rename the outer beam breaker column for the metadata join below
    dplyr::rename(
      `timestamp_ms` = !!sym(outer_irbb_nm)
    ) %>% 
    # Add back the original metadata (shared between the beam breaker pairs)
    dplyr::inner_join(
      preproc_data %>%
        dplyr::select(-c("sensor_id")),
      by = "timestamp_ms"
    ) %>%
    # Redo the renaming the outer beam breaker column
    dplyr::rename(
      !!outer_irbb_nm := "timestamp_ms"
    ) %>% 
    # Rename the beam breaker columns once more to remove spaces
    dplyr::rename(
      `Outer_beam_breaker` = !!sym(outer_irbb_nm),
      `Inner_beam_breaker` = !!sym(inner_irbb_nm)
    ) %>% 
    dplyr::select(data_type, chamber_id, year, month, day, Outer_beam_breaker, Inner_beam_breaker, diffs, irbb_direction_inferred, irbb_assignmnt_type, data_stage, lower_threshold_s, upper_threshold_s, date_labeled) %>% 
    dplyr::arrange(all_of(outer_irbb_nm), desc = FALSE)
  
  #### Handle duplicates
  
  # The same inner or outer beam breaker timestamp should not be assigned to multiple outer or inner beam breaker timestamps
  # To remove duplicate assignments, I need to use a temporal rule to retain the outer and inner beam breaker timestamps closest together in time. And drop the other matches as duplicates
  
  bb_ids <- c("Outer_beam_breaker", "Inner_beam_breaker")
  
  # Return the rows to retain
  tmp_df2 <- data.table::rbindlist(pblapply(1:length(bb_ids), function(i){
    
    dup_inds <- which(duplicated(integr8d_df[[bb_ids[i]]]))
    
    if(length(dup_inds) > 0){
      
      tmp_df <- data.table::rbindlist(lapply(1:length(dup_inds), function(j){
        
        # For each given beam breaker timestamp that is present more than once, retain the integrated event that represents the closest match (e.g. the smallest temporal difference) between timestamps for the given pair of beam breakers and the other beam breaker pair
        tmp_dup <- integr8d_df %>% 
          slice(dup_inds[j]) %>% 
          pull(!!sym(bb_ids[i]))
        
        return(
          integr8d_df %>% 
            dplyr::filter(
              !!sym(bb_ids[i]) == tmp_dup
            ) %>% 
            dplyr::arrange(-desc(abs(diffs))) %>% 
            slice(1)
        )
        
      }))
      
    }
    
    return(tmp_df)
    
  }))
  
  if(nrow(tmp_df2) > 0){
    
    # Among the rows to retain, drop rows that have duplicated timestamps for both beam breaker pairs as well as timestamp differences (true duplicates)
    tmp_df3 <- tmp_df2 %>% 
      dplyr::filter(
        !duplicated(!!sym(bb_ids[1])) & 
          !duplicated(!!sym(bb_ids[2])) &
          !duplicated(diffs)
      )
    
    # Get the indices of all of the duplicated rows
    all_dup_inds2 <- unlist(pblapply(1:length(bb_ids), function(i){
      
      dup_inds <- which(duplicated(integr8d_df[[bb_ids[i]]]))
      
      if(length(dup_inds) > 0){
        
        all_dup_inds <- unlist(lapply(1:length(dup_inds), function(j){
          
          tmp_dup <- integr8d_df %>% 
            slice(dup_inds[j]) %>% 
            pull(!!sym(bb_ids[i]))
          
          return(
            integr8d_df %>% 
              rowid_to_column() %>% 
              dplyr::filter(
                !!sym(bb_ids[i]) == tmp_dup
              ) %>% 
              pull(rowid)
          )
          
        }))
        
      }
      
    }))
    
    # Remove all of the duplicated rows, then add back the rows to retain
    integr8d_df_noDups <- integr8d_df %>% 
      slice(-c(unique(all_dup_inds2))) %>% 
      bind_rows(
        tmp_df3
      ) %>% 
      dplyr::arrange(!!sym(bb_ids[1]), desc = FALSE) %>% 
      rowid_to_column() %>% 
      dplyr::rename(
        `unique_entranceExit` = rowid
      ) %>%
      dplyr::select(data_type, chamber_id, year, month, day, Outer_beam_breaker, Inner_beam_breaker, diffs, irbb_direction_inferred, unique_entranceExit, irbb_assignmnt_type, data_stage, lower_threshold_s, upper_threshold_s, date_labeled)
    
  } else {
    
    integr8d_df_noDups <- integr8d_df %>% 
      dplyr::arrange(!!sym(bb_ids[1]), desc = FALSE) %>% 
      rowid_to_column() %>% 
      dplyr::rename(
        `unique_entranceExit` = rowid
      ) %>%
      dplyr::select(data_type, chamber_id, year, month, day, Outer_beam_breaker, Inner_beam_breaker, diffs, irbb_direction_inferred, unique_entranceExit, irbb_assignmnt_type, data_stage, lower_threshold_s, upper_threshold_s, date_labeled)
    
  }
  
  write.csv(integr8d_df_noDups, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
