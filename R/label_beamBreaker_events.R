#' @title label_beamBreaker_events
#' @description Use data from two pairs of beam breakers to label entrance and exit movements (in or out of a nest container or nesting area).
#' 
#' @param irbb_file_nm A character string. This should be the name of the file that contains all of the pre-processed and labeled beam breaker detections. Each row is a unique detection event. This spreadsheet must contain all the columns specified for the beam breaker data in the subsequent arguments
#' @param l_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify beam breaker events between the outer and inner pair that are close enough together for integration into entrance or exit events. This must be a numeric value
#' @param u_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify beam breaker events between the outer and inner pair that are close enough together for integration into entrance or exit events. This must be a numeric value
#' @param sensor_id_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "sensor_id")
#' @param timestamps_col A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param outer_irbb_nm A character value. The label used for the outer pair of beam breakers (e.g. the first pair of beam breakers that an animal encounters when moving into a nest container or area). This should be a value in the column named `sensor_id_col`
#' @param inner_irbb_nm A character value. The label used for the inner pair of beam breakers (e.g. the second pair of beam breakers that an individual encounters when moving into a nest container or area). This should be a value in the column named `sensor_id_col`
#' @param path A character string. This should be the path specifying the overall directory where data is saved for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data".
#' @param data_dir A character string. This should be the name of directory where the raw data is saved across sensors inside the path above. For instance, "raw_combined".
#' @param out_dir A character string. This should be the name of a directory specifying where the .csv file of pre-processed data should be saved for each sensor. For instance, "pre-processed". This folder will be appended to the data_path and created as a new directory if it doesn't already exist.
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @return A .csv file with the metadata columns from the original pre-processed data used as input, as well as columns indicating each of the timestamps of the lead and rear beam breaker pairs, a unique label for the given event (e.g. entrance or exit), a unique numeric identifier for the given event, and information about the given data processing stage. Each row in the .csv file is a labeled event across the outer and inner beam breaker pairs that was identified using a given temporal threshold
#' 

library(tidyverse)
library(pbapply)

irbb_file_nm <- "pre_processed_data_IRBB.csv"
l_th <- 0
u_th <- 2
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
outer_irbb_nm <- "Outer Beam Breaker"
inner_irbb_nm <- "Inner Beam Breaker"
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
data_dir <- "pre_processed"
out_dir <- "pre_processed"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"

# TKTK update this function to write to the preprocessed folder, and probably update he name of the output and the function itself, also the value in the "data_stage" column

# Make a function to integrate between beam breaker pairs to infer entrance and exit movements
label_beamBreaker_events <- function(irbb_file_nm, l_th, u_th, sensor_id_col, timestamps_col, outer_irbb_nm, inner_irbb_nm, path, data_dir, out_dir, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
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
  
  # Check that the raw data is a data frame
  if(!is.data.frame(preproc_data)){
    stop('The pre-processed data needs to be a data frame')
  }
  
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
      data_stage = "integration",
      temporal_threshold_s = threshold,
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
    dplyr::select(data_type, chamber_id, year, month, day, Outer_beam_breaker, Inner_beam_breaker, diffs, irbb_direction_inferred, irbb_assignmnt_type, data_stage, temporal_threshold_s, date_labeled) %>% 
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
      dplyr::select(data_type, chamber_id, year, month, day, Outer_beam_breaker, Inner_beam_breaker, diffs, irbb_direction_inferred, unique_entranceExit, irbb_assignmnt_type, data_stage, temporal_threshold_s, date_labeled)
    
  } else {
    
    integr8d_df_noDups <- integr8d_df %>% 
      dplyr::arrange(!!sym(bb_ids[1]), desc = FALSE) %>% 
      rowid_to_column() %>% 
      dplyr::rename(
        `unique_entranceExit` = rowid
      ) %>%
      dplyr::select(data_type, chamber_id, year, month, day, Outer_beam_breaker, Inner_beam_breaker, diffs, irbb_direction_inferred, unique_entranceExit, irbb_assignmnt_type, data_stage, temporal_threshold_s, date_labeled)
    
  }
  
  write.csv(integr8d_df_noDups, file.path(path, out_dir, "labeled_beamBreaker_data.csv"), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}
