#' @title detect_clusters
#' @description Find clusters of detections collected across different sensors
#' 
#' @param file_nms A character vector of length 1 or more. This argument should be the name and extension of each .csv file that contains all of the pre-processed data for a given sensor type, respectively. For instance, c("pre_processed_data_RFID.csv", "pre_processed_data_IRBB.csv", "pre_processed_data_Video.csv"). In each spreadsheet, each row should be a unique detection event, and the spreadsheet must contain all the columns specified in the subsequent arguments (with the exception of the PIT_tag_col_nm for non-RFID data).
#' @param threshold A single numeric value. This argument represents a temporal threshold in seconds that will be used to identify clusters of detections across all sensors (e.g. detections that occurred in close succession).
#' @param run_length A single numeric value. This argument indicates the minimum number of consecutive detections used to label a cluster of detections across sensors. The default setting is 2. 
#' @param sensor_id_col_nm A character string. This argument is the name of the metadata column that contains information about the data type (e.g. "sensor_id").
#' @param timestamps_col_nm A character string. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "timestamp_ms").
#' @param PIT_tag_col_nm A character string. This argument is the name of the metadata column that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID").
#' @param rfid_label A character string. This argument is the label for the RFID sensor in the sensor_id_col_nm column in pre-processed data (e.g. "RFID"). The default is NULL, since the input data does not need to contain RFID detections.
#' @param camera_label A character string. This argument is the label for the camera sensor that records video in the sensor_id_col_nm column in pre-processed data (e.g. "Camera"). The default is NULL, since the input data does not need to contain video recording events.
#' @param preproc_metadata_col_nms A character vector. This argument should be a string of the metadata column names from pre-processing that should be dropped from either or both data frames. For instance, c("thin_threshold_s", "data_stage", "date_pre_processed", "date_labeled").
#' @param general_metadata_col_nms A character vector. This argument should be a string of the general metadata column names that will be carried through into the spreadsheet generated by this function. For instance: c("chamber_id", "year", "month", "day"). These columns will be added as the first columns in the integrated data frame, in the same order in which they are provided.
#' @param video_metadata_col_nms A character vector. This argument should be a string of the video metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"). These columns will be added as later columns in resulting data frame, in the same order in which they were specified. The default is NULL, since video recording events are not required to be used as input.
#' @param path A character string. This argument should be the path on the local computer or external hard drive specifying where the data is saved across sensors for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022".
#' @param data_dir A character string. This argument should be the name of the directory where the pre-processed data that is used as input is saved inside the path above. For instance, "processed".
#' @param out_dir A character string. This argument should be the name of a directory specifying where the .csv file of integrated data should be saved. For instance, "processed". This folder will be appended to the path and created as a new directory if it doesn't already exist.
#' @param out_file_nm A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default is "detection_clusters.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS6" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details `detect_clusters` uses lags between the timestamps of all pre-processed detections (e.g. all detections compared to themselves) in order to find clusters of detections that occurred in close succession (controlled by the `threshold` and `run_length` arguments). This temporal calculation is performed in order to identify clusters of detections across one or more sensors that likely represent discrete movement events. `detect_clusters` uses the spreadsheets generated by `preprocess_detections` as input. This function does not take perching events into account, as perching events can be integrated in the function `score_clusters`.
#' 
#' @return A spreadsheet in .csv format with the metadata columns from the original pre-processed data used as input, as well as columns indicating the start and end time of each detection cluster, and the sequence of sensor events (e.g. the labels of each sensor that triggered in each clustered detected, ordered from left to right by the timing in which each sensor triggered). Each row in the .csv file is detection cluster identified by the function. Information about the temporal threshold and run length used for the integration and the date of integration is also contained in the resulting spreadsheet.

detect_clusters <- function(file_nms, threshold, run_length = 2, sensor_id_col_nm, timestamps_col_nm, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, preproc_metadata_col_nms, general_metadata_col_nms, video_metadata_col_nms = NULL, path, data_dir, out_dir, out_file_nm = "detection_clusters.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Get the user-specified values for each formal argument of the current function
  f_args <- getFunctionParameters()
  
  # Check that arguments that cannot ever be non-NULL are not NULL
  expect_nonNulls <- f_args[grep(paste(paste("^", c("file_nms", "threshold", "run_length", "sensor_id_col_nm", "timestamps_col_nm", "preproc_metadata_col_nms", "general_metadata_col_nms", "path", "data_dir", "out_dir", "out_file_nm", "tz", "POSIXct_format"), "$", sep = ""), collapse = "|"), names(f_args))]
  
  invisible(sapply(1:length(expect_nonNulls), function(i){
    check_not_null(names(expect_nonNulls[i]), expect_nonNulls[[i]])
  }))
  
  # Check that the sensor suffix in the input file names are correct
  invisible(lapply(1:length(file_nms), function(z){
    check_file_nm2(file_nms[z])
  }))
  
  # Check that the formal arguments that should be strings are strings
  expect_numeric <- c("threshold", "run_length")
  
  # Check that the formal arguments that should be NULL under certain conditions are NULL given the current user-specified arguments
  if(any(grepl("RFID", file_nms) | grepl("IRBB", file_nms)) & all(!grepl("Video", file_nms))){
    
    expect_nulls <- c("camera_label", "video_metadata_col_nms")
    
  } else if(all(grepl("Video", file_nms))){
    
    expect_nulls <- c("PIT_tag_col_nm", "rfid_label")
    
  }

  if(exists("expect_nulls")){
    
    expect_nulls <- f_args[grep(paste(paste("^", expect_nulls, "$", sep = ""), collapse = "|"), names(f_args))]
    
    invisible(sapply(1:length(expect_nulls), function(i){
      check_null(names(expect_nulls[i]), expect_nulls[[i]])
    }))
    
    expect_strings <- f_args[-grep(paste(paste("^", c(expect_numeric, names(expect_nulls)), "$", sep = ""), collapse = "|"), names(f_args))]
    
    invisible(sapply(1:length(expect_strings), function(i){
      check_string(names(expect_strings[i]), expect_strings[[i]])
    }))
    
  }

  # Check that all formal arguments that cannot be NULL are not NULL:
  if(all(grepl("RFID", file_nms))){
    
    expect_nonNull <- c("PIT_tag_col_nm", "rfid_label")
    
  } else if(all(grepl("Video", file_nms))){
    
    expect_nonNull <- c("camera_label", "video_metadata_col_nms")
    
  } else if(any(grepl("RFID", file_nms)) & any(grepl("Video", file_nms))){
    
    expect_nonNull <- c("PIT_tag_col_nm", "rfid_label", "camera_label", "video_metadata_col_nms")
    
  }
  
  if(exists("expect_nonNull")){
    
    expect_nonNulls <- f_args[grep(paste(paste("^", expect_nonNull, "$", sep = ""), collapse = "|"), names(f_args))]
    
    invisible(sapply(1:length(expect_nonNulls), function(i){
      check_not_null(names(expect_nonNulls[i]), expect_nonNulls[[i]])
    }))
    
  }
  
  # Check that the formal arguments that should be numeric are numeric
  invisible(sapply(1:length(expect_numeric), function(i){
    check_numeric(expect_numeric[i], f_args[[grep(paste(paste("^", expect_numeric[i], "$", sep = ""), collapse = "|"), names(f_args))]])
  }))
  
  # Check that the input directory exists
  check_dirs(path, data_dir)
  
  # Check that the input files exist in the input directory
  invisible(sapply(1:length(file_nms), function(i){
    check_file(file.path(path, data_dir), file_nms[i])
  }))
  
  # Create the directory for saving the output file if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in files and remove columns in a loop before binding together in a single data frame
  all_sensors <- data.table::rbindlist(lapply(1:length(file_nms), function(x){
    
    tmp <- read.csv(file.path(path, data_dir, file_nms[x])) 
    
    # Check that the data contains the timestamps column
    expected_cols <- f_args[grep("timestamp", names(f_args))]
    
    check_fArgs_data_cols(expected_cols, tmp)
    
    tmp <- tmp %>% 
      # Make sure that the timestamps are in the right format
      dplyr::mutate(
        !!timestamps_col_nm := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col_nm), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Drop extra columns in order to bind together data frames in the list
    cols2drop <- names(tmp)[grep(paste(paste("^", c(preproc_metadata_col_nms), "$", sep = ""), collapse = "|"), names(tmp))]
    
    tmp <- tmp %>% 
      dplyr::select(-c(all_of(cols2drop)))
    
    # If the data frame has video recordings, then drop all post-trigger video recordings (these have the same timestamp as the respective pre-trigger recordings)
    if(!is.null(camera_label) & !is.null(video_metadata_col_nms)){
      
      if(any(grepl(camera_label, tmp[[sensor_id_col_nm]]))){
        
        tmp_col_nm <- video_metadata_col_nms[grep("file_name", video_metadata_col_nms)]
        
        tmp <- tmp[-which(grepl("post_trigger", tmp[[tmp_col_nm]])), ]
        
      }
      
    }
    
    # If a given data frame does not have the column PIT_tag_col_nm, then create a column named like this but with NAs
    if(!is.null(PIT_tag_col_nm)){
      
      if(!any(grepl(PIT_tag_col_nm, names(tmp)))){
        
        tmp <- tmp %>% 
          dplyr::mutate(
            !!PIT_tag_col_nm := NA
          )
        
      }
      
    }
    
    # If a given data frame does not have the columns in video_metadata_col_nms, then create these columns and fill them with NAs
    if(!is.null(video_metadata_col_nms) & !any(grepl(paste(video_metadata_col_nms, collapse = "|"), names(tmp)))){
      
      vid_cols <- as.data.frame(matrix(data = NA, nrow = nrow(tmp), ncol = length(video_metadata_col_nms)))
      
      names(vid_cols) <- video_metadata_col_nms
      
      tmp <- tmp %>% 
        dplyr::bind_cols(
          vid_cols
        )
      
    }
    
    tmp <- tmp %>% 
      dplyr::select(names(.)[-grep(paste(c(PIT_tag_col_nm, video_metadata_col_nms), collapse = "|"), names(.))], all_of(PIT_tag_col_nm), all_of(video_metadata_col_nms))
    
  }))
  
  # Check that this object is a data frame
  check_df_class(all_sensors)
  
  # Check that the expected columns from formal arguments are found in the combined data
  colnames_fArgs <- f_args[grep("col", names(f_args))][-grep("preproc_metadata_col_nms|general_metadata_col_nms|video_metadata_col_nms", names(f_args[grep("col", names(f_args))]))]
  
  invisible(sapply(1:length(colnames_fArgs), function(i){
    check_fArgs_data_cols(colnames_fArgs[[i]], all_sensors)
  }))
  
  # Check that the expected columns from formal arguments do not have NAs
  colnames_fArgs2 <- colnames_fArgs[-grep("PIT_tag", names(colnames_fArgs))]
  
  invisible(sapply(1:length(colnames_fArgs2), function(i){
    check_fArgs_cols_nas(colnames_fArgs2[[i]], all_sensors)
  }))
  
  # Check that date-related columns are found in the data
  expected_cols <- c("year", "month", "day")
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_data_cols(expected_cols[i], all_sensors)
  }))
  
  # Check that the date-related columns do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_cols_nas(expected_cols[i], all_sensors)
  }))
  
  # Check that columns with timestamps are in the right format
  check_tstmps_cols("timestamps_col_nm", all_sensors, "%Y-%m-%d %H:%M:%OS6")
  
  # If RFID data is present, then get the unique PIT tag IDs for operations below
  if(!is.null(rfid_label)){
    
    if(any(grepl(rfid_label, unique(all_sensors[[sensor_id_col_nm]])))){
      
      tag_ids <- all_sensors %>%
        dplyr::filter(sensor_id == rfid_label) %>%
        dplyr::pull(!!sym(PIT_tag_col_nm)) %>%
        unique()
      
    }
    
  }
  
  # Group the data frame by day, then search for bursts of activity
  detectns <- all_sensors %>%
    dplyr::mutate(
      dates = paste(year, month, day, sep = "-")
    ) %>%
    dplyr::group_by(dates) %>% 
    dplyr::arrange(!!sym(timestamps_col_nm), .by_group = TRUE) %>% 
    # Make unique row indices within groups
    dplyr::mutate(
      group_row_id = dplyr::row_number()
    ) %>% 
    nest() %>% 
    dplyr::mutate(
      # Map over the nested data frames
      lags = map(
        .x = data,
        .f = ~ dplyr::mutate(.x,
                             shift = dplyr::lag(!!sym(timestamps_col_nm), default = first(!!sym(timestamps_col_nm)))
        ) %>% 
          # Convert differences to Boolean based on the thinning threshold to find stretches of detection events very close together
          dplyr::mutate(
            diff = as.numeric(!!sym(timestamps_col_nm) - shift),
            # Taking anything less than or equal to the threshold, see previous RFID pre-processing. The diff > 0 condition removes the first timestamp compared to itself
            binary_diff = (diff <= threshold & diff > 0)
          ) %>% 
          dplyr::select(all_of(timestamps_col_nm), diff, binary_diff) 
      )
    ) %>% 
    
    # Make a data frame of the first and last indices of each run longer than the given run_length that contain temporal difference values below or equal to the given threshold
    dplyr::mutate(
      lags_runs = map(
        .x = lags,
        .f = ~ dplyr::reframe(.x,
                              first_indices = cumsum(rle(binary_diff)[["lengths"]]) - (rle(binary_diff)[["lengths"]]),
                              last_indices = cumsum(rle(binary_diff)[["lengths"]]),
                              run_values = rle(binary_diff)[["values"]],
                              run_lengths = rle(binary_diff)[["lengths"]],
                              .groups = "keep"
        ) %>% 
          dplyr::filter(run_values & run_lengths >= run_length) %>% 
          dplyr::ungroup()
      )
    ) %>% 
    
    # Get the unique clusters of detections
    dplyr::mutate(
      bouts = map(
        .x = lags_runs,
        .y = data,
        # For each unique date, retain the first and last indices of sensor detections flagged as clusters
        # Use pmap_dfr to iterate over rows in each nested data frame, in which each row represents a unique cluster of detections by date
        .f = ~ dplyr::select(.x, first_indices, last_indices) %>%
          pmap_dfr(., function(first_indices, last_indices){
            
            tmp <- data.frame(
              start = .y[[1]] %>%
                dplyr::filter(group_row_id == first_indices) %>%
                dplyr::pull(all_of(timestamps_col_nm)),
              end = .y[[1]] %>%
                dplyr::filter(group_row_id == last_indices) %>%
                dplyr::pull(all_of(timestamps_col_nm))
            ) %>% 
              dplyr::mutate(
                event_seq = .y[[1]] %>%
                  dplyr::filter(group_row_id >= first_indices & group_row_id <= last_indices) %>%
                  dplyr::pull(sensor_id) %>% 
                  paste(., collapse = "; ")
              )
            
            # If RFID data is present, then add back PIT tag information
            if(!is.null(rfid_label)){
              
              if(any(grepl(rfid_label, tmp$event_seq))){
                
                PIT_tag_seq <- .y[[1]] %>%
                  dplyr::filter(group_row_id >= first_indices & group_row_id <= last_indices) %>%
                  dplyr::pull(all_of(PIT_tag_col_nm))
                
                # Drop NA values in the sequence
                PIT_tag_seq <- PIT_tag_seq[!is.na(PIT_tag_seq)]
                
                total_indiv1_detections <- length(which(PIT_tag_seq == tag_ids[1]))
                total_indiv2_detections <- length(which(PIT_tag_seq == tag_ids[2]))
                individual_initiated <- PIT_tag_seq[1]
                individual_ended <- PIT_tag_seq[length(PIT_tag_seq)]
                
              } else {
                
                total_indiv1_detections <- total_indiv2_detections <- individual_initiated <- individual_ended <- NA
                
              }
              
              tmp <- tmp %>%
                dplyr::mutate(
                  indiv1_id = tag_ids[1],
                  indiv2_id = tag_ids[2],
                  total_indiv1_detections = total_indiv1_detections,
                  total_indiv2_detections = total_indiv2_detections,
                  individual_initiated = individual_initiated,
                  individual_ended = individual_ended
                )
              
            }
            
            # If video data is present, then add back video-related information
            if(!is.null(camera_label) & !is.null(video_metadata_col_nms)){

              if(any(grepl(camera_label, tmp$event_seq))){

                vid_cols_tmp <- .y[[1]] %>%
                  dplyr::filter(group_row_id >= first_indices & group_row_id <= last_indices) %>%
                  dplyr::select(all_of(video_metadata_col_nms)) %>%
                  # Drop rows that have NAs in these columns
                  dplyr::filter(complete.cases(.))
                
                # If multiple videos occurred within a detection cluster, then concatenate these file names together
                if(nrow(vid_cols_tmp) > 1){

                  tmp_col_nm <- video_metadata_col_nms[grep("file", video_metadata_col_nms)]
                  
                  vid_file_nms <- vid_cols_tmp %>%
                    dplyr::reframe(
                      tmp_col_nm = str_c(tmp_col_nm, collapse = "; ")
                    ) %>%
                    dplyr::pull(tmp_col_nm)

                  vid_cols_tmp[[tmp_col_nm]] <- vid_file_nms

                  vid_cols_tmp <- vid_cols_tmp %>%
                    distinct()

                }

                tmp <- tmp %>%
                  dplyr::bind_cols(
                    vid_cols_tmp
                  )

              }

            }
            
            return(tmp)
            
          })
      ) 
    )
  
  detectns2 <- detectns %>%
    dplyr::select(-c(data, lags, lags_runs)) %>% 
    unnest(`cols` = c(bouts)) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-c(dates))
  
  if(nrow(detectns) > 0){
    
    detectns3 <- detectns2 %>% 
      dplyr::inner_join(
        all_sensors %>% 
          dplyr::select(all_of(general_metadata_col_nms), all_of(timestamps_col_nm)) %>% 
          distinct(),
        by = c("start" = timestamps_col_nm)
      ) %>% 
      dplyr::mutate(
        threshold_seconds = threshold,
        run_length = run_length,
        data_stage = "integration",
        date_processed = paste(Sys.Date(), Sys.time(), sep = " ")
      )
    
  } else {
    
    detectns3 <- data.frame(
      start = NA,
      end = NA,
      event_seq = NA,
      indiv1_id = tag_ids[1],
      indiv2_id = tag_ids[2],
      total_indiv1_detections = NA,
      total_indiv2_detections = NA,
      individual_initiated = NA,
      individual_ended = NA
    ) %>% 
      dplyr::bind_cols(
        all_sensors %>% 
          dplyr::select(all_of(general_metadata_col_nms), all_of(timestamps_col_nm)) %>% 
          distinct()
      ) %>% 
      dplyr::mutate(
        threshold_seconds = threshold,
        run_length = run_length,
        data_stage = "integration",
        date_processed = paste(Sys.Date(), Sys.time(), sep = " ")
      )
    
  }
  
  # Order the data and add back important metadata before writing it out
  if(!is.null(video_metadata_col_nms) & any(grepl(paste(video_metadata_col_nms, collapse = "|"), names(detectns3)))){
    
    detectns4 <- detectns3 %>% 
      dplyr::select(all_of(general_metadata_col_nms), names(.)[-grep(paste(c(general_metadata_col_nms, video_metadata_col_nms, "threshold_seconds", "run_length", "data_stage", "date_processed"), collapse = "|"), names(.))], all_of(video_metadata_col_nms), threshold_seconds, run_length, data_stage, date_processed)
    
  } else {
    
    detectns4 <- detectns3 %>% 
      dplyr::select(all_of(general_metadata_col_nms), names(.)[-grep(paste(c(general_metadata_col_nms, "threshold_seconds", "run_length", "data_stage", "date_processed"), collapse = "|"), names(.))], threshold_seconds, run_length, data_stage, date_processed)
    
  }
  
  write.csv(detectns4, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts)
  
}