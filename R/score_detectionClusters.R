#' @title score_detectionClusters
#' @description Score clusters of detections to infer directionality
#' 
#' @param file_nm A character vector of length 1. This argument should be the name and extension of one .csv file that contains clusters detected using the function `find_detectionClusters`. For instance, c("detection_clusters.csv"). In this spreadsheet, each row should be a unique cluster of detections. The spreadsheet must contain all the columns specified in the subsequent arguments.
#' @param rfid_label A character string. This argument is the label for the RFID sensor in the sensor_id_col_nm column in pre-processed data (e.g. "RFID"). Set this argument to NULL if RFID data is not present in the input dataset. The default is NULL.
#' @param camera_label A character string. This argument is the label for the camera sensor that records video in the sensor_id_col_nm column in pre-processed data (e.g. "Camera"). Set this argument to NULL if video data is not present in the input dataset. The default is NULL.
#' @param outer_irbb_label A character string. This argument is the label for the outer pair of beam breakers in the sensor_id_col_nm column in pre-processed data (e.g. "Outer Beam Breaker"). Set this argument to NULL if outer beam breaker data is not present in the input dataset. The default is NULL.
#' @param inner_irbb_label A character string. This argument is the label for the inner pair of beam breakers in the sensor_id_col_nm column in pre-processed data (e.g. "Inner Beam Breaker"). Set this argument to NULL if inner beam breaker data is not present in the input dataset. The default is NULL.
#' @param video_metadata_col_nms A character vector. This argument should be a string of the video metadata column names that will be carried through into the resulting data frame representing the integrated data. For instance: c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"). These columns will be added as later columns in resulting data frame, in the same order in which they were specified. The default is NULL.
#' @param integrate_perching Boolean. If TRUE, then the perching events identified using `find_perching_events` with either RFID and/or beam breaker data will be integrated with this dataset. This integration is done by finding detection clusters that occurred within the duration of a perching event. If FALSE, then perching events will not be integrated.
#' @param perching_dataset A character string. Use "RFID", "IRBB", or "RFID-IRBB" to specify which dataset of perching events to integrate. Specifying "RFID-IRBB" means that perching events detected using both sensor types will be integrated. The default is NULL.
#' @param perching_prefix A character string. The prefix for the file name of the perching events spreadsheet(s). This character string needs to contain all of the symbols in the file name(s) up until the sensor label and extension (e.g. "perching_events_"). The default is NULL.
#' @param sensor_id_col_nm A character string. This argument is the name of the metadata column in the perching event data to be integrated that contains information about the data type (e.g. "sensor_id"). The default is NULL, since integrating perching data is not a requirement.
#' @param PIT_tag_col_nm A character string. This argument is the name of the metadata column in the perching event data to be integrated that contains information about the PIT tags detected by the RFID antenna (e.g. "PIT_tag_ID"). The default is NULL, since integrating perching data is not a requirement.
#' @param pixel_col_nm A character string. This argument is the column name for the column that contains the number of pixels that triggered each unique video recording event and which will be used to calculate a magnitude of movement score. The default is `NULL`, since this argument is not needed for detection clusters without video recording events.
#' @param video_width A numeric argument. The width of each video frame recorded by ABISSMAL specified as the number of pixels (for instance 1280, which is the default video width currently used by ABISSMAL). This argument is used to specify the total number of pixels in a given video frame in order to generate magnitude of movement calculations. The default argument is NULL.
#' @param video_height A numeric argument. The height of each video frame recorded by ABISSMAL specified as the number of pixels. (for instance 720, which is the default video width currently used by ABISSMAL). This argument is used to specify the total number of pixels in a given video frame in order to generate magnitude of movement calculations. The default argument is NULL.
#' @param integrate_proproc_video A Boolean argument. When TRUE, the dataset of pre-processed video recording events will be integrated back into the dataset of scored detection clusters. The purpose of this integration is to avoid dropping single video recording events that occurred when no other sensors triggered, since find_detectionClusters searches for sequences of triggering events and therefore ignores singlet video recordings. When this argument is FALSE, this integration will not be performed. 
#' @param video_file_nm A character string. This argument should be the name (plus extension) of the spreadsheet that holds the pre-processed video recording events. The default is NULL, but this argument cannot be NULL when integrate_proproc_video is TRUE.
#' @param timestamps_col_nm A character string. This argument is the column name that holds timestamps of video recording events in the pre-processed video data. The default is NULL, but this argument cannot be NULL when integrate_proproc_video is TRUE.
#' @param path A character string. This argument should be the path on the local computer or external hard drive specifying where the data is saved across sensors for a given experimental setup. For instance, "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022".
#' @param data_dir A character string. This argument should be the name of the directory where the pre-processed data that is used as input is saved inside the path above. For instance, "processed".
#' @param out_dir A character string. This argument should be the name of a directory specifying where the .csv file of integrated data should be saved. For instance, "processed". This folder will be appended to the path and created as a new directory if it doesn't already exist.
#' @param out_file_nm A character string. The name (plus extension) of the resulting file that will be written to out_dir. The default is "scored_detectionClusters.csv"
#' @param tz A character string. This argument should contain the timezone used for converting timestamps to POSIXct format. For instance, "America/New York". See the base function `as.POSIXct` for more information.
#' @param POSIXct_format A character string. This argument should contain the format used to converting timestamps to POSIXct format. The default is "%Y-%m-%d %H:%M:%OS6" to return timestamps with milliseconds in decimal format. See the base function `as.POSIXct` for more information.
#' 
#' @details `score_detectionClusters` uses the order in which sensors triggered within clusters of detections identified by `find_detectionCusters` to score the direction of movement events. The function finds edges or transitions between sensor labels in the sequence of detections for each cluster. Then the function uses the order of the sensor labels in the first edge to label the directionality of movement events. Note that the function requires data from at least two sensor types (or two beam breaker pairs). When using beam breaker data, the function expects data from two pairs of these sensors. This function can also integrate clusters of detections with perching events identified by the function `find_perching_events` (e.g. when an individual was perched in the entrance of the nest container).
#' 
#' @return A spreadsheet in .csv format with the metadata columns from the original pre-processed data used as input (including individual identity information from RFID data), columns indicating the start and end time of each detection cluster, all the possible edges or transitions detected in the sequence of sensor events, the inferred directionality of sensor events, the rule used to score detection (using the first edge only), and the magnitude of movement. Then magnitude of movement is calculated as the percentage of the observed pixels that changed color during motion detection with respect to the total number of pixels in a given video frame: ( observed number of pixels that changed / (video_width x video_height) ) * 100. The function also integrates pre-processed video recording events that were dropped while searching for detection clusters. These video recording events can be added back to the detection cluster dataset using the arguments `integrate_preproc_video`, `video_file_nm`, and `timestamps_col_nm`. The function also adds a column of inferred location of movement, since these video recording events may represent movements inside of the container that were not picked up by other sensors. All detection clusters that were picked up by other sensors are scored as movements that likely occurred at the entance of the nest container.
#' 
#' Each row in the resulting .csv file is a unique detection cluster. Information about the date of processing is also contained in the resulting spreadsheet.

score_detectionClusters <- function(file_nm, rfid_label = NULL, camera_label = NULL, outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms, integrate_perching, perching_dataset = NULL, perching_prefix = NULL, sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video, video_file_nm = NULL, timestamps_col_nm = NULL, path, data_dir, out_dir, out_file_nm = "scored_detectionClusters.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  # Get the user-specified values for each formal argument of the current function
  f_args <- getFunctionParameters()
  
  # Check that arguments that cannot ever be non-NULL are not NULL
  expect_nonNulls <- f_args[grep(paste(paste("^", c("file_nm", "integrate_perching", "integrate_preproc_video", "path", "data_dir", "out_dir", "out_file_nm", "tz", "POSIXct_format"), "$", sep = ""), collapse = "|"), names(f_args))]
  
  invisible(sapply(1:length(expect_nonNulls), function(i){
    check_not_null(names(expect_nonNulls[i]), expect_nonNulls[[i]])
  }))
  
  expect_bool <- c("integrate_perching", "integrate_preproc_video")
  
  # Check that the formal arguments that should be Boolean are Boolean
  invisible(sapply(1:length(expect_bool), function(i){
    check_boolean(expect_bool[i], f_args[[grep(paste(paste("^", expect_bool[i], "$", sep = ""), collapse = "|"), names(f_args))]])
  }))
  
  # Check that the sensor suffix in the input file names for the perching data are correct
  if(integrate_perching){
    if(!is.null(perching_dataset)){
      if(!perching_dataset %in% c("RFID", "IRBB", "RFID-IRBB")){
        stop("The perching dataset is not specified correctly")
      }
    }
  }
  
  # Check that the sensor labels are specified correctly given the different combinations expected by this function
  if(all(is.null(rfid_label) & is.null(outer_irbb_label) & is.null(inner_irbb_label) & is.null(camera_label))){
    stop("Data from at least two sensor types, or two beam breaker pairs, must be specified")
  }
  
  if(!is.null(rfid_label) & all(is.null(outer_irbb_label) & is.null(inner_irbb_label) & is.null(camera_label))){
    stop("The RFID data must be accompanied by beam breaker and/or video data")
  }
  
  if(!is.null(camera_label) & all(is.null(outer_irbb_label) & is.null(inner_irbb_label) & is.null(rfid_label))){
    stop("The video data must be accompanied by beam breaker and/or RFID data")
  }
  
  if(!is.null(outer_irbb_label) & is.null(inner_irbb_label)){
    stop("The outer beam breaker data must be accompanied by inner beam breaker data")
  }
  
  if(!is.null(inner_irbb_label) & is.null(outer_irbb_label)){
    stop("The inner beam breaker data must be accompanied by outer beam breaker data")
  }
  
  # Check that if camera_label is not NULL, then video_metadata_cols and columns for calculating the magnitude of movement are also not NULL
  if(!is.null(camera_label)){
    
    check_not_null("video_metadata_col_nms", f_args[[grep(paste(paste("^", "video_metadata_col_nms", "$", sep = ""), collapse = "|"), names(f_args))]])
    
    vid_calc_cols <- c("pixel_col_nm", "video_width", "video_height")
    
    lapply(1:length(vid_calc_cols), function(i){
      
      check_not_null(vid_calc_cols[i], f_args[[grep(paste(paste("^", vid_calc_cols[i], "$", sep = ""), collapse = "|"), names(f_args))]])
      
    })
    
    # Check that arguments that should be numeric are numeric
    expect_numeric <- c("video_width", "video_height")
    
    invisible(sapply(1:length(expect_numeric), function(i){
      check_numeric(expect_numeric[i], f_args[[grep(paste(paste("^", expect_numeric[i], "$", sep = ""), collapse = "|"), names(f_args))]])
    }))
    
  }
  
  # If integrate_preproc_video is TRUE, then check that the other two necessary arguments are not NULL
  if(integrate_preproc_video){
    
    vid_int_cols <- c("video_file_nm", "timestamps_col_nm")
    
    lapply(1:length(vid_int_cols), function(i){
      
      check_not_null(vid_int_cols[i], f_args[[grep(paste(paste("^", vid_int_cols[i], "$", sep = ""), collapse = "|"), names(f_args))]])
      
    })
    
  }
  
  # Check that the formal arguments that should be NULL are NULL, and vice versa
  if(integrate_perching){
    
    expect_nonNull2 <- c("sensor_id_col_nm", "perching_dataset", "perching_prefix")
    
    if(!is.null(perching_dataset)){
      if(grepl("RFID", perching_dataset)){
        
        expect_nonNull2 <- c(expect_nonNull2, "PIT_tag_col_nm")
        
      }
    }
    
    invisible(sapply(1:length(expect_nonNull2), function(i){
      check_not_null(expect_nonNull2[i], f_args[[grep(paste(paste("^", expect_nonNull2[i], "$", sep = ""), collapse = "|"), names(f_args))]])
    }))
    
  } else if(!integrate_perching){
    
    expect_nulls <- c("sensor_id_col_nm", "PIT_tag_col_nm", "perching_dataset", "perching_prefix")
    
    invisible(sapply(1:length(expect_nulls), function(i){
      check_null(expect_nulls[i], f_args[[grep(paste(paste("^", expect_nulls[i], "$", sep = ""), collapse = "|"), names(f_args))]])
    }))
    
  }
  
  # Get all the NULL arguments (e.g. camera_label or sensor_id_col_nm may be NULL)
  wh_null <- which(unlist(sapply(1:length(f_args), function(i){
    is.null(f_args[[i]])
  })))
  
  if(length(wh_null) > 0){
    
    if(!exists("expect_numeric")){
      
      expect_strings <- f_args[-grep(paste(paste("^", c(expect_bool, names(f_args)[wh_null]), "$", sep = ""), collapse = "|"), names(f_args))]
      
    } else if(exists("expect_numeric")){
      
      expect_strings <- f_args[-grep(paste(paste("^", c(expect_bool, expect_numeric, names(f_args)[wh_null]), "$", sep = ""), collapse = "|"), names(f_args))]
      
    }
    
  }
  
  if(exists("expect_strings")){
    
    invisible(sapply(1:length(expect_strings), function(i){
      check_string(names(expect_strings[i]), expect_strings[[i]])
    }))
    
  }
  
  # Check that the input directory exists
  check_dirs(path, data_dir)
  
  # Check that the input file exists in the input directory
  check_file(file.path(path, data_dir), file_nm)
  
  # Check that the perching file(s) also exist in the input directory
  if(integrate_perching){
    
    if(!is.null(perching_dataset)){
      
      if(perching_dataset %in% c("RFID", "IRBB")){
        
        check_file(file.path(path, data_dir), paste(perching_prefix, perching_dataset, ".csv", sep = ""))
        
      } else if(perching_dataset %in% c("RFID", "IRBB")){
        
        ps <- strsplit(perching_dataset, split = "-")[[1]]
        
        sapply(1:length(ps), function(i){
          
          check_file(file.path(path, data_dir), paste(perching_prefix, ps[i], ".csv", sep = ""))
          
        })
        
      }
      
    }
    
  }
  
  # Create the directory for saving the output file if it doesn't already exist
  if(!dir.exists(file.path(path, out_dir))){
    dir.create(file.path(path, out_dir))
  }
  
  # Read in the detection clusters
  detectns <- read.csv(file.path(path, data_dir, file_nm))
  
  # Check that the data contains the columns with timestamps
  tmp_cols <- c("start", "end")
  
  invisible(lapply(1:length(tmp_cols), function(z){
    check_fArgs_data_cols(tmp_cols[z], detectns)
  }))
  
  detectns <- detectns %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      start = as.POSIXct(format(as.POSIXct(start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      end = as.POSIXct(format(as.POSIXct(end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    rowid_to_column()
  
  # Check that this object is a data frame
  check_df_class(detectns)
  
  # Check that date-related columns are found in the data
  expected_cols <- c("year", "month", "day")
  
  invisible(sapply(1:length(expected_cols), function(i){
    check_data_cols(expected_cols[i], detectns)
  }))
  
  # Check that the date-related columns do not have NAs
  invisible(sapply(1:length(expected_cols), function(i){
    check_cols_nas(expected_cols[i], detectns)
  }))
  
  # Check that columns with timestamps are in the right format
  check_tstmps_cols("start", detectns, "%Y-%m-%d %H:%M:%OS6")
  check_tstmps_cols("end", detectns, "%Y-%m-%d %H:%M:%OS6")
  
  # Check that the columns in video_metadata_cols are in the main input data when camera_label is not NULL
  if(!is.null(camera_label)){
    
    expected_cols <- f_args[[grep(paste(paste("^", "video_metadata_col_nms", "$", sep = ""), collapse = "|"), names(f_args))]]
    
    invisible(sapply(1:length(expected_cols), function(i){
      check_fArgs_data_cols(expected_cols[[i]], detectns)
    }))
    
  }
  
  # Create a table of rules for scoring the direction of movement events
  ent_rules <- c(
    paste(outer_irbb_label, rfid_label, sep = " - "),
    paste(rfid_label, inner_irbb_label, sep = " - "),
    paste(outer_irbb_label, inner_irbb_label, sep = " - "),
    paste(outer_irbb_label, camera_label, sep = " - "),
    paste(inner_irbb_label, camera_label, sep = " - "),
    paste(rfid_label, camera_label, sep = " - ")
  )
  
  exi_rules <- c(
    paste(rfid_label, outer_irbb_label, sep = " - "),
    paste(inner_irbb_label, rfid_label, sep = " - "),
    paste(inner_irbb_label, outer_irbb_label, sep = " - "),
    paste(camera_label, outer_irbb_label, sep = " - "),
    paste(camera_label, inner_irbb_label, sep = " - "),
    paste(camera_label, rfid_label, sep = " - ")
  )
  
  dir_rules <- data.frame(
    rule = c(ent_rules, exi_rules),
    direction = c(rep("entrance", length(ent_rules)), rep("exit", length(exi_rules)))
  ) %>% 
    # Drop incomplete rules generated by an empty string when a sensor is not specified
    dplyr::filter(
      !grepl("^-|-$", rule)
    )
  
  # Find all of the edges that occur in each sequence of sensor triggering events
  detectns_edges <- detectns %>%
    group_by(rowid) %>% 
    nest() %>% 
    # Make a data frame of the indices of edges in the sensor type sequences
    dplyr::mutate(
      # Each nested data frame represents a different cluster of detections
      edges = map(
        .x = data,
        # Get the edges for each burst of detections
        .f = ~ dplyr::select(.x, start, end, event_seq) %>% 
          pmap_dfr(., function(start, end, event_seq){
            
            sensor_events <- strsplit(event_seq, split = "; ")[[1]]
            
            li <- cumsum(rle(sensor_events)[["lengths"]])
            
            edges <- unlist(lapply(1:length(li), function(i){
              if(i < length(li)){
                return(paste(sensor_events[li[i]], sensor_events[li[i + 1]], sep = " - "))
              } else if(i == 1 & length(li) == 1){
                return(NA)
              }
            }))
            
            # Use the first edge to label directionality
            if(!is.na(edges[1])){
              
              if(edges[1] %in% dir_rules[["rule"]]){
                
                direction <- dir_rules[["direction"]][grep(edges[1], dir_rules[["rule"]])]
                
                rule <- dir_rules[["rule"]][grep(edges[1], dir_rules[["rule"]])]
                
              } else {
                
                direction <- "not scored"
                rule <- "none"
                
              }
              
            } else {
              
              direction <- NA
              rule <- NA
              
            }
            
            # Get edges using dyads of the last indices
            edges <- data.frame(t(matrix(edges)))
            
            names(edges) <- paste("Edge", seq(1, length(edges), 1), sep = "_")
            
            # Add the unique sensor labels that were present in each bout
            sensor_ids <- paste(unique(sensor_events), collapse = "; ")
            
            tmp <- data.frame(
              start = start,
              end = end,
              sensor_ids = sensor_ids
            ) %>% 
              bind_cols(
                edges
              ) %>% 
              dplyr::mutate(
                direction_scored = direction,
                direction_rule = rule
              )
            
            return(tmp)
            
          })
      ) 
    ) %>%
    dplyr::select(-c(data)) %>% 
    unnest(`cols` = c(edges)) %>%
    ungroup()
  
  # If only RFID data is present, then add back metadata about individual identities from the integration
  if(!is.null(rfid_label)){
    
    if(any(grepl(rfid_label, detectns$event_seq)) & is.null(camera_label)){
      
      detectns_edges2 <- detectns_edges %>% 
        dplyr::inner_join(
          detectns %>% 
            dplyr::select("rowid", names(.)[grep("indiv", names(.))]),
          by = "rowid"
        ) %>%
        dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))], names(.)[grep("indiv", names(.))])
      
    } else {
      
      detectns_edges2 <- detectns_edges %>% 
        dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))])
      
    }
    
  }
  
  # If only Camera data is present, then add back metadata about videos from the integration
  if(!is.null(camera_label)){
    
    if(any(grepl(camera_label, detectns$event_seq)) & is.null(rfid_label)){
      
      detectns_edges2 <- detectns_edges %>% 
        dplyr::inner_join(
          detectns %>% 
            dplyr::select("rowid", names(.)[grep(paste(video_metadata_col_nms, collapse = "|"), names(.))]),
          by = "rowid"
        ) %>% 
        dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))], names(.)[grep(paste(video_metadata_col_nms, collapse = "|"), names(.))])
      
    } else {
      
      detectns_edges2 <- detectns_edges %>% 
        dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))])
      
    }
    
  }
  
  # If RFID and Camera data is present, then add back individual identity and video metadata
  if(!is.null(rfid_label) & !is.null(camera_label)){
    
    if(any(grepl(rfid_label, detectns$event_seq)) & any(grepl(camera_label, detectns$event_seq))){
      
      detectns_edges2 <- detectns_edges %>% 
        dplyr::inner_join(
          detectns %>% 
            dplyr::select("rowid", names(.)[grep("indiv", names(.))], names(.)[grep(paste(video_metadata_col_nms, collapse = "|"), names(.))]),
          by = "rowid"
        ) %>% 
        dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))], names(.)[grep("indiv", names(.))], names(.)[grep(paste(video_metadata_col_nms, collapse = "|"), names(.))])
      
    } else {
      
      detectns_edges2 <- detectns_edges %>%
        dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))])
      
    }
    
  }
  
  # If only beam breaker data is present, then return the current columns in order
  if(is.null(rfid_label) & is.null(camera_label)){
    
    detectns_edges2 <- detectns_edges %>% 
      dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))])
    
  }
  
  # Integrate the prep-processed video recording events if specified, and if the camera is not NULL
  if(integrate_preproc_video & !is.null(camera_label)){
    
    # Read in the pre-processed video data depending on which datasets were specified
    # Drop all post-trigger video recordings (these have the same timestamp as the respective pre-trigger recordings)
    video_pp <- read.csv(file.path(path, data_dir, video_file_nm))
    
    tmp_col_nm <- video_metadata_col_nms[grep("file_name", video_metadata_col_nms)]
    
    video_pp <- video_pp[-which(grepl("post_trigger", video_pp[[tmp_col_nm]])), ]
    
    video_pp <- video_pp %>% 
      # Make sure that the timestamps are in the right format
      dplyr::mutate(
        !!timestamps_col_nm := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col_nm), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Find all of the pre-processed video recording events that are not in the current dataset of detection clusters. Search for video recording events that do not fall within the start and end timestamps of the detection clusters
    names(video_pp)[grep(timestamps_col_nm, names(video_pp))] <- "timestamp"
    
    video_pp2 <- video_pp %>% 
      rowid_to_column() %>% 
      dplyr::select(timestamp, rowid) %>% 
      # For each pre-processed video recording event, figure out whether the recording timestamp occurred within the start and end of any detection cluster
      pmap_dfr(., function(rowid, timestamp){
        
        tmp <- detectns_edges2 %>% 
          dplyr::filter(
            timestamp >= start & timestamp <= end
          )
        
        if(nrow(tmp) > 0){
          
          return(data.frame(rowid, timestamp, captured = "Yes"))
          
        } else {
          
          return(data.frame(rowid, timestamp, captured = "No"))
          
        }
        
      })
    
    if(nrow(video_pp2) > 0){
      
      # Get the pre-processed video recording events that are not captured in the detection clusters and add these to the scored detection cluster dataset as new rows, with a metadata about the inferred local of movement
      detectns_edges3 <- detectns_edges2 %>% 
        dplyr::mutate(
          inferredMovement_Location = "container_entrance"
        ) 
      
      # Modify the pre-processed video recordings object to have the same number of Edge columns as detections_edges3, as well as other columns (fill these with NAs)
      video_pp3 <- video_pp %>% 
        rowid_to_column() %>% 
        dplyr::full_join(
          video_pp2 %>% 
            dplyr::select(rowid, captured),
          by = "rowid"
        ) %>%
        dplyr::filter(captured == "No") %>%
        dplyr::select(timestamp, all_of(video_metadata_col_nms)) %>% 
        dplyr::mutate(
          start = timestamp,
          end = timestamp,
          sensor_ids = camera_label,
          direction_scored = NA,
          direction_rule = NA,
          indiv1_id = NA,
          indiv2_id = NA,
          total_indiv1_detections = NA,
          total_indiv2_detections = NA,
          individual_initiated = NA,
          individual_ended = NA,
          inferredMovement_Location = "inside_container"
        ) %>% 
        rowid_to_column()
      
      edge_cols <- names(detectns_edges3)[grep("Edge", names(detectns_edges3))]
      edge_nas <- data.frame(matrix(ncol = length(edge_cols), nrow = nrow(video_pp3)))
      names(edge_nas) <- edge_cols
      
      video_pp3 <- video_pp3 %>% 
        bind_cols(
          edge_nas
        )
      
      detectns_edges3 <- detectns_edges3 %>% 
        bind_rows(
          video_pp3 %>% 
            dplyr::select(names(detectns_edges3))
        ) %>% 
        # Fix the row IDs
        dplyr::select(-c(rowid)) %>% 
        rowid_to_column() 
      
    } else {
      
      detectns_edges3 <- detectns_edges2
      
      warning("All video recording events were already captured in the current set of detection clusters; skipping integration of pre-processed video recording events")
      
    }
    
  } else {
    
    detectns_edges3 <- detectns_edges2
    
  }
  
  # Perform the magnitude of movement calculations for video recording events if the camera label is not NULL
  if(!is.null(camera_label)){
    
    detectns_edges3 <- detectns_edges3 %>% 
      dplyr::mutate(
        magnitude_movement = round(( !!sym(pixel_col_nm) / (video_width * video_height) ) *100, 4)
      ) %>% 
      dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))], names(.)[grep("indiv", names(.))], names(.)[grep(paste(c(video_metadata_col_nms, "magnitude_movement", "inferredMovement_Location"), collapse = "|"), names(.))])
    
  }
  
  # Integrate perching events if specified
  if(integrate_perching){
    
    # Read in perching event data depending on which datasets were specified
    if(!is.null(perching_dataset)){
      
      if(perching_dataset != "RFID-IRBB"){
        
        perch_df <- read.csv(file.path(path, data_dir, paste(perching_prefix, perching_dataset, ".csv", sep = "")))  
        
        # Check that the expected columns are present
        if(perching_dataset == "RFID"){
          
          expected_cols <- c("sensor_id_col_nm", "PIT_tag_col_nm")
          
        } else {
          
          expected_cols <- c("sensor_id_col_nm")
          
        }
        
        expected_cols <- invisible(sapply(1:length(expected_cols), function(i){
          return(f_args[[grep(paste(paste("^", expected_cols[i], "$", sep = ""), collapse = "|"), names(f_args))]])
        }))
        
        expected_cols <- c(expected_cols, "perching_start", "perching_end")
        
        invisible(sapply(1:length(expected_cols), function(i){
          check_fArgs_data_cols(expected_cols[i], perch_df)
        }))
        
        perch_df <- perch_df %>% 
          # Make sure that the timestamps are in the right format
          dplyr::mutate(
            perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
            perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
          )
        
        check_tstmps_cols("perching_start", perch_df, "%Y-%m-%d %H:%M:%OS6")
        check_tstmps_cols("perching_end", perch_df, "%Y-%m-%d %H:%M:%OS6")
        
      } else if(perching_dataset == "RFID-IRBB"){
        
        tmp_file_nms <- c(paste(perching_prefix, strsplit(perching_dataset, split = "-")[[1]][1], ".csv", sep = ""), paste(perching_prefix, strsplit(perching_dataset, split = "-")[[1]][2], ".csv", sep = ""))
        
        perch_df <- data.table::rbindlist(lapply(1:length(tmp_file_nms), function(z){
          tmp <- read.csv(file.path(path, data_dir, tmp_file_nms[z]))
          # Add a PIT tag column with NAs for the beam breaker data 
        }), fill = TRUE)
        
        # Check that the expected columns are present
        expected_cols <- c("sensor_id_col_nm", "PIT_tag_col_nm")
        
        expected_cols <- invisible(sapply(1:length(expected_cols), function(i){
          return(f_args[[grep(paste(paste("^", expected_cols[i], "$", sep = ""), collapse = "|"), names(f_args))]])
        }))
        
        expected_cols <- c(expected_cols, "perching_start", "perching_end")
        
        invisible(sapply(1:length(expected_cols), function(i){
          check_fArgs_data_cols(expected_cols[i], perch_df)
        }))
        
        perch_df <- perch_df %>% 
          # Make sure that the timestamps are in the right format
          dplyr::mutate(
            perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
            perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
          )
        
        # Check that columns with timestamps are in the right format
        check_tstmps_cols("perching_start", perch_df, "%Y-%m-%d %H:%M:%OS6")
        check_tstmps_cols("perching_end", perch_df, "%Y-%m-%d %H:%M:%OS6")
        
      }
      
    }
    
    if(nrow(perch_df) > 0 & !is.null(perching_dataset)){
      
      # For each detection cluster, figure out whether it occurred during a perching event and add that perching event to the final dataset
      if(perching_dataset %in% c("RFID", "RFID-IRBB")){
        
        tmp_df <- detectns_edges3 %>% 
          dplyr::select(rowid, start, end) %>% 
          pmap_dfr(., function(rowid, start, end){
            
            tmp_perching <- perch_df %>% 
              dplyr::filter(
                start >= perching_start & end <= perching_end 
              ) %>% 
              dplyr::rename(
                perching_PIT_tag = !!sym(PIT_tag_col_nm)
              ) %>% 
              dplyr::mutate(
                de_rowid = rowid
              ) %>% 
              dplyr::select(de_rowid, all_of(sensor_id_col_nm), perching_PIT_tag, perching_start, perching_end, perching_duration_s)
            
            return(tmp_perching)
            
          })
        
      } else if(perching_dataset == "IRBB"){
        
        tmp_df <- detectns_edges3 %>% 
          dplyr::select(rowid, start, end) %>% 
          pmap_dfr(., function(rowid, start, end){
            
            tmp_perching <- perch_df %>% 
              dplyr::filter(
                start >= perching_start & end <= perching_end 
              ) %>% 
              dplyr::mutate(
                de_rowid = rowid
              ) %>% 
              dplyr::select(de_rowid, all_of(sensor_id_col_nm), perching_start, perching_end, perching_duration_s)
            
            return(tmp_perching)
            
          })
        
      } 
      
      # Then add the perching event assignment for the given bout as new columns, depending on the perching datasets used as input
      if(perching_dataset %in% c("RFID", "IRBB")){
        
        detectns_edges_p <- detectns_edges3 %>% 
          dplyr::left_join(
            tmp_df %>% 
              dplyr::rename(
                perching_sensor = !!sym(sensor_id_col_nm)
              ),
            by = c("rowid" = "de_rowid")
          )
        
        # If both RFID and IRBB were specified as perching datasets, then add back perching events from each dataset as separate columns
      } else if(perching_dataset == "RFID-IRBB"){
        
        detectns_edges_p <- detectns_edges3 %>% 
          dplyr::left_join(
            tmp_df %>% 
              dplyr::filter(!!sym(sensor_id_col_nm) == "RFID") %>% 
              dplyr::rename(
                `perching_rfid_start` = "perching_start",
                `perching_rfid_end` = "perching_end",
                `perching_rfid_duration_s` = "perching_duration_s"
              ) %>% 
              dplyr::select(-c(all_of(sensor_id_col_nm))),
            by = c("rowid" = "de_rowid")
          ) %>% 
          dplyr::left_join(
            tmp_df %>% 
              dplyr::filter(!!sym(sensor_id_col_nm) == "Outer Beam Breaker") %>% 
              dplyr::rename(
                `perching_outer_irbb_start` = "perching_start",
                `perching_outer_irbb_end` = "perching_end",
                `perching_outer_irbb_duration_s` = "perching_duration_s"
              ) %>% 
              dplyr::select(-c(all_of(sensor_id_col_nm), "perching_PIT_tag")),
            by = c("rowid" = "de_rowid")
          ) %>% 
          dplyr::left_join(
            tmp_df %>% 
              dplyr::filter(!!sym(sensor_id_col_nm) == "Inner Beam Breaker") %>% 
              dplyr::rename(
                `perching_inner_irbb_start` = "perching_start",
                `perching_inner_irbb_end` = "perching_end",
                `perching_inner_irbb_duration_s` = "perching_duration_s"
              ) %>% 
              dplyr::select(-c(all_of(sensor_id_col_nm), "perching_PIT_tag")),
            by = c("rowid" = "de_rowid")
          )
        
      }
      
    } else {
      
      warning("The perching events dataset was empty; skipping integration of perching events")
      detectns_edges_p <- detectns_edges3
      
    }
    
  } else {
    
    detectns_edges_p <- detectns_edges3
    
  }
  
  detectns_edges_p2 <- detectns_edges_p %>% 
    # Order by the timestamps
    dplyr::arrange(
      -desc(start)
    ) %>% 
    dplyr::select(-c(rowid)) %>%
    dplyr::mutate(
      data_stage = "integrated",
      date_processed = paste(Sys.Date(), Sys.time(), sep = " ")
    )
  
  write.csv(detectns_edges_p2, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts) 
  
}