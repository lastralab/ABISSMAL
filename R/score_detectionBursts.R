
rm(list = ls())

library(tidyverse)
library(data.table)

run_length <- 2
file_nm <- "detections_across_sensors.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
general_metadata_cols <- c("chamber_id", "year", "month", "day")
integrate_perching <- TRUE
# path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
path <- "/home/gsvidaurre/Desktop/MANUSCRIPTS/Prep/ParentalCareTracking_MethodsPaper/ABS_2023_Talk"
data_dir <- "processed"
out_dir <- "processed"
out_file_nm <- "scored_detectionBursts.csv"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"


# Then I want another function that uses the output of this function as input, and returns which sensors were detected in each bout, all of the edges that were detected (e.g. Camera then RFID, etc) in the sequence, the inferred directionality by comparing all possible pairs of sensors if present in the bout, and the PIT tag ID(s) associated with the given bout.
# Keep the find perching events and bout detection functions. I also want to keep the option to integrate perching events fromn the raw RFID data in this second function

integrate_sensors <- function(file_nm, run_length = 2, sensor_id_col, timestamps_col, PIT_tag_col, general_metadata_cols, integrate_perching, path, data_dir, out_dir, out_file_nm = "scored_detectionBursts.csv", tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS"){
  
  # Get the current global options
  orig_opts <- options()
  
  # Set the number of digits for visualization. Under the hood there is full precision, but this helps for visual confirmation of decimal seconds
  options("digits.secs" = 6)
  
  detectns <- read.csv(file.path(path, data_dir, file_nm)) %>% 
    # Make sure that the timestamps are in the right format
    dplyr::mutate(
      start = as.POSIXct(format(as.POSIXct(start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      end = as.POSIXct(format(as.POSIXct(end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    ) %>% 
    rowid_to_column()
  
  glimpse(detectns) 
  # View(detectns)
  
  # Find all of the edges that occur in each sequence of sensor triggering events
  detectns_edges <- detectns %>%
    # slice(1:7) %>% 
    group_by(rowid) %>% 
    nest() %>% 
    # glimpse()
    # Make a data frame of the indices of edges in the sensor type sequences
    dplyr::mutate(
      # Map over the data frames (each nested data frame represents a different detection bout or burst)
      edges = map(
        .x = data,
        # Get the edges for each burst of detections
        # TKTK in testing, need to check that the correct number of columns are returned even when edges very in length across bouts
        .f = ~ dplyr::select(.x, start, end, event_seq) %>% 
          pmap_dfr(., function(start, end, event_seq){
            
            sensor_events <- strsplit(event_seq, split = "; ")[[1]]
            
            li <- cumsum(rle(sensor_events)[["lengths"]])
            
            # Get edges using dyads of the last indices
            edges <- unlist(lapply(1:length(li), function(i){
              if(i < length(li)){
                return(paste(sensor_events[li[i]], sensor_events[li[i + 1]], sep = " - "))
              } else if(i == 1 & length(li) == 1){
                return(NA)
              }
            }))
            
            # Use the first edge to label directionality
            if(!is.na(edges[1])){
              
              if(grepl("RFID", edges[1]) & grepl("Camera", edges[1])){
                
                if(edges[1] == "RFID - Camera"){
                  rfid_camera_direction <- "entrance"
                } else if(edges[1] == "Camera - RFID"){
                  rfid_camera_direction <- "exit"
                }
                
                rfid_outer_irbb_direction <- rfid_inner_irbb_direction <- outer_irbb_camera_direction <- inner_irbb_camera_direction <- outer_inner_irbb_direction <- NA
                
              } else if(grepl("RFID", edges[1]) & grepl("Beam", edges[1])){
                
                if(edges[1] == "Outer Beam Breaker - RFID"){
                  rfid_outer_irbb_direction <- "entrance"
                  rfid_inner_irbb_direction <- NA
                } else if(edges[1] == "RFID - Outer Beam Breaker"){
                  rfid_outer_irbb_direction <- "exit"
                  rfid_inner_irbb_direction <- NA
                } else if(edges[1] == "RFID - Inner Beam Breaker"){
                  rfid_inner_irbb_direction <- "entrance"
                  rfid_outer_irbb_direction <- NA
                } else if(edges[1] == "Inner Beam Breaker - RFID"){
                  rfid_inner_irbb_direction <- "exit"
                  rfid_outer_irbb_direction <- NA
                }
                
                rfid_camera_direction <- outer_irbb_camera_direction <- inner_irbb_camera_direction <- outer_inner_irbb_direction <- NA
                
              } else if(grepl("Camera", edges[1]) & grepl("Beam", edges[1])){
                
                if(edges[1] == "Outer Beam Breaker - Camera"){
                  outer_irbb_camera_direction <- "entrance"
                  inner_irbb_camera_direction <- NA
                } else if(edges[1] == "Camera - Outer Beam Breaker"){
                  outer_irbb_camera_direction <- "exit"
                  inner_irbb_camera_direction <- NA
                } else if(edges[1] == "Inner Beam Breaker - Camera"){
                  inner_irbb_camera_direction <- "entrance"
                  outer_irbb_camera_direction <- NA
                } else if(edges[1] == "Camera - Inner Beam Breaker"){
                  inner_irbb_camera_direction <- "exit"
                  outer_irbb_camera_direction <- NA
                }
                
                rfid_camera_direction <- rfid_outer_irbb_direction <- rfid_inner_irbb_direction <- outer_inner_irbb_direction <- NA
                
              } else if(!grepl("Camera|RFID", edges[1]) & grepl("Beam", edges[1])){
                
                if(edges[1] == "Outer Beam Breaker - Inner Beam Breaker"){
                  outer_inner_irbb_direction <- "entrance"
                } else if(edges[1] == "Inner Beam Breaker - Outer Beam Breaker"){
                  outer_inner_irbb_direction <- "exit"
                }
                
                rfid_camera_direction <- rfid_outer_irbb_direction <- rfid_inner_irbb_direction <- inner_irbb_camera_direction <- outer_irbb_camera_direction <- NA
                
              }
              
            } else {
              
              rfid_camera_direction <- rfid_outer_irbb_direction <- rfid_inner_irbb_direction <- outer_irbb_camera_direction <- inner_irbb_camera_direction <- outer_inner_irbb_direction <- NA
              
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
                rfid_camera_direction = rfid_camera_direction,
                rfid_outer_irbb_direction = rfid_outer_irbb_direction,
                rfid_inner_irbb_direction = rfid_inner_irbb_direction,
                outer_irbb_camera_direction = outer_irbb_camera_direction,
                inner_irbb_camera_direction = inner_irbb_camera_direction,
                outer_inner_irbb_direction = outer_inner_irbb_direction
              )
            
            return(tmp)
            
          })
      ) 
    ) %>%
    dplyr::select(-c(data)) %>% 
    unnest(`cols` = c(edges)) %>%
    ungroup()
  
  glimpse(detectns_edges)
  
  # If RFID data is present, then add back metadata about individual identities (RFID data) from the integration
  if(any(grepl("RFID", detectns$event_seq))){
    
    detectns_edges <- detectns_edges %>% 
      dplyr::inner_join(
        detectns %>% 
          dplyr::select("rowid", names(.)[grep("indiv", names(.))]),
        by = "rowid"
      ) %>% 
      dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))], names(.)[grep("indiv", names(.))])
    
  } else {
    
    detectns_edges <- detectns_edges %>% 
      dplyr::select(rowid, start, end, sensor_ids, names(.)[grep("Edge", names(.))], names(.)[grep("direction", names(.))])
    
  }
  
  glimpse(detectns_edges)
  
  # Integrate perching events if specified
  if(integrate_perching){
    
    perch_df <- read.csv(file.path(path, data_dir, "perching_events.csv"))  %>% 
      # Make sure that the timestamps are in the right format
      dplyr::mutate(
        perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
        perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    glimpse(perch_df)
    glimpse(detectns_edges)
    
    if(nrow(perch_df) > 0){
      
      # For each burst or bout of activity, figure out whether it occurred during a perching event and add that perching event to the final dataset
      tmp_df <- detectns_edges %>% 
        dplyr::select(rowid, start, end) %>% 
        pmap_dfr(., function(rowid, start, end){
          
          tmp_perching <- perch_df %>% 
            dplyr::filter(
              start >= perching_start & end <= perching_end 
            ) %>% 
            dplyr::rename(
              perching_PIT_tag = !!sym(PIT_tag_col)
            ) %>% 
            dplyr::mutate(
              de_rowid = rowid
            ) %>% 
            dplyr::select(de_rowid, perching_PIT_tag, perching_start, perching_end, perching_duration_s, unique_perching_event)
          
          return(tmp_perching)
          
        })
      
      glimpse(tmp_df)
      
      # Then add the perching event assignment for the given bout as new columns
      detectns_edges_p <- detectns_edges %>% 
        dplyr::left_join(
          tmp_df,
          by = c("rowid" = "de_rowid")
        )
      
    } else {
      
      warning("The perching events dataset was empty; skipping integration of perching events")
      detectns_edges_p <- detectns_edges
      
    }
    
  } else {
    
    detectns_edges_p <- detectns_edges
    
  }
  
  write.csv(detectns_edges_p, file.path(path, out_dir, out_file_nm), row.names = FALSE)
  
  # Reset the current global options
  options(orig_opts) 
  
  
}