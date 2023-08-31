# G. Smith-Vidaurre
# 03 January 2023

# See more info on the testthat package: https://r-pkgs.org/testing-basics.html

# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_detectionClusters.R")

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

########## Testing output ########## 

test_that("The function detects the expected number of clusters using RFID data", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  
  # Create 4 clusters of detections: each cluster consists of 2 detections spaced 1 second apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 1
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = c(starts, ends)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "pre_processed_data_RFID.csv"), row.names = FALSE)
  
  # Note that the run length needs to be set to 1 in order to correctly detect detection clusters of length 2
  find_detectionClusters(file_nms = "pre_processed_data_RFID.csv", threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "detection_clusters.csv"))
  
  # Test that the results are 4 detection clusters, or the length of the start timestamps created above
  expect_equal(nrow(test_res), length(starts))
  
  # Test that the sequence of sensor triggering events is correct
  test_seq <- paste(c(
    rep("RFID", length(c(starts[1], ends[1])))
  ), collapse = "; ")
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$event_seq[i], test_seq)
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  test_detectns <- length(c(starts[1], ends[1]))
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], test_detectns)
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function detects the expected number of clusters using data from RFID and 2 beam breaker pairs", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("pbapply")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(pbapply)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for two sensor types
  
  # Create 4 clusters of detections: each cluster consists of 2 detections per sensor spaced 1 second apart
  starts_rfid <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends_rfid <- starts_rfid + 1
  
  starts_irbb_o <- starts_rfid + 2
  ends_irbb_o <- starts_irbb_o + 1
  
  starts_irbb_i <- starts_irbb_o + 2
  ends_irbb_i <- starts_irbb_i + 1
  
  # Create separate spreadsheets for the simulated RFID and IRBB data
  rfid_ts <- data.frame(timestamp_ms = c(starts_rfid, ends_rfid)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  irbb_ts <- data.frame(timestamp_ms = c(starts_irbb_o, ends_irbb_o, starts_irbb_i, ends_irbb_i)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      sensor_id = c(rep("Outer Beam Breaker", length(c(starts_irbb_o, ends_irbb_o))), rep("Inner Beam Breaker", length(c(starts_irbb_i, ends_irbb_i)))),
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(rfid_ts, file.path(tmp_path, "processed", "pre_processed_data_RFID.csv"), row.names = FALSE)
  write.csv(irbb_ts, file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv"), row.names = FALSE)
  
  # Note that the run length needs to be set to 1 in order to correctly detect detection clusters of length 2
  find_detectionClusters(file_nms = c("pre_processed_data_RFID.csv", "pre_processed_data_IRBB.csv"), threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "detection_clusters.csv"))
  
  # Test that the results are 4 detection clusters, or the length of the start timestamps for 1 sensor created above
  expect_equal(nrow(test_res), length(starts_rfid))
  
  # Test that the sequence of sensor triggering events is correct
  test_seq <- paste(c(
    rep("RFID", length(c(starts_rfid[1], ends_rfid[1]))),
    rep("Outer Beam Breaker", length(c(starts_irbb_o[1], ends_irbb_o[1]))),
    rep("Inner Beam Breaker", length(c(starts_irbb_i[1], ends_irbb_i[1])))
  ), collapse = "; ")
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$event_seq[i], test_seq)
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  test_detectns <- length(c(starts_rfid[1], ends_rfid[1]))
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], test_detectns)
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function detects the expected number of clusters using data from RFID, 2 beam breaker pairs, and the camera", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("pbapply")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(pbapply)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for 3 sensor types
  
  # Create 4 clusters of detections: each cluster consists of 2 detections per sensor spaced 1 second apart
  starts_rfid <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends_rfid <- starts_rfid + 1
  
  starts_irbb_o <- starts_rfid + 2
  ends_irbb_o <- starts_irbb_o + 1
  
  starts_irbb_i <- starts_irbb_o + 2
  ends_irbb_i <- starts_irbb_i + 1
  
  starts_camera <- starts_irbb_i + 2
  ends_camera <- starts_camera + 1
  
  # Create separate spreadsheets for the simulated RFID, IRBB, and camera data
  rfid_ts <- data.frame(timestamp_ms = c(starts_rfid, ends_rfid)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  irbb_ts <- data.frame(timestamp_ms = c(starts_irbb_o, ends_irbb_o, starts_irbb_i, ends_irbb_i)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      sensor_id = c(rep("Outer Beam Breaker", length(c(starts_irbb_o, ends_irbb_o))), rep("Inner Beam Breaker", length(c(starts_irbb_i, ends_irbb_i)))),
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  # Each video recording timestamp is repeated twice to simulate our current setup of a pre-motion trigger and post-motion trigger video per timestamp representing the movement detection event
  camera_ts <- data.frame(timestamp_ms = rep(starts_camera, each = 2)) %>%
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      sensor_id = "Camera",
      total_pixels_motionTrigger = 60000, 
      pixel_threshold = 100, 
      video_file_name = paste(paste(rep(paste("Box_01_2023_8_1", paste(hour(starts_camera), minute(starts_camera), second(starts_camera), sep = "_"), sep = "_"), 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = ""),
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(rfid_ts, file.path(tmp_path, "processed", "pre_processed_data_RFID.csv"), row.names = FALSE)
  write.csv(irbb_ts, file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv"), row.names = FALSE)
  write.csv(camera_ts, file.path(tmp_path, "processed", "pre_processed_data_Video.csv"), row.names = FALSE)
  
  # Note that the run length needs to be set to 1 in order to correctly detect detection clusters of length 2
  find_detectionClusters(file_nms = c("pre_processed_data_RFID.csv", "pre_processed_data_IRBB.csv", "pre_processed_data_Video.csv"), threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = "Camera", drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "detection_clusters.csv"))
  
  # Test that the results are 4 detection clusters, or the length of the start timestamps for 1 sensor created above
  expect_equal(nrow(test_res), length(starts_rfid))
  
  # Test that the sequence of sensor triggering events is correct
  test_seq <- paste(c(
    rep("RFID", length(c(starts_rfid[1], ends_rfid[1]))),
    rep("Outer Beam Breaker", length(c(starts_irbb_o[1], ends_irbb_o[1]))),
    rep("Inner Beam Breaker", length(c(starts_irbb_i[1], ends_irbb_i[1]))),
    rep("Camera", length(c(starts_camera[1])))
  ), collapse = "; ")
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$event_seq[i], test_seq)
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  test_detectns <- length(c(starts_rfid[1], ends_rfid[1]))
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], test_detectns)
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function detects the expected number of clusters using RFID data across different thresholds and run_lengths", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("pbapply")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(pbapply)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  ths <- seq(0.5, 5, by = 0.5)
  
  # Since run lengths are calculated from the difference in timestamps, each run_length value should be applied to a clusters of detections of length rls + 1
  rls <- seq(1, 10, by = 1)
  
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  invisible(mapply(function(x, y){
    
    tmp <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], starts[i] + max(seq_len(y*y)), by = x)[1:(y + 1)]
      # sim_ts
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    # Write out a spreadsheet with these timestamps that will be used as input data for the function
    sim_ts <- data.frame(timestamp_ms = tmp$tstmps) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        sensor_id = "RFID",
        PIT_tag_ID = "test",
        thin_threshold_s = 1,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      )
    
    tmp_nm <- paste(paste("simulated_single_sensor", paste("th", x, sep = ""), paste("rl", y, sep = ""), sep = "_"), ".csv", sep = "")
    
    write.csv(sim_ts, file.path(tmp_path, "processed", tmp_nm), row.names = FALSE)
    
    find_detectionClusters(file_nms = tmp_nm, threshold = x, run_length = y, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "detection_clusters.csv"))
    
    # Test that the number of detection clusters in the results is the same as the length of the start timestamps created above
    expect_equal(nrow(test_res), length(starts))
    
    # Test that the sequence of sensor triggering events is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      test_seq <- paste(c(
        rep("RFID", length(tmp$tstmps[tmp$cluster == i]))
      ), collapse = "; ")
      
      expect_equal(test_res$event_seq[i], test_seq)
      
    }))
    
    # Test that the results have the correct number of detections per individual (since RFID data was used as input)
    invisible(lapply(1:nrow(test_res), function(i){
      
      expect_equal(test_res$total_indiv1_detections[i], length(tmp$tstmps[tmp$cluster == i]))
      
    }))      
    
  }, ths, rls, SIMPLIFY = FALSE))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function detects the expected number of clusters using data from RFID and 2 pairs of beam breakers across different thresholds and run_lengths", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("pbapply")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(pbapply)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for two sensor types (RFID and 2 pairs of beam breakers)
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  ths <- seq(0.5, 5, by = 0.5)
  
  # Since run lengths are calculated from the difference in timestamps, each run_length value should be applied to a clusters of detections of length rls + 1
  rls <- seq(1, 10, by = 1)
  
  starts_rfid <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  # threshold values are x, run length values are y        
  invisible(mapply(function(x, y){
    
    # Create the RFID timestamps
    tmp_rfid <- data.table::rbindlist(lapply(1:length(starts_rfid), function(i){
      
      sim_ts <- seq(starts_rfid[i], starts_rfid[i] + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    # Create the outer beam breaker timestamps. For each simulated detection cluster, the first timestamp for the outer beam breaker pair is offset by x seconds more than the last RFID timestamp
    tmp_irbb_o <- data.table::rbindlist(lapply(1:length(starts_rfid), function(i){
      
      start_irbb_o <- max(tmp_rfid$tstmps[tmp_rfid$cluster == i]) + x
      
      sim_ts <- seq(start_irbb_o, start_irbb_o + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))

    # Create the inner beam breaker timestamps. For each simulated detection cluster, the first timestamp for the outer beam breaker pair is offset by x seconds more than the last timestamp of the cluster for the outer beam breaker pair
    tmp_irbb_i <- data.table::rbindlist(lapply(1:length(starts_rfid), function(i){
      
      start_irbb_i <- max(tmp_irbb_o$tstmps[tmp_irbb_o$cluster == i]) + x
      
      sim_ts <- seq(start_irbb_i, start_irbb_i + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    # Create separate spreadsheets for the simulated RFID and IRBB data
    rfid_ts <- data.frame(timestamp_ms = tmp_rfid$tstmps) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        sensor_id = "RFID",
        PIT_tag_ID = "test",
        thin_threshold_s = 1,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      )
    
    irbb_ts <- data.frame(timestamp_ms = c(tmp_irbb_o$tstmps, tmp_irbb_i$tstmps)) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        sensor_id = c(rep("Outer Beam Breaker", nrow(tmp_irbb_o)), rep("Inner Beam Breaker", nrow(tmp_irbb_i))),
        thin_threshold_s = 1,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      )
    
    tmp_nm_rfid <- paste(paste("simulated_RFID", paste("th", x, sep = ""), paste("rl", y, sep = ""), sep = "_"), ".csv", sep = "")
    
    tmp_nm_irbb <- paste(paste("simulated_IRBB", paste("th", x, sep = ""), paste("rl", y, sep = ""), sep = "_"), ".csv", sep = "")
    
    write.csv(rfid_ts, file.path(tmp_path, "processed", tmp_nm_rfid), row.names = FALSE)
    write.csv(irbb_ts, file.path(tmp_path, "processed", tmp_nm_irbb), row.names = FALSE)
    
    find_detectionClusters(file_nms = c(tmp_nm_rfid, tmp_nm_irbb), threshold = x, run_length = y, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "detection_clusters.csv"))

    # Test that the number of detection clusters in the results is the same as the length of the RFID start timestamps created above
    expect_equal(nrow(test_res), length(starts_rfid))
    
    # Test that the sequence of sensor triggering events is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      test_seq <- paste(c(
        rep("RFID", length(tmp_rfid$tstmps[tmp_rfid$cluster == i])),
        rep("Outer Beam Breaker", length(tmp_irbb_o$tstmps[tmp_irbb_o$cluster == i])),
        rep("Inner Beam Breaker", length(tmp_irbb_i$tstmps[tmp_irbb_i$cluster == i]))
      ), collapse = "; ")
      
      expect_equal(test_res$event_seq[i], test_seq)
      
    }))
    
    # Test that the results have the correct number of detections per individual (since RFID data was used as input)
    invisible(lapply(1:nrow(test_res), function(i){
      
      expect_equal(test_res$total_indiv1_detections[i], length(tmp_rfid$tstmps[tmp_rfid$cluster == i]))
      
    }))      
    
  }, ths, rls, SIMPLIFY = FALSE))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function detects the expected number of clusters using data from RFID, 2 beam breaker pairs, and the camera across different thresholds and run_lengths", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("pbapply")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(pbapply)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for three sensor types (RFID, 2 pairs of beam breakers, 1 camera)
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  ths <- seq(0.5, 5, by = 0.5)
  
  # Since run lengths are calculated from the difference in timestamps, each run_length value should be applied to a clusters of detections of length rls + 1
  rls <- seq(1, 10, by = 1)
  
  starts_rfid <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  invisible(mapply(function(x, y){
    
    # Create the RFID timestamps
    tmp_rfid <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts_rfid[i], starts_rfid[i] + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    # Create the outer beam breaker timestamps. For each simulated detection cluster, the first timestamp for the outer beam breaker pair is offset by x seconds more than the last RFID timestamp
    tmp_irbb_o <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      start_irbb_o <- max(tmp_rfid$tstmps[tmp_rfid$cluster == i]) + x
      
      sim_ts <- seq(start_irbb_o, start_irbb_o + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    # Create the inner beam breaker timestamps. For each simulated detection cluster, the first timestamp for the outer beam breaker pair is offset by x seconds more than the last timestamp of the cluster for the outer beam breaker pair
    tmp_irbb_i <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      start_irbb_i <- max(tmp_irbb_o$tstmps[tmp_irbb_o$cluster == i]) + x
      
      sim_ts <- seq(start_irbb_i, start_irbb_i + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    # Create the camera timestamps. For each simulated detection cluster, the first timestamp for the camera is offset by x seconds more than the last timestamp of the cluster for the inner beam breaker pair
    tmp_camera <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      start_camera <- max(tmp_irbb_i$tstmps[tmp_irbb_i$cluster == i]) + x
      
      sim_ts <- seq(start_camera, start_camera + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    # Create separate spreadsheets for the simulated RFID and IRBB data
    rfid_ts <- data.frame(timestamp_ms = tmp_rfid$tstmps) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        sensor_id = "RFID",
        PIT_tag_ID = "test",
        thin_threshold_s = 1,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      )
    
    irbb_ts <- data.frame(timestamp_ms = c(tmp_irbb_o$tstmps, tmp_irbb_i$tstmps)) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        sensor_id = c(rep("Outer Beam Breaker", nrow(tmp_irbb_o)), rep("Inner Beam Breaker", nrow(tmp_irbb_i))),
        thin_threshold_s = 1,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      )
    
    
    # Each video recording timestamp is repeated twice to simulate our current setup of a pre-motion trigger and post-motion trigger video per timestamp representing the movement detection event
    camera_ts <- data.frame(timestamp_ms = rep(tmp_camera$tstmps, each = 2)) %>%
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        sensor_id = "Camera",
        total_pixels_motionTrigger = 60000, 
        pixel_threshold = 100, 
        video_file_name = paste(paste(rep(paste("Box_01_2023_8_1", paste(hour(tmp_camera$tstmps), minute(tmp_camera$tstmps), second(tmp_camera$tstmps), sep = "_"), sep = "_"), each = 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = ""),
        thin_threshold_s = 1,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      )
    
    tmp_nm_rfid <- paste(paste("simulated_RFID", paste("th", x, sep = ""), paste("rl", y, sep = ""), sep = "_"), ".csv", sep = "")
    
    tmp_nm_irbb <- paste(paste("simulated_IRBB", paste("th", x, sep = ""), paste("rl", y, sep = ""), sep = "_"), ".csv", sep = "")
    
    tmp_nm_camera <- paste(paste("simulated_camera", paste("th", x, sep = ""), paste("rl", y, sep = ""), sep = "_"), ".csv", sep = "")
    
    write.csv(rfid_ts, file.path(tmp_path, "processed", tmp_nm_rfid), row.names = FALSE)
    write.csv(irbb_ts, file.path(tmp_path, "processed", tmp_nm_irbb), row.names = FALSE)
    write.csv(camera_ts, file.path(tmp_path, "processed", tmp_nm_camera), row.names = FALSE)
    
    find_detectionClusters(file_nms = c(tmp_nm_rfid, tmp_nm_irbb, tmp_nm_camera), threshold = x, run_length = y, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = "Camera", drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "detection_clusters.csv"))
    
    # Test that the number of detection clusters in the results is the same as the length of the RFID start timestamps created above
    expect_equal(nrow(test_res), length(starts_rfid))
    
    # Test that the sequence of sensor triggering events is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      test_seq <- paste(c(
        rep("RFID", length(tmp_rfid$tstmps[tmp_rfid$cluster == i])),
        rep("Outer Beam Breaker", length(tmp_irbb_o$tstmps[tmp_irbb_o$cluster == i])),
        rep("Inner Beam Breaker", length(tmp_irbb_i$tstmps[tmp_irbb_i$cluster == i])),
        rep("Camera", length(tmp_camera$tstmps[tmp_camera$cluster == i]))
      ), collapse = "; ")
      
      expect_equal(test_res$event_seq[i], test_seq)
      
    }))
    
    # Test that the results have the correct number of detections per individual (since RFID data was used as input)
    invisible(lapply(1:nrow(test_res), function(i){
      
      expect_equal(test_res$total_indiv1_detections[i], length(tmp_rfid$tstmps[tmp_rfid$cluster == i]))
      
    }))      
    
  }, ths, rls, SIMPLIFY = FALSE))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})


########## Testing error handling ########## 

test_that("the function catches when the input file names do not contain the patterns 'RFID', 'IRBB', or 'Video'", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  
  # Create 4 clusters of detections: each cluster consists of 2 detections spaced 1 second apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 1
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = c(starts, ends)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "pre_processed_data_RFID.csv"), row.names = FALSE)
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv"), row.names = FALSE)
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "pre_processed_data_Video.csv"), row.names = FALSE)
  
  expect_error(
    find_detectionClusters(file_nms = "pre_processed_data_RFD.csv", threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The input file name does not contain the correct sensor suffix"
  )
  
  expect_error(
    find_detectionClusters(file_nms = c("pre_processed_data_RFID.csv", "pre_processed_data_irbb.csv", "pre_processed_data_Video.csv"), threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The input file name does not contain the correct sensor suffix"
  )
  
  expect_error(
    find_detectionClusters(file_nms = c("pre_processed_data_RFID.csv", "pre_processed_data_IRBB.csv", "pre_processed_data_NO.csv"), threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The input file name does not contain the correct sensor suffix"
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when non-NULL arguments are NULL", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  
  # Create 4 clusters of detections: each cluster consists of 2 detections spaced 1 second apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 1
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = c(starts, ends)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "pre_processed_data_RFID.csv"), row.names = FALSE)

  # General arguments that cannot ever be NULL:
  arg_nms <- c("file_nms", "threshold", "run_length", "sensor_id_col_nm", "timestamps_col_nm", "preproc_metadata_col_nms", "general_metadata_col_nms", "path", "data_dir", "out_dir", "out_file_nm", "tz", "POSIXct_format")
  
  args <- list(
    `file_nms` = "pre_processed_data_RFID.csv",
    `threshold` = 2,
    `run_length` = 1,
    `sensor_id_col_nm` = "sensor_id",
    `timestamps_col_nm` = "timestamp_ms",
    `preproc_metadata_col_nms` = c("thin_threshold_s", "data_stage", "date_pre_processed"),
    `general_metadata_col_nms` = c("chamber_id", "sensor_id"),
    `path` = path,
    `data_dir` = file.path(data_dir, "processed"),
    `out_dir` = file.path(data_dir, "processed"),
    `out_file_nm` = "detection_clusters.csv", 
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )

  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      find_detectionClusters(file_nms = args[["file_nms"]], threshold = args[["threshold"]], run_length = args[["run_length"]], sensor_id_col_nm = args[["sensor_id_col_nm"]], timestamps_col_nm = args[["timestamps_col_nm"]], PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_col_nms = args[["preproc_metadata_col_nms"]], general_metadata_col_nms = args[["general_metadata_col_nms"]], video_metadata_col_nms = NULL, path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], out_file_nm = args[["out_file_nm"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # Arguments that cannot be NULL depending on which sensor is specified:
  
  # When using RFID data, the following arguments cannot be NULL:
  arg_nms <- c("PIT_tag_col_nm", "rfid_label")
  
  args <- list(
    `PIT_tag_col_nm` = "PIT_tag_ID",
    `rfid_label` = "RFID"
  )

  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      find_detectionClusters(file_nms = "pre_processed_data_RFID.csv", threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = args[["rfid_label"]], camera_label = NULL, drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # When using Video data, the following arguments cannot be NULL:
  write.csv(sim_ts, file.path(tmp_path, "processed", "pre_processed_data_Video.csv"), row.names = FALSE)
  
  arg_nms <- c("camera_label", "video_metadata_col_nms")
  
  args <- list(
    `camera_label` = "Video",
    `video_metadata_col_nms` = c("total_pixels_motionTrigger", "pixel_threshold")
  )

  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      find_detectionClusters(file_nms = "pre_processed_data_Video.csv", threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = args[["camera_label"]], drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = args[["video_metadata_col_nms"]], path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # When using RFID and Video data, the following arguments cannot be NULL:
  arg_nms <- c("PIT_tag_col_nm", "rfid_label", "camera_label", "video_metadata_col_nms")
  
  args <- list(
    `PIT_tag_col_nm` = "PIT_tag_ID",
    `rfid_label` = "RFID",
    `camera_label` = "Video",
    `video_metadata_col_nms` = c("total_pixels_motionTrigger", "pixel_threshold")
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)

    expect_error(
      find_detectionClusters(file_nms = c("pre_processed_data_RFID.csv", "pre_processed_data_Video.csv"), threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = args[["rfid_label"]], camera_label = args[["camera_label"]], drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = args[["video_metadata_col_nms"]], path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})




test_that("the function catches when NULL arguments are non-NULL", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  
  # Create 4 clusters of detections: each cluster consists of 2 detections spaced 1 second apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 1
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = c(starts, ends)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "pre_processed_data_RFID.csv"), row.names = FALSE)
  
  # Arguments that should be NULL depending on which sensor datasets are specified:
  
  # When using RFID data, the following arguments should be NULL:
  arg_nms <- c("camera_label", "video_metadata_col_nms")
  
  args <- list(
    `camera_label` = NULL,
    `video_metadata_col_nms` = NULL
  )
  
  # i <- 2
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- "test"

    expect_error(
      find_detectionClusters(file_nms = "pre_processed_data_RFID.csv", threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = args[["camera_label"]], drop_tag = NULL, preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = args[["video_metadata_col_nms"]], path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a NULL value but the argument", arg_nms[i], "is not NULL", sep = " ")
    )
    
  }))
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "pre_processed_data_Video.csv"), row.names = FALSE)
  
  # When using Video data, the following arguments should be NULL:
  arg_nms <- c("PIT_tag_col_nm", "rfid_label", "drop_tag")
  
  args <- list(
    `PIT_tag_col_nm` = NULL,
    `rfid_label` = NULL,
    `drop_tag` = NULL
  )
  
  # i <- 2
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- "test"
    
    expect_error(
      find_detectionClusters(file_nms = "pre_processed_data_RFID.csv", threshold = 2, run_length = 1, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = args[["camera_label"]], camera_label = "Video", drop_tag = args[["drop_tag"]], preproc_metadata_col_nms = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_col_nms = c("chamber_id", "year", "month", "day"), video_metadata_col_nms = args[["video_metadata_col_nms"]], path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a NULL value but the argument", arg_nms[i], "is not NULL", sep = " ")
    )
    
  }))
  
  # Checks when sensor = Video
  
  # Repeat timestamps (1 pre- and 1 post-motion video per timestamp)
  tstmps <- rep(starts, each = 2)
  # tstmps
  
  px <- 100
  
  #### 1/2 of the videos have total pixels less than the pixel threshold ####
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = tstmps) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "Camera",
      data_type = "Video",
      total_pixels_motionTrigger = rep(c(px * 10, px / 10), each = 4), 
      pixel_threshold = px, 
      video_file_name = paste(paste(rep(paste("Box_01_2023_1_1", paste(hour(starts), minute(starts), second(starts), sep = "_"), sep = "_"), 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = ""),
      data_stage = "processed",
      date_combined = Sys.Date()
    )
  
  
  
  # Arguments that should be NULL when sensor = Video:
  arg_nms <- c("group_col_nm", "thin_threshold", "mode")
  
  args <- list(
    `group_col_nm` = NULL,
    `thin_threshold` = NULL,
    `mode` = NULL
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- "test"
    
    expect_error(
      preprocess_detections(sensor = "Video", timestamps_col_nm_nm = "timestamps_col_nm_nm", group_col_nm = args[["group_col_nm"]], pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = args[["thin_threshold"]], mode = args[["mode"]], pixel_threshold = "pixel_threshold", path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a NULL value but the argument", arg_nms[i], "is not NULL", sep = " ")
    )
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})





test_that("the function catches when character string arguments are not strings", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  # Create 4 clusters of detections: each cluster consists of 21 detections spaced 0.5 seconds apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  event_ts <- sapply(1:length(starts), function(x){
    
    return(seq(starts[x], ends[x], 0.5))
    
  }, simplify = FALSE)
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = c(event_ts[[1]], event_ts[[2]], event_ts[[3]], event_ts[[4]])) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "RFID",
      data_type = "RFID",
      data_stage = "processed",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  # General arguments that should always be strings. Not including sensor here because this argument is checked in a separate test above
  arg_nms <- c("timestamps_col_nm_nm", "path", "data_dir", "out_dir", "tz", "POSIXct_format")
  
  args <- list(
    `timestamps_col_nm_nm` = "timestamp_ms",
    `path` = path,
    `data_dir` = file.path(data_dir, "processed"),
    `out_dir` = file.path(data_dir, "processed"),
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      preprocess_detections(sensor = "RFID", timestamps_col_nm_nm = args[["timestamps_col_nm_nm"]], group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )
    
  }))
  
  # Arguments that should be strings when sensor = RFID or IRBB:
  arg_nms <- c("group_col_nm", "mode")
  
  args <- list(
    `group_col_nm` = "PIT_tag_ID",
    `mode` = "thin"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      preprocess_detections(sensor = "RFID", timestamps_col_nm_nm = "timestamps_col_nm_nm", group_col_nm = args[["group_col_nm"]], pixel_col_nm = NULL, thin_threshold = th, mode = args[["mode"]], pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )
    
  }))
  
  # Checks when sensor = Video
  
  # Repeat timestamps (1 pre- and 1 post-motion video per timestamp)
  tstmps <- rep(starts, each = 2)
  # tstmps
  
  px <- 100
  
  #### 1/2 of the videos have total pixels less than the pixel threshold ####
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = tstmps) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "Camera",
      data_type = "Video",
      total_pixels_motionTrigger = rep(c(px * 10, px / 10), each = 4), 
      pixel_threshold = px, 
      video_file_name = paste(paste(rep(paste("Box_01_2023_1_1", paste(hour(starts), minute(starts), second(starts), sep = "_"), sep = "_"), 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = ""),
      data_stage = "processed",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  # Arguments that should be strings when sensor = Video:
  arg_nms <- c("pixel_col_nm")
  
  args <- list(
    `pixel_col_nm` = "total_pixels_motionTrigger"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      preprocess_detections(sensor = "Video", timestamps_col_nm_nm = "timestamps_col_nm_nm", group_col_nm = NULL, pixel_col_nm = args[["pixel_col_nm"]], thin_threshold = NULL, mode = NULL, pixel_threshold = px, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when numeric arguments are non-numeric", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  # Create 4 clusters of detections: each cluster consists of 21 detections spaced 0.5 seconds apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  event_ts <- sapply(1:length(starts), function(x){
    
    return(seq(starts[x], ends[x], 0.5))
    
  }, simplify = FALSE)
  
  ####### Testing thin_threshold #######
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = c(event_ts[[1]], event_ts[[2]], event_ts[[3]], event_ts[[4]])) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "RFID",
      data_type = "RFID",
      data_stage = "processed",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = as.character(th), mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "Expected a numeric value but the argument thin_threshold is not numeric"
  )
  
  ####### Testing pixel_threshold #######
  
  # Repeat these timestamps (1 pre- and 1 post-motion video per timestamp)
  tstmps <- rep(starts, each = 2)
  # tstmps
  
  px <- 100
  
  #### 1/2 of the videos have total pixels less than the pixel threshold ####
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = tstmps) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "Camera",
      data_type = "Video",
      total_pixels_motionTrigger = rep(c(px * 10, px / 10), each = 4), 
      pixel_threshold = px, 
      video_file_name = paste(paste(rep(paste("Box_01_2023_1_1", paste(hour(starts), minute(starts), second(starts), sep = "_"), sep = "_"), 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = ""),
      data_stage = "processed",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "Video", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = as.character(px), path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("Expected a numeric value but the argument pixel_threshold is not numeric", sep = " ")
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when paths don't exist", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  # Create 4 clusters of detections: each cluster consists of 21 detections spaced 0.5 seconds apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  event_ts <- sapply(1:length(starts), function(x){
    
    return(seq(starts[x], ends[x], 0.5))
    
  }, simplify = FALSE)
  
  
  th <- 1
  
  sim_ts <- data.frame(timestamp_ms = c(event_ts[[1]], event_ts[[2]], event_ts[[3]], event_ts[[4]])) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "RFID",
      data_type = "RFID",
      data_stage = "processed",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    ) 
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  # Remove this file
  file.remove(file.path(tmp_path, "processed", "combined_raw_data_RFID.csv"))
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The file combined_raw_data_RFID.csv does not exist in the directory", file.path(tmp_path, "processed"), sep = " ")
  )
  
  # Generate the file again, then remove the whole directory
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  # Remove the directory where this file is located
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The directory", file.path(path, data_dir, "processed"), "does not exist", sep = " ")
  )
  
})

test_that("the input dataset has all of the expected columns", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  # Create 4 clusters of detections: each cluster consists of 21 detections spaced 0.5 seconds apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  event_ts <- sapply(1:length(starts), function(x){
    
    return(seq(starts[x], ends[x], 0.5))
    
  }, simplify = FALSE)
  
  
  th <- 1
  
  # Columns that must always be present in the raw data. All of these columns are used to make the timestamp_ms column
  col_nms <- c("original_timestamp", "year", "month", "day")
  
  invisible(lapply(1:length(col_nms), function(i){
    
    # Write out a spreadsheet with these timestamps that will be used as input data for the function
    sim_ts <- data.frame(timestamp_ms = c(event_ts[[1]], event_ts[[2]], event_ts[[3]], event_ts[[4]])) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
        sensor_id = "RFID",
        data_type = "RFID",
        data_stage = "processed",
        date_combined = Sys.Date(),
        PIT_tag_ID = "test"
      ) %>% 
      # Drop the given column
      dplyr::select(-c(all_of(col_nms[i])))
    
    write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_RFID.csv"), row.names = FALSE)
    
    expect_error(
      preprocess_detections(sensor = "RFID", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("The column", col_nms[i], "was not found in the data frame", sep = " ")
    )
    
  }))
  
  # When sensor = RFID or IRBB, the column in group_col_nm must be present
  sim_ts <- data.frame(timestamp_ms = c(event_ts[[1]], event_ts[[2]], event_ts[[3]], event_ts[[4]])) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "RFID",
      data_type = "RFID",
      data_stage = "processed",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    ) %>% 
    # Drop the PIT_tag_ID column
    dplyr::select(-c(PIT_tag_ID))
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The column PIT_tag_ID was not found in the data frame")
  )
  
  # When sensor = Video, the column in pixel_col_nm must be present
  # Repeat these timestamps (1 pre- and 1 post-motion video per timestamp)
  tstmps <- rep(starts, each = 2)
  px <- 100
  
  sim_ts <- data.frame(timestamp_ms = tstmps) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "Camera",
      data_type = "Video",
      total_pixels_motionTrigger = rep(c(px * 10, px / 10), each = 4), 
      pixel_threshold = px, 
      video_file_name = paste(paste(rep(paste("Box_01_2023_1_1", paste(hour(starts), minute(starts), second(starts), sep = "_"), sep = "_"), 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = ""),
      data_stage = "processed",
      date_combined = Sys.Date()
    ) %>% 
    # Drop the pixels column
    dplyr::select(-c(total_pixels_motionTrigger))
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "Video", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = px, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The column total_pixels_motionTrigger was not found in the data frame")
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the input dataset has no NAs in columns that cannot have NAs", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "processed"))){ 
    dir.create(file.path(tmp_path, "processed"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  # Create 4 clusters of detections: each cluster consists of 21 detections spaced 0.5 seconds apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  event_ts <- sapply(1:length(starts), function(x){
    
    return(seq(starts[x], ends[x], 0.5))
    
  }, simplify = FALSE)
  
  
  th <- 1
  
  # Columns in the raw data that cannot ever have NA values (these columns are used to create timestamps used for lag calculations across functions):
  col_nms <- c("original_timestamp", "year", "month", "day")
  
  invisible(lapply(1:length(col_nms), function(i){
    
    # Write out a spreadsheet with these timestamps that will be used as input data for the function
    sim_ts <- data.frame(timestamp_ms = c(event_ts[[1]], event_ts[[2]], event_ts[[3]], event_ts[[4]])) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
        sensor_id = "RFID",
        data_type = "RFID",
        data_stage = "processed",
        date_combined = Sys.Date(),
        PIT_tag_ID = "test"
      )
    
    sim_ts[[col_nms[i]]][1] <- NA
    
    write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_RFID.csv"), row.names = FALSE)
    
    expect_error(
      preprocess_detections(sensor = "RFID", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("The column", col_nms[i], "has NA values", sep = " ")
    )
    
  }))
  
  # When sensor = RFID or IRBB, the column in group_col_nm should not have NAs
  sim_ts <- data.frame(timestamp_ms = c(event_ts[[1]], event_ts[[2]], event_ts[[3]], event_ts[[4]])) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "RFID",
      data_type = "RFID",
      data_stage = "processed",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  sim_ts[["PIT_tag_ID"]][1] <- NA
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The column PIT_tag_ID has NA values")
  )
  
  # When sensor = Video, the column in pixel_col_nm must be present
  # Repeat these timestamps (1 pre- and 1 post-motion video per timestamp)
  tstmps <- rep(starts, each = 2)
  px <- 100
  
  sim_ts <- data.frame(timestamp_ms = tstmps) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = "Camera",
      data_type = "Video",
      total_pixels_motionTrigger = rep(c(px * 10, px / 10), each = 4), 
      pixel_threshold = px, 
      video_file_name = paste(paste(rep(paste("Box_01_2023_1_1", paste(hour(starts), minute(starts), second(starts), sep = "_"), sep = "_"), 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = ""),
      data_stage = "processed",
      date_combined = Sys.Date()
    )
  
  sim_ts[["total_pixels_motionTrigger"]][1] <- NA
  
  write.csv(sim_ts, file.path(tmp_path, "processed", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "Video", timestamps_col_nm_nm = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = px, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The column total_pixels_motionTrigger has NA values")
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

