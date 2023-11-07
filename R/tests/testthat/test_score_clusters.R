# G. Smith-Vidaurre
# 03 January 2023

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/score_clusters.R")

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

# Another test that would be good to add is whether multiple video file names are concatenated if/when detection clusters have more than 1 camera event

test_that("The function labels entrances and exits as expected using data from 2 beam breaker pairs", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID and 2 beam breaker pairs", {
  
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
  
  # Generate a file with pre-processed timestamps for two sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID and 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)

  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), rep("Inner Beam Breaker", 1)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c(rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
    
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], sim_dats$total_indiv1_detections[i])
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera", {
  
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
  # library(dplyr)

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
  
  # Generate a file with pre-processed timestamps for three sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID, 2 pairs of beam breakers, and a camera. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2), "Camera"), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), "Inner Beam Breaker", "Camera"), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2), "Camera"), collapse = "; "), # entrance
        paste(c("Camera", rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Camera", "Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c("Camera", rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      total_pixels_motionTrigger = 60000, 
      pixel_threshold = 100, 
      video_file_name = paste(paste(paste("Box_01_2023_8_1", paste(hour(start), minute(start), second(start), sep = "_"), sep = "_"), "pre_trigger", sep = "_"), ".mp4", sep = ""),
      
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )

  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], sim_dats$total_indiv1_detections[i])
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera and integrating pre-processed video data", {
  
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
  # library(dplyr)
  
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
  
  # Generate a file with pre-processed timestamps for three sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID, 2 pairs of beam breakers, and a camera. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2), "Camera"), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), "Inner Beam Breaker", "Camera"), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2), "Camera"), collapse = "; "), # entrance
        paste(c("Camera", rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Camera", "Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c("Camera", rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      total_pixels_motionTrigger = 60000, 
      pixel_threshold = 100, 
      video_file_name = paste(paste(paste("Box_01_2023_8_1", paste(hour(start), minute(start), second(start), sep = "_"), sep = "_"), "pre_trigger", sep = "_"), ".mp4", sep = ""),
      
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Generate a file with pre-processed video timestamps 

  # Create 6 video recording detections, two of which will map onto detection clusters, and 4 of which will not (singlet video recording events)
  starts_camera <- c(
    starts[c(1:2)] + 2, 
    as.POSIXct(c(
      "2023-01-01 05:00:00 EST"
    )) + seq(1, 200, 50)
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
  
  write.csv(camera_ts, file.path(tmp_path, "processed", "pre_processed_data_Video.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = TRUE, video_file_nm = "pre_processed_data_Video.csv", timestamps_col_nm = "timestamp_ms", path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 6 inferred container entrance movements and 4 inferred inside container movements, in that order
  expect_equal(test_res$inferredMovement_Location, c(rep("container_entrance", 6), rep("inside_container", 4)))
  
  # Test that the number of detection clusters in the input and output data are the same as expected
  expect_equal(nrow(sim_dats) + (nrow(camera_ts)/2 - 2), nrow(test_res))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from 2 beam breaker pairs and integrating RFID perching data", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)

  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )

  write.csv(sim_perch, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = "RFID", perching_prefix = "perching_events_", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){

    if(test_res$start[i] %in% starts_p){
      expect_true(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    } else {
      expect_false(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID and 2 beam breaker pairs and integrating RFID perching data", {
  
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
  
  # Generate a file with pre-processed timestamps for two sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID and 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), rep("Inner Beam Breaker", 1)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c(rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = "RFID", perching_prefix = "perching_events_", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], sim_dats$total_indiv1_detections[i])
  }))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      expect_true(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    } else {
      expect_false(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera and integrating RFID perching data", {
  
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
  
  # Generate a file with pre-processed timestamps for three sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID, 2 pairs of beam breakers, and a camera. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2), "Camera"), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), "Inner Beam Breaker", "Camera"), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2), "Camera"), collapse = "; "), # entrance
        paste(c("Camera", rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Camera", "Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c("Camera", rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      total_pixels_motionTrigger = 60000, 
      pixel_threshold = 100, 
      video_file_name = paste(paste(paste("Box_01_2023_8_1", paste(hour(start), minute(start), second(start), sep = "_"), sep = "_"), "pre_trigger", sep = "_"), ".mp4", sep = ""),
      
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = TRUE, perching_dataset = "RFID", perching_prefix = "perching_events_", pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], sim_dats$total_indiv1_detections[i])
  }))

  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      expect_true(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    } else {
      expect_false(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera and integrating pre-processed video data as well as RFID perching data", {
  
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
  # library(dplyr)
  
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
  
  # Generate a file with pre-processed timestamps for three sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID, 2 pairs of beam breakers, and a camera. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2), "Camera"), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), "Inner Beam Breaker", "Camera"), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2), "Camera"), collapse = "; "), # entrance
        paste(c("Camera", rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Camera", "Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c("Camera", rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      total_pixels_motionTrigger = 60000, 
      pixel_threshold = 100, 
      video_file_name = paste(paste(paste("Box_01_2023_8_1", paste(hour(start), minute(start), second(start), sep = "_"), sep = "_"), "pre_trigger", sep = "_"), ".mp4", sep = ""),
      
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
  
  # Generate a file with pre-processed video timestamps 
  
  # Create 6 video recording detections, two of which will map onto detection clusters, and 4 of which will not (singlet video recording events)
  starts_camera <- c(
    starts[c(1:2)] + 2, 
    as.POSIXct(c(
      "2023-01-01 05:00:00 EST"
    )) + seq(1, 200, 50)
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
  
  write.csv(camera_ts, file.path(tmp_path, "processed", "pre_processed_data_Video.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = TRUE, perching_dataset = "RFID", perching_prefix = "perching_events_", pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = TRUE, video_file_nm = "pre_processed_data_Video.csv", timestamps_col_nm = "timestamp_ms", path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 6 inferred container entrance movements and 4 inferred inside container movements, in that order
  expect_equal(test_res$inferredMovement_Location, c(rep("container_entrance", 6), rep("inside_container", 4)))
  
  # Test that the number of detection clusters in the input and output data are the same as expected
  expect_equal(nrow(sim_dats) + (nrow(camera_ts)/2 - 2), nrow(test_res))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      expect_true(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    } else {
      expect_false(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from 2 beam breaker pairs and integrating IRBB perching data", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "IRBB",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch, file.path(tmp_path, "processed", "perching_events_IRBB.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = "IRBB", perching_prefix = "perching_events_", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      expect_true(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    } else {
      expect_false(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID and 2 beam breaker pairs and integrating IRBB perching data", {
  
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
  
  # Generate a file with pre-processed timestamps for two sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID and 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), rep("Inner Beam Breaker", 1)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c(rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "IRBB",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch, file.path(tmp_path, "processed", "perching_events_IRBB.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = "IRBB", perching_prefix = "perching_events_", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], sim_dats$total_indiv1_detections[i])
  }))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      expect_true(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    } else {
      expect_false(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera and integrating IRBB perching data", {
  
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
  
  # Generate a file with pre-processed timestamps for three sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID, 2 pairs of beam breakers, and a camera. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2), "Camera"), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), "Inner Beam Breaker", "Camera"), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2), "Camera"), collapse = "; "), # entrance
        paste(c("Camera", rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Camera", "Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c("Camera", rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      total_pixels_motionTrigger = 60000, 
      pixel_threshold = 100, 
      video_file_name = paste(paste(paste("Box_01_2023_8_1", paste(hour(start), minute(start), second(start), sep = "_"), sep = "_"), "pre_trigger", sep = "_"), ".mp4", sep = ""),
      
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "IRBB",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch, file.path(tmp_path, "processed", "perching_events_IRBB.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = TRUE, perching_dataset = "IRBB", perching_prefix = "perching_events_", pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], sim_dats$total_indiv1_detections[i])
  }))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      expect_true(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    } else {
      expect_false(all(!sapply(test_res[i, grep("perch", names(test_res))], is.na)))
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from 2 beam breaker pairs and integrating RFID and IRBB perching data", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch_rfid <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  sim_perch_irbb <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "IRBB",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch_rfid, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
  
  write.csv(sim_perch_irbb, file.path(tmp_path, "processed", "perching_events_IRBB.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = "RFID-IRBB", perching_prefix = "perching_events_", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      
      expect_true(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_([a-z]+)_irbb", names(test_res))], is.na)))
      
    } else {

      expect_false(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_([a-z]+)_irbb", names(test_res))], is.na)))
      
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID and 2 beam breaker pairs and integrating RFID and IRBB perching data", {
  
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
  
  # Generate a file with pre-processed timestamps for two sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID and 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), rep("Inner Beam Breaker", 1)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c(rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch_rfid <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  sim_perch_irbb <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "IRBB",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch_rfid, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
  
  write.csv(sim_perch_irbb, file.path(tmp_path, "processed", "perching_events_IRBB.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = "RFID-IRBB", perching_prefix = "perching_events_", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], sim_dats$total_indiv1_detections[i])
  }))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      
      expect_true(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_([a-z]+)_irbb", names(test_res))], is.na)))
      
    } else {
      
      expect_false(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_([a-z]+)_irbb", names(test_res))], is.na)))
      
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera and integrating RFID and IRBB perching data", {
  
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
  
  # Generate a file with pre-processed timestamps for three sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID, 2 pairs of beam breakers, and a camera. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2), "Camera"), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), "Inner Beam Breaker", "Camera"), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2), "Camera"), collapse = "; "), # entrance
        paste(c("Camera", rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Camera", "Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c("Camera", rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      total_pixels_motionTrigger = 60000, 
      pixel_threshold = 100, 
      video_file_name = paste(paste(paste("Box_01_2023_8_1", paste(hour(start), minute(start), second(start), sep = "_"), sep = "_"), "pre_trigger", sep = "_"), ".mp4", sep = ""),
      
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch_rfid <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  sim_perch_irbb <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "IRBB",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch_rfid, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
  
  write.csv(sim_perch_irbb, file.path(tmp_path, "processed", "perching_events_IRBB.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = TRUE, perching_dataset = "RFID-IRBB", perching_prefix = "perching_events_", pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  expect_equal(test_res$direction_scored, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the results have the correct number of detections per individual (since RFID data was used as input)
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$total_indiv1_detections[i], sim_dats$total_indiv1_detections[i])
  }))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      
      expect_true(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_([a-z]+)_irbb", names(test_res))], is.na)))
      
    } else {
      
      expect_false(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_([a-z]+)_irbb", names(test_res))], is.na)))
      
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera and integrating pre-processed video data as well as RFID and IRBB perching data", {
  
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
  # library(dplyr)
  
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
  
  # Generate a file with pre-processed timestamps for three sensor types
  
  # Create 3 entrance and 3 exit events using different combinations of the RFID, 2 pairs of beam breakers, and a camera. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2), "Camera"), collapse = "; "), # entrance
        paste(c(rep("RFID", 2), "Inner Beam Breaker", "Camera"), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 3), rep("RFID", 2), "Camera"), collapse = "; "), # entrance
        paste(c("Camera", rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Camera", "Inner Beam Breaker", 5), rep("RFID", 2)), collapse = "; "), # exit
        paste(c("Camera", rep("RFID", 2), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      total_pixels_motionTrigger = 60000, 
      pixel_threshold = 100, 
      video_file_name = paste(paste(paste("Box_01_2023_8_1", paste(hour(start), minute(start), second(start), sep = "_"), sep = "_"), "pre_trigger", sep = "_"), ".mp4", sep = ""),
      
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed", "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  sim_perch_rfid <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "RFID",
      PIT_tag_ID = "test",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  sim_perch_irbb <- data.frame(perching_start = starts_p) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      sensor_id = "IRBB",
      perching_end = ends_p,
      perching_duration_s = perching_end - perching_start,
      unique_perching_event = seq(1, nrow(.), 1),
      threshold = 1,
      min_perching_run_length = 3,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_perch_rfid, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
  
  write.csv(sim_perch_irbb, file.path(tmp_path, "processed", "perching_events_IRBB.csv"), row.names = FALSE)
  
  # Generate a file with pre-processed video timestamps 
  
  # Create 6 video recording detections, two of which will map onto detection clusters, and 4 of which will not (singlet video recording events)
  starts_camera <- c(
    starts[c(1:2)] + 2, 
    as.POSIXct(c(
      "2023-01-01 05:00:00 EST"
    )) + seq(1, 200, 50)
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
  
  write.csv(camera_ts, file.path(tmp_path, "processed", "pre_processed_data_Video.csv"), row.names = FALSE)
  
  score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = TRUE, perching_dataset = "RFID-IRBB", perching_prefix = "perching_events_", pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = TRUE, video_file_nm = "pre_processed_data_Video.csv", timestamps_col_nm = "timestamp_ms", path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "scored_detectionClusters.csv"))
  
  # Test that the results are 6 inferred container entrance movements and 4 inferred inside container movements, in that order
  expect_equal(test_res$inferredMovement_Location, c(rep("container_entrance", 6), rep("inside_container", 4)))
  
  # Test that the number of detection clusters in the input and output data are the same as expected
  expect_equal(nrow(sim_dats) + (nrow(camera_ts)/2 - 2), nrow(test_res))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      
      expect_true(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_([a-z]+)_irbb", names(test_res))], is.na)))
      
    } else {
      
      expect_false(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_([a-z]+)_irbb", names(test_res))], is.na)))
      
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})


########## Testing error handling ########## 

test_that("the function catches when the perching_dataset argument does not contain the patterns 'RFID', 'IRBB', or 'RFID-IRBB'", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = "RFD", perching_prefix = "perching_events", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The perching dataset is not specified correctly"
  )
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = "RFID-irbb", perching_prefix = "perching_events", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The perching dataset is not specified correctly"
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  # General arguments that cannot ever be NULL:
  arg_nms <- c("file_nm", "integrate_perching", "path", "data_dir", "out_dir", "out_file_nm", "tz", "POSIXct_format")
  
  args <- list(
    `file_nm` = "detection_clusters.csv",
    `integrate_perching` = FALSE,
    `path` = path,
    `data_dir` = file.path(data_dir, "processed"),
    `out_dir` = file.path(data_dir, "processed"),
    `out_file_nm` = "scored_detection_clusters.csv", 
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )

  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      score_clusters(file_nm = args[["file_nm"]], sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = args[["integrate_perching"]], perching_dataset = NULL, perching_prefix = NULL, integrate_preproc_video = FALSE, path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], out_file_nm = args[["out_file_nm"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # Arguments that cannot be NULL when perching events are integrated:
  arg_nms <- c("sensor_id_col_nm", "PIT_tag_col_nm", "perching_dataset", "perching_prefix")
  
  args <- list(
    `sensor_id_col_nm` = "sensor_id",
    `PIT_tag_col_nm` = "PIT_tag_ID",
    `perching_dataset` = "RFID",
    `perching_prefix` = "perching_events_"
  )

  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = args[["sensor_id_col_nm"]], PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = args[["perching_dataset"]], perching_prefix = args[["perching_prefix"]], integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # Arguments that cannot be NULL when pre-processed video recordings are integrated:
  arg_nms <- c("video_file_nm", "timestamps_col_nm")
  
  args <- list(
    `video_file_nm` = "test.csv",
    `timestamps_col` = "timestamp_ms"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("total_pixels_motionTrigger"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = TRUE, video_file_nm = args[["video_file_nm"]], timestamps_col_nm = args[["timestamps_col_nm"]], path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # When camera_label is not NULL, video_metadata_col_nms and columns for video-related calculation also cannot be NULL
  arg_nms <- c("video_metadata_col_nms", "pixel_col_nm", "video_width", "video_height")
  
  args <- list(
    `video_metadata_col_nms` = c("total_pixels_motionTrigger"),
    `pixel_col_nm` = c("total_pixels_motionTrigger"),
    `video_width` = 1280,
    `video_height` = 720
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = args[["video_metadata_col_nms"]], integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = args[["pixel_col_nm"]], video_width = args[["video_width"]], video_height = args[["video_height"]], integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # Finally, check that the sensor label arguments are specified correctly given the combinations expected by the function
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "Data from at least two sensor types, or two beam breaker pairs, must be specified"
  )
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = NULL, outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The RFID data must be accompanied by beam breaker and/or video data"
  )
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The video data must be accompanied by beam breaker and/or RFID data"
  )
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = NULL, video_metadata_col_nms = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The outer beam breaker data must be accompanied by inner beam breaker data"
  )
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = NULL, inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The inner beam breaker data must be accompanied by outer beam breaker data"
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when NULL arguments are non-NULL", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  # Arguments that should be NULL? When integrate perching is FALSE, then the 4 perching related columns should be NULL
  arg_nms <- c("sensor_id_col_nm", "PIT_tag_col_nm", "perching_dataset", "perching_prefix")
  
  args <- list(
    `sensor_id_col_nm` = NULL,
    `PIT_tag_col_nm` = NULL,
    `perching_dataset` = NULL,
    `perching_prefix` = NULL
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- "test"
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = args[["sensor_id_col_nm"]], PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = FALSE, perching_dataset = args[["perching_dataset"]], perching_prefix = args[["perching_prefix"]], pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  # General arguments that must always be strings:
  arg_nms <- c("file_nm", "path", "data_dir", "out_dir", "out_file_nm", "tz", "POSIXct_format")
  
  args <- list(
    `file_nm` = "detection_clusters.csv",
    `path` = path,
    `data_dir` = file.path(data_dir, "processed"),
    `out_dir` = file.path(data_dir, "processed"),
    `out_file_nm` = "scored_detection_clusters.csv", 
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      score_clusters(file_nm = args[["file_nm"]], sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], out_file_nm = args[["out_file_nm"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )
    
  }))
  
  # Arguments that must be strings when perching events are integrated (the perching_dataset is caught in a different test):
  arg_nms <- c("sensor_id_col_nm", "PIT_tag_col_nm", "perching_prefix")
  
  args <- list(
    `sensor_id_col_nm` = "sensor_id",
    `PIT_tag_col_nm` = "PIT_tag_ID",
    `perching_prefix` = "perching_events_"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = args[["sensor_id_col_nm"]], PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = NULL, integrate_perching = TRUE, perching_dataset = "RFID", perching_prefix = args[["perching_prefix"]], pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )
    
  }))
  
  # Arguments that must be strings when pre-processed video recordings are integrated:
  arg_nms <- c("video_file_nm", "timestamps_col_nm")
  
  args <- list(
    `video_file_nm` = "test.csv",
    `timestamps_col_nm` = "timestamp_ms"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("total_pixels_motionTrigger"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = TRUE, video_file_nm = args[["video_file_nm"]], timestamps_col_nm = args[["timestamps_col_nm"]], path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )
    
  }))
  
  # When camera_label is not NULL, video_metadata_col_nms and pixel_col_nm must be strings
  arg_nms <- c("video_metadata_col_nms", "pixel_col_nm")
  
  args <- list(
    `video_metadata_col_nms` = c("total_pixels_motionTrigger"),
    `pixel_col_nm` = c("total_pixels_motionTrigger")
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = args[["video_metadata_col_nms"]], pixel_col_nm = args[["pixel_col_nm"]], integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )
    
  }))
  
  # The sensor label arguments are caught in the non-NULL tests above
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when Boolean arguments are not Boolean", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = c("pixel_threshold"), integrate_perching = "test", perching_dataset = NULL, perching_prefix = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("Expected a Boolean value but the argument integrate_perching is not Boolean", sep = " ")
  )
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = c("pixel_threshold"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, integrate_preproc_video = "FALSE", path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("Expected a Boolean value but the argument integrate_preproc_video is not Boolean", sep = " ")
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when numeric arguments are not numeric", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  # When camera_label is not NULL, the video_width and height arguments must be numeric
  arg_nms <- c("video_width", "video_height")
  
  args <- list(
    `video_width` = 1280,
    `video_height` = 720
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- "test"
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = c("total_pixels_motionTrigger"), pixel_col_nm = "total_pixels_motionTrigger", integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, video_width = args[["video_width"]], video_height = args[["video_height"]], integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a numeric value but the argument", arg_nms[i], "is not numeric", sep = " ")
    )
    
  }))
  
  # The sensor label arguments are caught in the non-NULL tests above
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when paths don't exist", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Create a spreadsheet of the simulated directional movement events
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  # Remove this file
  file.remove(file.path(tmp_path, "processed", "detection_clusters.csv"))
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = c("pixel_threshold"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The file detection_clusters.csv does not exist in the directory", file.path(path, data_dir, "processed"), sep = " ")
  )
  
  # Generate the file again, then remove the whole directory
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  # Remove the directory where this file is located
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL,  video_metadata_col_nms = c("pixel_threshold"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The directory", file.path(path, data_dir, "processed"), "does not exist", sep = " ")
  )
  
})

test_that("the input dataset has all of the expected columns", {
  
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Columns that must always be present in the main input data
  col_nms <- c("start", "end")
  
  invisible(lapply(1:length(col_nms), function(i){
    
    # Create a spreadsheet of the simulated directional movement events
    sim_dats <- data.frame(start = starts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(start),
        month = month(start),
        day = day(start),
        end = ends,
        event_seq = c(
          paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
          paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
          paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
          paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
          paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
          paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
        ),
        indiv1_id = "test",
        indiv2_id = NA,
        # Matches the number of RFID detections if present above
        total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
        total_indiv2_detections = 0,
        individual_initiated = "test",
        individual_ended = "test",
        threshold_seconds = 1,
        run_length = 1,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      ) %>% 
      # Drop the given column
      dplyr::select(-c(all_of(col_nms[i])))
    
    write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = c("pixel_threshold"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("The column", col_nms[i], "was not found in the data frame", sep = " ")
    )
    
  }))
  
  # When integrate_perching = TRUE, I also need to check that the perching_end and perching_starts timestamps columns are present in each dataset. And also that the columns sensor_id_col_nm and PIT_tag_col_nm are present
  
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  # Columns that must always be present in the perching event data
  col_nms <- c("perching_start", "perching_end", "sensor_id", "PIT_tag_ID")
  
  # i <- 3
  invisible(lapply(1:length(col_nms), function(i){
    
    # Create a spreadsheet of the simulated directional movement events
    sim_perch_rfid <- data.frame(perching_start = starts_p) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        sensor_id = "RFID",
        PIT_tag_ID = "test",
        perching_end = ends_p,
        perching_duration_s = perching_end - perching_start,
        unique_perching_event = seq(1, nrow(.), 1),
        threshold = 1,
        min_perching_run_length = 3,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      )
    
    sim_perch_irbb <- data.frame(perching_start = starts_p) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        sensor_id = "IRBB",
        perching_end = ends_p,
        perching_duration_s = perching_end - perching_start,
        unique_perching_event = seq(1, nrow(.), 1),
        threshold = 1,
        min_perching_run_length = 3,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      ) 
    
    # Drop the given column if it exists
    if(col_nms[i] %in% names(sim_perch_rfid)){
      sim_perch_rfid <- sim_perch_rfid %>% 
        dplyr::select(-c(all_of(col_nms[i])))
    }
    
    # Drop the given column if it exists
    if(col_nms[i] %in% names(sim_perch_irbb)){
      sim_perch_irbb <- sim_perch_irbb %>% 
        dplyr::select(-c(all_of(col_nms[i])))
    }
    
    write.csv(sim_perch_rfid, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
    
    write.csv(sim_perch_irbb, file.path(tmp_path, "processed", "perching_events_IRBB.csv"), row.names = FALSE)
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("pixel_threshold"), integrate_perching = TRUE, perching_dataset = "RFID-IRBB", perching_prefix = "perching_events_", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("The column", col_nms[i], "was not found in the data frame", sep = " ")
    )
    
  }))

  # When camera_label is not NULL, I need to check that the video_metadata_cols are present in the main data
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      pixel_threshold = 100,
      total_pixels_motionTrigger = 10000,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    ) %>% 
    dplyr::select(-c("pixel_threshold"))
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  expect_error(
    score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = NULL, camera_label = "Video", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("pixel_threshold", "total_pixels_motionTrigger"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The column", "pixel_threshold", "was not found in the data frame", sep = " ")
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
  
  # Generate a file with pre-processed timestamps for 1 sensor type
  
  # Create 3 entrance and 3 exit events using different combinations of the 2 pairs of beam breakers. Each event is a detection cluster of a given duration (see ends below)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST"
  )) + seq(1, 300, 50)
  
  ends <- starts + rep(c(5, 10, 15), 2)
  
  # Columns that must never have NAs in the main input data
  col_nms <- c("start", "end")

  invisible(lapply(1:length(col_nms), function(i){
    
    # Create a spreadsheet of the simulated directional movement events
    sim_dats <- data.frame(start = starts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(start),
        month = month(start),
        day = day(start),
        end = ends,
        event_seq = c(
          paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
          paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
          paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
          paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
          paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
          paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
        ),
        indiv1_id = "test",
        indiv2_id = NA,
        # Matches the number of RFID detections if present above
        total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
        total_indiv2_detections = 0,
        individual_initiated = "test",
        individual_ended = "test",
        threshold_seconds = 1,
        run_length = 1,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      ) %>% 
      # Drop the given column
      dplyr::select(-c(all_of(col_nms[i])))
    
    sim_dats[[col_nms[i]]] <- NA
    
    write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = NULL, PIT_tag_col_nm = NULL, rfid_label = "RFID", camera_label = "Video", outer_irbb_label = NULL, inner_irbb_label = NULL, video_metadata_col_nms = c("pixel_threshold"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, pixel_col_nm = "total_pixels_motionTrigger", video_width = 1280, video_height = 720, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("The column", col_nms[i], "needs to be in a format compatible with temporal calculations")
    )
    
  }))
  
  # When integrate_perching = TRUE, I also need to check that the perching_end and perching_starts timestamps columns do not have NAs
  sim_dats <- data.frame(start = starts) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(start),
      month = month(start),
      day = day(start),
      end = ends,
      event_seq = c(
        paste(c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)), collapse = "; "), # entrance
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 2)), collapse = "; "), # no directionality
        paste(c(rep("Outer Beam Breaker", 4), rep("Inner Beam Breaker", 5)), collapse = "; "), # entrance
        paste(c(rep("Inner Beam Breaker", 2), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 5), rep("Outer Beam Breaker", 2)), collapse = "; "), # exit
        paste(c(rep("Inner Beam Breaker", 1), rep("Outer Beam Breaker", 4)), collapse = "; ") # exit
      ),
      indiv1_id = "test",
      indiv2_id = NA,
      # Matches the number of RFID detections if present above
      total_indiv1_detections = c(NA, 2, 3, NA, 2, 4), 
      total_indiv2_detections = 0,
      individual_initiated = "test",
      individual_ended = "test",
      threshold_seconds = 1,
      run_length = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(sim_dats, file.path(tmp_path, "processed",  "detection_clusters.csv"), row.names = FALSE)
  
  # Create a dataset of 3 simulated perching events that encompass 2 entrances and 1 exit above
  starts_p <- starts[c(1:2, 5)]
  ends_p <- starts_p + c(25, 50, 10)
  
  # Columns that must always be present in the perching event data
  col_nms <- c("perching_start", "perching_end")
  
  invisible(lapply(1:length(col_nms), function(i){
    
    # Create a spreadsheet of the simulated directional movement events
    sim_perch_rfid <- data.frame(perching_start = starts_p) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        sensor_id = "RFID",
        PIT_tag_ID = "test",
        perching_end = ends_p,
        perching_duration_s = perching_end - perching_start,
        unique_perching_event = seq(1, nrow(.), 1),
        threshold = 1,
        min_perching_run_length = 3,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      )
    
    sim_perch_irbb <- data.frame(perching_start = starts_p) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        sensor_id = "IRBB",
        perching_end = ends_p,
        perching_duration_s = perching_end - perching_start,
        unique_perching_event = seq(1, nrow(.), 1),
        threshold = 1,
        min_perching_run_length = 3,
        data_stage = "pre-processing",
        date_pre_processed = Sys.Date()
      ) 
    
    if(col_nms[i] %in% names(sim_perch_rfid)){
      sim_perch_rfid[[col_nms[i]]] <- NA
    }
    
    if(col_nms[i] %in% names(sim_perch_irbb)){
      sim_perch_irbb[[col_nms[i]]] <- NA
    }
    
    write.csv(sim_perch_rfid, file.path(tmp_path, "processed", "perching_events_RFID.csv"), row.names = FALSE)
    
    write.csv(sim_perch_irbb, file.path(tmp_path, "processed", "perching_events_IRBB.csv"), row.names = FALSE)
    
    expect_error(
      score_clusters(file_nm = "detection_clusters.csv", sensor_id_col_nm = "sensor_id", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", video_metadata_col_nms = c("pixel_threshold"), integrate_perching = TRUE, perching_dataset = "RFID-IRBB", perching_prefix = "perching_events_", pixel_col_nm = NULL, video_width = NULL, video_height = NULL, integrate_preproc_video = FALSE, path = path, data_dir = file.path(data_dir, "processed"), out_dir = file.path(data_dir, "processed"), out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("The column", col_nms[i], "needs to be in a format compatible with temporal calculations")
    )
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})
