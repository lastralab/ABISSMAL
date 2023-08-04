

rm(list = ls())

library(tidyverse)
library(data.table)

file_nm <- "detection_clusters.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
general_metadata_cols <- c("chamber_id", "year", "month", "day")
video_metadata_cols <- c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name")
rfid_label <- "RFID"
camera_label <- "Camera"
outer_irbb_label <- "Outer Beam Breaker"
inner_irbb_label <- "Inner Beam Breaker"
integrate_perching <- TRUE
# perching_dataset <- "RFID"
perching_dataset <- "RFID-IRBB"
perching_prefix <- "perching_events_"
# path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
path <- "/home/gsvidaurre/Desktop/MANUSCRIPTS/Prep/ParentalCareTracking_MethodsPaper/ABS_2023_Talk"
data_dir <- "processed"
out_dir <- "processed"
out_file_nm <- "scored_detectionClusters.csv"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS" 


if (!require(testthat)) install.packages('testthat')
library(testthat)

score_detectionClusters <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/score_detectionClusters.R")$value

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

# Test that the function labels entrances and exits as expected when data from 2 sensor types is used as input (RFID and 2 pairs of beam breakers here)
test_that("The function labels entrances and exits as expected using data from RFID and 2 beam breaker pairs", {
  
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
  
  write.csv(sim_dats, file.path(tmp_path, "simulated_detectionClusters.csv"), row.names = FALSE)
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = NULL, PIT_tag_col = NULL, rfid_label = "RFID", camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "scored_detectionClusters.csv"))
  
  # Test that the results are 3 entrance events and 3 exit events, in that order
  event_labels <- sapply(1:nrow(test_res), function(i){
    
    wh <- which(!is.na(test_res[i, grep("direction", names(test_res))]))
    test_res[i, grep("direction", names(test_res))][[wh]]

  })
  
  expect_equal(event_labels, rep(c("entrance", "exit"), each = 3))
  
  # Test that the number of detection clusters in the input and output data are the same
  expect_equal(nrow(sim_dats), nrow(test_res))
  
  # Test that order of sensor labels in the first edge is correct
  sensor_seq <- sapply(1:nrow(test_res), function(i){
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]]), collapse = " - ")
    
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


########## Testing error messages ########## 

