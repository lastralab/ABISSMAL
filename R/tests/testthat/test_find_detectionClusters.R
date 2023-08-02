
# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

find_detectionClusters <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_detectionClusters.R")$value

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

########## Testing output ########## 

# Each of these test_that expressions contains several expect_equal statements

# Test that the function detects the expected clusters of detections when data from one sensor is used as input
test_that("The function detects the expected number of clusters using data from one sensor", {
  
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
  
  write.csv(sim_ts, file.path(tmp_path, "simulated_single_sensor.csv"), row.names = FALSE)
  
  # Note that the run length needs to be set to 1 in order to correctly detect detection clusters of length 2
  find_detectionClusters(file_nms = "simulated_single_sensor.csv", threshold = 2, run_length = 1, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_cols = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "detection_clusters.csv"))
  
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

# Test that the function detects the expected clusters of detections when data from 2 sensor types is used as input (RFID and 2 pairs of beam breakers here)
test_that("The function detects the expected number of clusters using data from one sensor", {
  
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
  
  write.csv(rfid_ts, file.path(tmp_path, "simulated_rfid.csv"), row.names = FALSE)
  write.csv(irbb_ts, file.path(tmp_path, "simulated_irbb.csv"), row.names = FALSE)
  
  # Note that the run length needs to be set to 1 in order to correctly detect detection clusters of length 2
  find_detectionClusters(file_nms = c("simulated_rfid.csv", "simulated_irbb.csv"), threshold = 2, run_length = 1, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_cols = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "detection_clusters.csv"))
  
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

# Test that the function detects the expected clusters of detections when data from multiple sensors is used as input (RFID, 2 pairs of beam breakers, and video recording events)
test_that("The function detects the expected number of clusters using data from three sensor types", {
  
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
  
  write.csv(rfid_ts, file.path(tmp_path, "simulated_rfid.csv"), row.names = FALSE)
  write.csv(irbb_ts, file.path(tmp_path, "simulated_irbb.csv"), row.names = FALSE)
  write.csv(camera_ts, file.path(tmp_path, "simulated_camera.csv"), row.names = FALSE)
  
  # Note that the run length needs to be set to 1 in order to correctly detect detection clusters of length 2
  find_detectionClusters(file_nms = c("simulated_rfid.csv", "simulated_irbb.csv", "simulated_camera.csv"), threshold = 2, run_length = 1, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = "PIT_tag_ID", rfid_label = "RFID", camera_label = "Camera", drop_tag = NULL, preproc_metadata_cols = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "detection_clusters.csv"))
  
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


# Test that the function detects the expected clusters of detections across different combinations of `threshold` and `run_length` when data from one sensor is used as input
test_that("The function detects the expected number of clusters using data from one sensor across different thresholds and run_lengths", {
  
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
    
    write.csv(sim_ts, file.path(tmp_path, tmp_nm), row.names = FALSE)
    
    find_detectionClusters(file_nms = tmp_nm, threshold = x, run_length = y, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_cols = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "detection_clusters.csv"))
    
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

# next: do the same test as immediately above but with 2 and then 3 sensor types





########## Testing error messages ########## 









library(tidyverse)

l_th <- 0
u_th <- 5
rfid_file_nm <- "pre_processed_data_RFID.csv"
irbb_file_nm <- "labeled_beamBreaker_data.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
outer_irbb_col <- "Outer_beam_breaker"
inner_irbb_col <- "Inner_beam_breaker"
irbb_event_col <- "irbb_direction_inferred"
irbb_unique_col <- "unique_entranceExit"
preproc_metadata_cols <- c("thin_threshold_s", "data_stage", "date_pre_processed", "lower_threshold_s", "upper_threshold_s", "date_labeled")
method <- "temporal"
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
data_dir <- "pre_processed"
out_dir <- "integrated"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"
integrate_perching <- TRUE

general_metadata_cols