
if (!require(testthat)) install.packages('testthat')
library(testthat)

score_detectionClusters <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/score_detectionClusters.R")$value

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

#### No perching integration ####

# Test that the function labels entrances and exits as expected when data from 1 sensor type is used as input (2 pairs of beam breakers)
test_that("The function labels entrances and exits as expected using data from 2 beam breaker pairs", {
  
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
  
  write.csv(sim_dats, file.path(tmp_path, "simulated_detectionClusters.csv"), row.names = FALSE)
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = NULL, PIT_tag_col = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

# Test that the function labels entrances and exits as expected when data from 2 sensor types is used as input (RFID and 2 pairs of beam breakers)
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

# Test that the function labels entrances and exits as expected when data from 3 sensor types is used as input (RFID, 2 pairs of beam breakers, and a camera)
test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera", {
  
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

  write.csv(sim_dats, file.path(tmp_path, "simulated_detectionClusters.csv"), row.names = FALSE)
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = NULL, PIT_tag_col = NULL, rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = FALSE, perching_dataset = NULL, perching_prefix = NULL, path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

#### RFID perching integration ####

# Test that the function labels entrances and exits as expected when data from 1 sensor type is used as input (2 pairs of beam breakers), and perching events are integrated
test_that("The function labels entrances and exits as expected using data from 2 beam breaker pairs and integrating RFID perching data", {
  
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
  
  write.csv(sim_dats, file.path(tmp_path, "simulated_detectionClusters.csv"), row.names = FALSE)

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

  write.csv(sim_perch, file.path(tmp_path, "simulated_perching_events_RFID.csv"), row.names = FALSE)
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = "sensor_id", PIT_tag_col = "PIT_tag_ID", rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, integrate_perching = TRUE, perching_dataset = "RFID", perching_prefix = "simulated_perching_events_", path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

# Test that the function labels entrances and exits as expected when data from 2 sensor types is used as input (RFID and 2 pairs of beam breakers), and perching events are integrated
test_that("The function labels entrances and exits as expected using data from RFID and 2 beam breaker pairs and integrating RFID perching data", {
  
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
  
  write.csv(sim_perch, file.path(tmp_path, "simulated_perching_events_RFID.csv"), row.names = FALSE)
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = "sensor_id", PIT_tag_col = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, integrate_perching = TRUE, perching_dataset = "RFID", perching_prefix = "simulated_perching_events_", path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

# Test that the function labels entrances and exits as expected when data from 3 sensor types is used as input (RFID, 2 pairs of beam breakers, and a camera), and perching events are integrat
test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera and integrating RFID perching data", {
  
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
  
  write.csv(sim_dats, file.path(tmp_path, "simulated_detectionClusters.csv"), row.names = FALSE)
  
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
  
  write.csv(sim_perch, file.path(tmp_path, "simulated_perching_events_RFID.csv"), row.names = FALSE)
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = "sensor_id", PIT_tag_col = "PIT_tag_ID", rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = TRUE, perching_dataset = "RFID", perching_prefix = "simulated_perching_events_", path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

#### IRBB perching integration ####

# Test that the function labels entrances and exits as expected when data from 1 sensor type is used as input (2 pairs of beam breakers), and perching events are integrated
test_that("The function labels entrances and exits as expected using data from 2 beam breaker pairs and integrating IRBB perching data", {
  
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
  
  write.csv(sim_dats, file.path(tmp_path, "simulated_detectionClusters.csv"), row.names = FALSE)
  
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
  
  write.csv(sim_perch, file.path(tmp_path, "simulated_perching_events_IRBB.csv"), row.names = FALSE)
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = "sensor_id", PIT_tag_col = NULL, rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, integrate_perching = TRUE, perching_dataset = "IRBB", perching_prefix = "simulated_perching_events_", path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

# Test that the function labels entrances and exits as expected when data from 2 sensor types is used as input (RFID and 2 pairs of beam breakers), and perching events are integrated
test_that("The function labels entrances and exits as expected using data from RFID and 2 beam breaker pairs and integrating IRBB perching data", {
  
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
  
  write.csv(sim_perch, file.path(tmp_path, "simulated_perching_events_IRBB.csv"), row.names = FALSE)
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = "sensor_id", PIT_tag_col = NULL, rfid_label = "RFID", camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, integrate_perching = TRUE, perching_dataset = "IRBB", perching_prefix = "simulated_perching_events_", path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

# Test that the function labels entrances and exits as expected when data from 3 sensor types is used as input (RFID, 2 pairs of beam breakers, and a camera), and perching events are integrat
test_that("The function labels entrances and exits as expected using data from RFID, 2 beam breaker pairs, and a camera and integrating IRBB perching data", {
  
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
  
  write.csv(sim_dats, file.path(tmp_path, "simulated_detectionClusters.csv"), row.names = FALSE)
  
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
  
  write.csv(sim_perch, file.path(tmp_path, "simulated_perching_events_IRBB.csv"), row.names = FALSE)
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = "sensor_id", PIT_tag_col = NULL, rfid_label = "RFID", camera_label = "Camera", outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name"), integrate_perching = TRUE, perching_dataset = "IRBB", perching_prefix = "simulated_perching_events_", path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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


#### RFID and IRBB perching integration ####

# Test that the function labels entrances and exits as expected when data from 1 sensor type is used as input (2 pairs of beam breakers), and perching events are integrated from RFID and IRBB datasets
test_that("The function labels entrances and exits as expected using data from 2 beam breaker pairs and integrating RFID and IRBB perching data", {
  
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
  
  write.csv(sim_dats, file.path(tmp_path, "simulated_detectionClusters.csv"), row.names = FALSE)
  
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
  
  write.csv(sim_perch_rfid, file.path(tmp_path, "simulated_perching_events_RFID.csv"), row.names = FALSE)
  
  write.csv(sim_perch_irbb, file.path(tmp_path, "simulated_perching_events_IRBB.csv"), row.names = FALSE)
  
  source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/score_detectionClusters.R")
  
  score_detectionClusters(file_nm = "simulated_detectionClusters.csv", sensor_id_col = "sensor_id", PIT_tag_col = "PIT_tag_ID", rfid_label = NULL, camera_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, integrate_perching = TRUE, perching_dataset = "RFID-IRBB", perching_prefix = "simulated_perching_events_", path = path, data_dir = data_dir, out_dir = data_dir, out_file_nm = "scored_detectionClusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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
    
    paste(unique(strsplit(sim_dats$event_seq[i], split = "; ")[[1]])[1:2], collapse = " - ")
    
  })
  
  invisible(lapply(1:nrow(test_res), function(i){
    expect_equal(test_res$Edge_1[i], sensor_seq[i])
  }))
  
  # Test that the correct entrance and/or exit movements were assigned to perching events
  i <- 3
  invisible(lapply(1:nrow(test_res), function(i){
    
    if(test_res$start[i] %in% starts_p){
      
      expect_true(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_irbb", names(test_res))], is.na)))
      
    } else {
      
      # TKTK a problem here
      expect_false(all(!sapply(test_res[i, grep("perching_rfid", names(test_res))], is.na)) | all(!sapply(test_res[i, grep("perching_irbb", names(test_res))], is.na)))
      
    }
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# TKTK: next steps are to perform the same tests across numbers of sensors but with IRBB perching event integration, then RFID + IRBB perching event integration

########## Testing error messages ########## 

