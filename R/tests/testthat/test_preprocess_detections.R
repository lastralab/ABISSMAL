# G. Smith-Vidaurre
# 03 January 2023

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("/home/gsvidaurre/Desktop/GitHub_repos/ABISSMAL/R/preprocess_detections.R")

source("/home/gsvidaurre/Desktop/GitHub_repos/ABISSMAL/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

########## Testing output ########## 

test_that("The correct number and timing of discrete movement events are retained per pre-processing mode for RFID data and a single temporal threshold", {
  
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
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  ####### `retain_first` mode #######
  preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # Check that each detection cluster is separated by more than the given temporal threshold
  diffs <- test_res$timestamp_ms - lag(test_res$timestamp_ms)
  diffs <- diffs[!is.na(diffs)]
  
  expect_true(all(diffs >= th))
  
  # Check that the first timestamp from the raw data is returned as the timestamp per detection cluster
  tmp_starts <- starts[order(starts)]
  
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$timestamp_ms[x], tmp_starts[x])
    
  }))
  
  ####### `thin` mode #######
  preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "thin", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detections. This should be the number of total clusters by the number of detections expected per cluster using mode = 'thin'
  expect_equal(nrow(test_res), length(starts) * length(seq(1, length(event_ts[[1]]), 2)))
  
  # Test detections are separated by more than the given temporal threshold
  diffs <- test_res$timestamp_ms - lag(test_res$timestamp_ms)
  diffs <- diffs[!is.na(diffs)]
  
  expect_true(all(diffs >= th))
  
  # Test that the every other timestamp from the raw data is returned as the timestamp per detection cluster
  tstmps <- c(
    event_ts[[1]][-seq(2, length(event_ts[[1]]) - 1, 2)], 
    event_ts[[3]][-seq(2, length(event_ts[[3]]) - 1, 2)], 
    event_ts[[2]][-seq(2, length(event_ts[[2]]) - 1, 2)], 
    event_ts[[4]][-seq(2, length(event_ts[[4]]) - 1, 2)]
  )

  expect_equal(test_res$timestamp_ms, tstmps)
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete movement events are retained per pre-processing mode for RFID data and a single temporal threshold for an even number of timestamps", {
  
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
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  
  # Create 4 clusters of detections: each cluster consists of 20 detections spaced 0.5 seconds apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  event_ts <- sapply(1:length(starts), function(x){
    
    tmp <- seq(starts[x], ends[x], 0.5)
    tmp <- tmp[-length(tmp)]
    
    return(tmp)
    
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  ####### `retain_first` mode #######
  preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # Check that each detection cluster is separated by more than the given temporal threshold
  diffs <- test_res$timestamp_ms - lag(test_res$timestamp_ms)
  diffs <- diffs[!is.na(diffs)]
  
  expect_true(all(diffs >= th))
  
  # Check that the first timestamp from the raw data is returned as the timestamp per detection cluster
  tmp_starts <- starts[order(starts)]
  
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$timestamp_ms[x], tmp_starts[x])
    
  }))
  
  ####### `thin` mode #######
  preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "thin", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detections. This should be the number of total clusters by the number of detections expected per cluster using mode = 'thin'
  expect_equal(nrow(test_res), length(starts) * length(seq(1, length(event_ts[[1]]), 2)))
  
  # Test detections are separated by more than the given temporal threshold
  diffs <- test_res$timestamp_ms - lag(test_res$timestamp_ms)
  diffs <- diffs[!is.na(diffs)]
  
  expect_true(all(diffs >= th))
  
  # Test that the every other timestamp from the raw data is returned as the timestamp per detection cluster
  tstmps <- c(
    event_ts[[1]][-seq(2, length(event_ts[[1]]), 2)], 
    event_ts[[3]][-seq(2, length(event_ts[[3]]), 2)], 
    event_ts[[2]][-seq(2, length(event_ts[[2]]), 2)], 
    event_ts[[4]][-seq(2, length(event_ts[[4]]), 2)]
  )
  
  expect_equal(test_res$timestamp_ms, tstmps)
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete movement events are retained per pre-processing mode for RFID data and a single temporal threshold when movement events are short", {
  
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
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  
  # Create 4 clusters of detections: each cluster consists of 21 detections spaced 0.5 seconds apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 0.5
  
  event_ts <- sapply(1:length(starts), function(x){
    
    return(seq(starts[x], ends[x], length.out = 2))
    
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  ####### `retain_first` mode #######
  preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # Check that each detection cluster is separated by more than the given temporal threshold
  diffs <- test_res$timestamp_ms - lag(test_res$timestamp_ms)
  diffs <- diffs[!is.na(diffs)]
  
  expect_true(all(diffs >= th))
  
  # Check that the first timestamp from the raw data is returned as the timestamp per detection cluster
  tmp_starts <- starts[order(starts)]
  
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$timestamp_ms[x], tmp_starts[x])
    
  }))
  
  ####### `thin` mode #######
  preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "thin", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detections. This should be the number of total clusters by the number of detections expected per cluster using mode = 'thin'
  expect_equal(nrow(test_res), length(starts) * length(seq(1, length(event_ts[[1]]), 2)))
  
  # Test detections are separated by more than the given temporal threshold
  diffs <- test_res$timestamp_ms - lag(test_res$timestamp_ms)
  diffs <- diffs[!is.na(diffs)]
  
  expect_true(all(diffs >= th))
  
  # Test that the first timestamp per sequence from the raw data is returned as the timestamp per detection cluster
  tstmps <- c(
    event_ts[[1]][1], 
    event_ts[[3]][1], 
    event_ts[[2]][1], 
    event_ts[[4]][1]
  )
  
  expect_equal(test_res$timestamp_ms, tstmps)
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete movement events are retained per pre-processing mode for RFID data and multiple temporal thresholds", {
  
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
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections spaced different numbers of seconds apart (testing `threshold`)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  ths <- seq(0.5, 5, by = 0.5)
  
  invisible(lapply(1:length(ths), function(x){
    
    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], ends[i], by = ths[x])
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    # Write out a spreadsheet with these timestamps that will be used as input data for the function
    sim_ts <- data.frame(timestamp_ms = tmp_tstmps$tstmps) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
        sensor_id = "RFID",
        data_type = "RFID",
        data_stage = "raw_combined",
        date_combined = Sys.Date(),
        PIT_tag_ID = "test"
      )
    
    write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
    
    ####### `retain_first` mode #######
    
    preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Check that the results contain the expected number of detection clusters
    expect_equal(nrow(test_res), length(starts))
    
    # Check that each detection cluster is separated by more than the given temporal threshold
    diffs <- test_res$timestamp_ms - lag(test_res$timestamp_ms)
    diffs <- diffs[!is.na(diffs)]
    
    expect_true(all(diffs >= ths[x]))
    
    # Check that the first timestamp from the raw data is returned as the timestamp per detection cluster
    tmp_starts <- starts[order(starts)]
    
    invisible(lapply(1:nrow(test_res), function(z){
      
      expect_equal(test_res$timestamp_ms[z], tmp_starts[z])
      
    }))
    
    ####### `thin` mode #######
    preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "thin", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Check that the results contain the expected number of detections. This should be the number of total clusters by the number of detections expected per cluster using mode = 'thin'
    # cat("thin mode: th = ", ths[x], "\n")
    
    num_seq <- seq(starts[1], ends[1], by = ths[x])
    
    expect_equal(nrow(test_res), length(starts) * length(seq(1, length(num_seq), 2)))
    
    # Check that detections are separated by more than the given temporal threshold
    diffs <- test_res$timestamp_ms - lag(test_res$timestamp_ms)
    diffs <- diffs[!is.na(diffs)]
    
    expect_true(all(diffs >= ths[x]))
    
    # Check that the every other timestamp from the raw data is returned as the timestamp per detection cluster
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], ends[i], by = ths[x])
      
      if(length(sim_ts) %% 2 != 0){
        
        sim_ts <- sim_ts[-seq(2, length(sim_ts) - 1, 2)]
        
      } else if(length(sim_ts) %% 2 == 0){
        
        sim_ts <- sim_ts[-seq(2, length(sim_ts), 2)]
        
      }
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    tmp_tstmps$tstmps <- tmp_tstmps$tstmps[order(tmp_tstmps$tstmps)]
    
    expect_equal(test_res$timestamp_ms, tmp_tstmps$tstmps)
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete movement events are retained per pre-processing mode for IRBB data and a single temporal threshold", {
  
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
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Generate a file with raw timestamps for 2 sensors
  
  # Create 4 clusters of detections: each cluster consists of 21 detections spaced 0.5 seconds apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  event_ts <- sapply(1:length(starts), function(x){
    
    return(seq(starts[x], ends[x], 0.5))
    
  }, simplify = FALSE)
  
  sns <- c("Outer Beam Breaker", "Inner Beam Breaker")
  
  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  sim_ts <- data.frame(timestamp_ms = c(event_ts[[1]], event_ts[[2]], event_ts[[3]], event_ts[[4]])) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = rep(sns, each = length(event_ts[[1]]) * 2),
      data_type = "IRBB",
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
  
  th <- 1
  
  ####### `retain_first` mode #######
  preprocess_detections(sensor = "IRBB", timestamps_col_nm = "timestamp_ms", group_col_nm = "sensor_id", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # Check that each detection cluster per beam breaker pair is separated by more than the given temporal threshold
  invisible(lapply(1:length(sns), function(i){
    
    tmp <- test_res %>% 
      dplyr::filter(sensor_id == sns[i])
    
    diffs <- tmp$timestamp_ms - lag(tmp$timestamp_ms)
    diffs <- diffs[!is.na(diffs)]
    
    expect_true(all(diffs >= th))
    
  }))
  
  # Check that the first timestamp from the raw data is returned as the timestamp per detection cluster
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$timestamp_ms[x], starts[x])
    
  }))
  
  ####### `thin` mode #######
  preprocess_detections(sensor = "IRBB", timestamps_col_nm = "timestamp_ms", group_col_nm = "sensor_id", pixel_col_nm = NULL, thin_threshold = th, mode = "thin", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Across beam breaker pairs, check that the results contain the expected number of detections. This should be the number of total clusters by the number of detections expected per cluster using mode = 'thin'
  expect_equal(nrow(test_res), length(starts) * length(seq(1, length(event_ts[[1]]), 2)))
  
  # Check that each detection cluster per beam breaker pair is separated by more than the given temporal threshold
  invisible(lapply(1:length(sns), function(i){
    
    tmp <- test_res %>% 
      dplyr::filter(sensor_id == sns[i])
    
    diffs <- tmp$timestamp_ms - lag(tmp$timestamp_ms)
    diffs <- diffs[!is.na(diffs)]
    
    expect_true(all(diffs >= th))
    
  }))
  
  # Check that for each beam breaker pair, every other timestamp from the raw data is returned as the timestamp per detection cluster
  tstmps_outer <- c(
    event_ts[[1]][-seq(2, length(event_ts[[1]]) - 1, 2)], 
    event_ts[[2]][-seq(2, length(event_ts[[2]]) - 1, 2)]
  )
  
  tstmps_inner <- c(
    event_ts[[3]][-seq(2, length(event_ts[[3]]) - 1, 2)], 
    event_ts[[4]][-seq(2, length(event_ts[[4]]) - 1, 2)]
  )
  
  tmp_tstmps <- list(tstmps_outer, tstmps_inner)
  
  invisible(lapply(1:length(sns), function(i){
    
    tmp <- test_res %>% 
      dplyr::filter(sensor_id == sns[i])
    
    expect_equal(tmp$timestamp_ms, tmp_tstmps[[i]])
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete movement events are retained per pre-processing mode for IRBB data and multiple temporal thresholds", {
  
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
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections spaced different numbers of seconds apart (testing `threshold`)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  ths <- seq(0.5, 5, by = 0.5)
  
  invisible(lapply(1:length(ths), function(x){
    
    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], ends[i], by = ths[x])
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    sns <- c("Outer Beam Breaker", "Inner Beam Breaker")
    
    # Write out a spreadsheet with these timestamps that will be used as input data for the function
    sim_ts <- data.frame(timestamp_ms = tmp_tstmps$tstmps) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(timestamp_ms),
        month = month(timestamp_ms),
        day = day(timestamp_ms),
        original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
        sensor_id = rep(sns, each = nrow(.) / length(sns)),
        data_type = "IRBB",
        data_stage = "raw_combined",
        date_combined = Sys.Date()
      )
    
    write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
    
    ####### `retain_first` mode #######
    preprocess_detections(sensor = "IRBB", timestamps_col_nm = "timestamp_ms", group_col_nm = "sensor_id", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Check that the results contain the expected number of detection clusters
    expect_equal(nrow(test_res), length(starts))
    
    # Check that each detection cluster per beam breaker pair is separated by more than the given temporal threshold
    invisible(lapply(1:length(sns), function(i){
      
      tmp <- test_res %>% 
        dplyr::filter(sensor_id == sns[i])
      
      diffs <- tmp$timestamp_ms - lag(tmp$timestamp_ms)
      diffs <- diffs[!is.na(diffs)]
      
      expect_true(all(diffs >= ths[x]))
      
    }))
    
    # Check that the first timestamp from the raw data is returned as the timestamp per detection cluster
    invisible(lapply(1:nrow(test_res), function(z){
      
      expect_equal(test_res$timestamp_ms[z], starts[z])
      
    }))
    
    ####### `thin` mode #######
    preprocess_detections(sensor = "IRBB", timestamps_col_nm = "timestamp_ms", group_col_nm = "sensor_id", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "thin", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Check that the results contain the expected number of detections. This should be the number of total clusters by the number of detections expected per cluster using mode = 'thin'
    num_seq <- seq(starts[1], ends[1], by = ths[x])
    
    expect_equal(nrow(test_res), length(starts) * length(seq(1, length(num_seq), 2)))
    
    # Check that each detection cluster per beam breaker pair is separated by more than the given temporal threshold
    invisible(lapply(1:length(sns), function(i){
      
      tmp <- test_res %>% 
        dplyr::filter(sensor_id == sns[i])
      
      diffs <- tmp$timestamp_ms - lag(tmp$timestamp_ms)
      diffs <- diffs[!is.na(diffs)]
      
      expect_true(all(diffs >= ths[x]))
      
    }))
    
    # Check that the every other timestamp from the raw data is returned as the timestamp per detection cluster
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], ends[i], by = ths[x])
      
      if(length(sim_ts) %% 2 != 0){
        
        sim_ts <- sim_ts[-seq(2, length(sim_ts) - 1, 2)]
        
      } else if(length(sim_ts) %% 2 == 0){
        
        sim_ts <- sim_ts[-seq(2, length(sim_ts), 2)]
        
      }
      
      return(data.frame(tstmps = sim_ts, cluster = i))
      
    }))
    
    tmp_tstmps$tstmps <- tmp_tstmps$tstmps[order(tmp_tstmps$tstmps)]
    
    expect_equal(test_res$timestamp_ms, tmp_tstmps$tstmps)
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete movement events are retained for video data and a single pixel threshold", {
  
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
  # library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Create the input data directory that the function expects
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Generate a file of raw video recording timestamps
  
  # Create 4 video recording events
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
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
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  preprocess_detections(sensor = "Video", timestamps_col_nm = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = px, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_Video.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detection clusters (this should be 1/2 the number of timestamps, see pixel values specified above)
  expect_equal(nrow(test_res), length(tstmps) / 2)
  
  # Check that pixel values per video are greater than the specified pixel threshold
  expect_true(all(test_res$total_pixels_motionTrigger >= px))
  
  # Check that there is a pre- and post-motion video recording per timestamp
  tstmps2 <- unique(test_res$timestamp_ms)
  
  invisible(lapply(1:length(tstmps2), function(i){
    
    nms <- test_res %>% 
      dplyr::filter(timestamp_ms == tstmps2[i]) %>% 
      pull(video_file_name)
    
    expect_true(length(c(nms[grepl("pre", nms)], nms[grepl("post", nms)])) == 2)
    
  }))
  
  #### All of the videos have total pixels greater than the pixel threshold ####
  
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
      total_pixels_motionTrigger = px * 10, 
      pixel_threshold = px, 
      video_file_name = paste(paste(rep(paste("Box_01_2023_1_1", paste(hour(starts), minute(starts), second(starts), sep = "_"), sep = "_"), 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = ""),
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  preprocess_detections(sensor = "Video", timestamps_col_nm = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = px, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_Video.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      timestamp_ms = as.POSIXct(format(as.POSIXct(timestamp_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detection clusters (this should be the number of timestamps, see above)
  expect_equal(nrow(test_res), length(tstmps))
  
  # Check that pixel values per video are greater than the specified pixel threshold
  expect_true(all(test_res$total_pixels_motionTrigger >= px))
  
  # Check that there is a pre- and post-motion video recording per timestamp
  tstmps2 <- unique(test_res$timestamp_ms)
  
  invisible(lapply(1:length(tstmps2), function(i){
    
    nms <- test_res %>% 
      dplyr::filter(timestamp_ms == tstmps2[i]) %>% 
      pull(video_file_name)
    
    expect_true(length(c(nms[grepl("pre", nms)], nms[grepl("post", nms)])) == 2)
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})


########## Testing error handling ########## 

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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){
    dir.create(file.path(tmp_path, "raw_combined"))
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  # General arguments that cannot ever be NULL:
  arg_nms <- c("sensor", "timestamps_col_nm", "path", "data_dir", "out_dir", "tz", "POSIXct_format")
  
  args <- list(
    `sensor` = "RFID",
    `timestamps_col_nm` = "timestamp_ms",
    `path` = path,
    `data_dir` = file.path(data_dir, "raw_combined"),
    `out_dir` = file.path(data_dir, "processed"),
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      preprocess_detections(sensor = args[["sensor"]], timestamps_col_nm = args[["timestamps_col_nm"]], group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # Arguments that cannot be NULL depending on which sensor is specified:
  
  # When sensor = RFID|IRBB, the arguments group_col_nm, thin_threshold, mode cannot be NULL
  arg_nms <- c("group_col_nm", "thin_threshold", "mode")
  
  args <- list(
    `group_col_nm` = "PIT_tag_ID",
    `thin_threshold` = th,
    `mode` = "thin"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamps_col_nm", group_col_nm = args[["group_col_nm"]], pixel_col_nm = NULL, thin_threshold = args[["thin_threshold"]], mode = args[["mode"]], pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # When sensor = Video, the arguments pixel_col_nm, pixel_threshold cannot be NULL
  px <- 100
  arg_nms <- c("pixel_col_nm", "pixel_threshold")
  
  args <- list(
    `pixel_col_nm` = "PIT_tag_ID",
    `pixel_threshold` = px
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      preprocess_detections(sensor = "Video", timestamps_col_nm = "timestamps_col_nm", group_col_nm = NULL, pixel_col_nm = args[["pixel_col_nm"]], thin_threshold = NULL, mode = NULL, pixel_threshold = args[["pixel_threshold"]], drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when the sensor argument is not RFID, IRBB, or Video", {
  
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  expect_error(
    preprocess_detections(sensor = 1, timestamps_col_nm = "timestamps_col_nm", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The value provided for the argument, sensor, is not correct. Check your spelling or captialization"
  )
  
  expect_error(
    preprocess_detections(sensor = "RFD", timestamps_col_nm = "timestamps_col_nm", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The value provided for the argument, sensor, is not correct. Check your spelling or captialization"
  )
  
  expect_error(
    preprocess_detections(sensor = "rfid", timestamps_col_nm = "timestamps_col_nm", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The value provided for the argument, sensor, is not correct. Check your spelling or captialization"
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  # Arguments that should be NULL when sensor = RFID:
  arg_nms <- c("pixel_col_nm", "pixel_threshold")
  
  args <- list(
    `pixel_col_nm` = NULL,
    `pixel_threshold` = NULL
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- "test"
    
    expect_error(
      preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamps_col_nm", group_col_nm = "PIT_tag_ID", pixel_col_nm = args[["pixel_col_nm"]], thin_threshold = th, mode = "thin", pixel_threshold = args[["pixel_threshold"]], drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  # Arguments that should be NULL when sensor = Video:
  arg_nms <- c("group_col_nm", "thin_threshold", "mode", "drop_tag")
  
  args <- list(
    `group_col_nm` = NULL,
    `thin_threshold` = NULL,
    `mode` = NULL,
    `drop_tag` = NULL
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- "test"
    
    expect_error(
      preprocess_detections(sensor = "Video", timestamps_col_nm = "timestamps_col_nm", group_col_nm = args[["group_col_nm"]], pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = args[["thin_threshold"]], mode = args[["mode"]], pixel_threshold = "pixel_threshold", drop_tag = args[["drop_tag"]], path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  # General arguments that should always be strings. Not including sensor here because this argument is checked in a separate test above
  arg_nms <- c("timestamps_col_nm", "path", "data_dir", "out_dir", "tz", "POSIXct_format")
  
  args <- list(
    `timestamps_col_nm` = "timestamp_ms",
    `path` = path,
    `data_dir` = file.path(data_dir, "raw_combined"),
    `out_dir` = file.path(data_dir, "processed"),
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      preprocess_detections(sensor = "RFID", timestamps_col_nm = args[["timestamps_col_nm"]], group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
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
      preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamps_col_nm", group_col_nm = args[["group_col_nm"]], pixel_col_nm = NULL, thin_threshold = th, mode = args[["mode"]], pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  # Arguments that should be strings when sensor = Video:
  arg_nms <- c("pixel_col_nm")
  
  args <- list(
    `pixel_col_nm` = "total_pixels_motionTrigger"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      preprocess_detections(sensor = "Video", timestamps_col_nm = "timestamps_col_nm", group_col_nm = NULL, pixel_col_nm = args[["pixel_col_nm"]], thin_threshold = NULL, mode = NULL, pixel_threshold = px, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = as.character(th), mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "Video", timestamps_col_nm = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = as.character(px), drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    ) 
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  # Remove this file
  file.remove(file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"))
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The file combined_raw_data_RFID.csv does not exist in the directory", file.path(tmp_path, "raw_combined"), sep = " ")
  )
  
  # Generate the file again, then remove the whole directory
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  # Remove the directory where this file is located
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The directory", file.path(path, data_dir, "raw_combined"), "does not exist", sep = " ")
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
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
        data_stage = "raw_combined",
        date_combined = Sys.Date(),
        PIT_tag_ID = "test"
      ) %>% 
      # Drop the given column
      dplyr::select(-c(all_of(col_nms[i])))
    
    write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
    
    expect_error(
      preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    ) %>% 
    # Drop the PIT_tag_ID column
    dplyr::select(-c(PIT_tag_ID))
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    ) %>% 
    # Drop the pixels column
    dplyr::select(-c(total_pixels_motionTrigger))
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "Video", timestamps_col_nm = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = px, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
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
        data_stage = "raw_combined",
        date_combined = Sys.Date(),
        PIT_tag_ID = "test"
      )
    
    sim_ts[[col_nms[i]]][1] <- NA
    
    write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
    
    expect_error(
      preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
      data_stage = "raw_combined",
      date_combined = Sys.Date(),
      PIT_tag_ID = "test"
    )
  
  sim_ts[["PIT_tag_ID"]][1] <- NA
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col_nm = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  sim_ts[["total_pixels_motionTrigger"]][1] <- NA
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_Video.csv"), row.names = FALSE)
  
  expect_error(
    preprocess_detections(sensor = "Video", timestamps_col_nm = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = px, drop_tag = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The column total_pixels_motionTrigger has NA values")
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})
