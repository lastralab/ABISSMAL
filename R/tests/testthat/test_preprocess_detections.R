# G. Smith-Vidaurre
# 03 January 2023

# See more info on the testthat package: https://r-pkgs.org/testing-basics.html

# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

if (!require(testthat)) install.packages('testthat')
library(testthat)

preprocess_detections <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/preprocess_detections.R", print = FALSE)$value

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

########## Testing output ########## 

###### RFID ######

# A single temporal threshold
test_that("The correct number and timing of discrete movement events are retained per pre-processing mode", {
  
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
  dir.create(file.path(tmp_path, "raw_combined"))
  
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
  preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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
  preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "thin", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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

# Multiple temporal thresholds
test_that("The correct number and timing of discrete movement events are retained per pre-processing mode", {
  
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
  dir.create(file.path(tmp_path, "raw_combined"))
  
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
    
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # cat("retain first mode: th = ", ths[x], "\n")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "thin", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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

###### Beam breakers ######

# A single temporal threshold
test_that("The correct number and timing of discrete movement events are retained per pre-processing mode", {
  
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
  dir.create(file.path(tmp_path, "raw_combined"))
  
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
  preprocess_detections(sensor = "IRBB", timestamps_col = "timestamp_ms", group_col_nm = "sensor_id", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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
  preprocess_detections(sensor = "IRBB", timestamps_col = "timestamp_ms", group_col_nm = "sensor_id", pixel_col_nm = NULL, thin_threshold = th, mode = "thin", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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

# Multiple temporal thresholds
test_that("The correct number and timing of discrete movement events are retained per pre-processing mode", {
  
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
  dir.create(file.path(tmp_path, "raw_combined"))
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections spaced different numbers of seconds apart (testing `threshold`)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  ths <- seq(0.5, 5, by = 0.5)
  
  x <- 1
  ths[x]
  
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
    preprocess_detections(sensor = "IRBB", timestamps_col = "timestamp_ms", group_col_nm = "sensor_id", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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
    invisible(lapply(1:nrow(test_res), function(z){
      
      expect_equal(test_res$timestamp_ms[z], starts[z])
      
    }))
    
    ####### `thin` mode #######
    preprocess_detections(sensor = "IRBB", timestamps_col = "timestamp_ms", group_col_nm = "sensor_id", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "thin", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_IRBB.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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
      
      expect_true(all(diffs >= th))
      
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

###### Video ######

# A single pixel threshold, with videos that have total pixels greater and less than this threshold
test_that("The correct number and timing of discrete movement events are retained", {
  
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
  dir.create(file.path(tmp_path, "raw_combined"))
  
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
  
  preprocess_detections(sensor = "Video", timestamps_col = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = px, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_Video.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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
  
  preprocess_detections(sensor = "Video", timestamps_col = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = px, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_Video.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
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


########## Testing error messages ########## 

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
  dir.create(file.path(tmp_path, "raw_combined"))
  
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
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = as.character(th), mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("Expected a numeric value but", th, "is not numeric", sep = " ")
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
    preprocess_detections(sensor = "Video", timestamps_col = "timestamp_ms", group_col_nm = NULL, pixel_col_nm = "total_pixels_motionTrigger", thin_threshold = NULL, mode = NULL, pixel_threshold = as.character(px), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("Expected a numeric value but the argument pixel_threshold is not numeric", sep = " ")
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# TKTK CONTINUE
# test that the the function catches when non-NULL arguments are NULL
# test that the the function catches when NULL arguments are non-NULL
# test that the the function catches when character string arguments are not strings

# TKTK make a test of sensor id spelling, and check errors in the function for other tests that should be done

# TKTK next up
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
  dir.create(file.path(tmp_path, "raw_combined"))
  
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
    ) %>% 
    # Drop the PIT_tag_ID column
    dplyr::select(-c(PIT_tag_ID))
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The column PIT_tag_ID was not found in the data frame")
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# TKTK next up
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
  dir.create(file.path(tmp_path, "raw_combined"))
  
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
  
  # Replace one value in the detections column with an NA value
  sim_ts$original_timestamp[1] <- NA
  # head(sim_ts)
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The column timestamp_ms needs to be in a format compatible with temporal calculations"
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})



# TKTK merge with above
test_that("the input dataset has no NAs in the grouping column", {
  
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
  dir.create(file.path(tmp_path, "raw_combined"))
  
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
  
  # Replace one value in the detections column with an NA value
  sim_ts$PIT_tag_ID[1] <- NA
  # head(sim_ts)
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  # The function uses original_timestamp to create timestamp_ms before the NA values check
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The column PIT_tag_ID has NA values"
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# TKTK merge with above
test_that("the input dataset has the year, month, and day columns, and that these columns do not have NAs", {
  
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
  dir.create(file.path(tmp_path, "raw_combined"))
  
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
  
  # Drop the year column
  sim_ts2 <- sim_ts[, -grep("year", names(sim_ts))]
  # glimpse(sim_ts2)
  
  write.csv(sim_ts2, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The column year was not found in the data frame"
  )
  
  # Drop the month column
  sim_ts2 <- sim_ts[, -grep("month", names(sim_ts))]
  # glimpse(sim_ts2)
  
  write.csv(sim_ts2, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The column month was not found in the data frame"
  )
  
  # Drop the day column
  sim_ts2 <- sim_ts[, -grep("day", names(sim_ts))]
  # glimpse(sim_ts2)
  
  write.csv(sim_ts2, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  th <- 1
  
  expect_error(
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The column day was not found in the data frame"
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})
