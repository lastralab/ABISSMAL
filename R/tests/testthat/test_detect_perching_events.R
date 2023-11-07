# G. Smith-Vidaurre
# 03 January 2023

rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/detect_perching_events.R")

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

########## Testing output ########## 

test_that("The correct number and timing of perching events are identified for RFID data with a single temporal threshold and run length", {
  
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Generate a file with raw timestamps for one sensor
  
  # Create 4 clusters of detections: each cluster consists of a given number of detections (1 `run_length` value) spaced a given number of seconds apart (1 `threshold` value)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  # Set a single value each for the temporal threshold and run_length  
  th <- 1
  run_length <- 2
  
  # Each cluster will contain detections spaced by less than the temporal threshold
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
  
  detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = run_length, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "perching_events_RFID.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # Check that the start and end timestamps of each perching event are correct
  tmp_starts <- starts[order(starts)]
  tmp_ends <- ends[order(ends)]
  
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$perching_start[x], tmp_starts[x])
    expect_equal(test_res$perching_end[x], tmp_ends[x])
    
  }))
  
  # Check that the duration of each perching event is correct
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$perching_duration_s[x], as.numeric(tmp_ends[x] - tmp_starts[x]))
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete perching events are identified for RFID data with multiple temporal thresholds and run lengths", {
  
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  ths <- seq(0.5, 5, by = 0.5)
  
  # Since run lengths are calculated from the difference in timestamps, each run_length value should be applied to a clusters of detections of length rls + 1
  rls <- seq(1, 10, by = 1)
  
  # The threshold and run length vectors must have the same number of values
  invisible(lapply(1:length(ths), function(x){
    
    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], starts[i] + max(seq_len(rls[x]*rls[x])), by = ths[x])[1:(rls[x] + 1)]
      
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
    
    detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = ths[x], run_length = rls[x], sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "perching_events_RFID.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
        perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Check that the results contain the expected number of detection clusters
    expect_equal(nrow(test_res), length(starts))
    
    # Check that the start and end timestamps of each perching event are correct
    invisible(lapply(1:nrow(test_res), function(i){

      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])

      expect_equal(test_res$perching_start[i], starts[i])
      expect_equal(test_res$perching_end[i], tmp_end)

    }))

    # Check that the duration of each perching event is correct
    invisible(lapply(1:nrow(test_res), function(i){

      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])

      expect_equal(test_res$perching_duration_s[i], as.numeric(tmp_end - starts[i]))

    }))
  
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of perching events are identified for the outer beam breaker pair with a single temporal threshold and run length", {
  
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Generate a file with raw timestamps for 1 beam breaker sensor
  
  # Create 4 clusters of detections: each cluster consists of a given number of detections (1 `run_length` value) spaced a given number of seconds apart (1 `threshold` value). Here this will be 2 perching events for each sensor
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  # Set a single value each for the temporal threshold and run_length  
  th <- 1
  run_length <- 2
  
  # Each cluster will contain detections spaced by less than the temporal threshold
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
      sensor_id = "Outer Beam Breaker",
      data_type = "IRBB",
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
  
  detect_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = th, run_length = run_length, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "perching_events_IRBB.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )

  # Check that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # Check that the start and end timestamps of each perching event are correct
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$perching_start[x], starts[x])
    expect_equal(test_res$perching_end[x], ends[x])
    
  }))
  
  # Check that the duration of each perching event is correct
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$perching_duration_s[x], as.numeric(ends[x] - starts[x]))
    
  }))
  
  # Check that 4 perching events were identified per sensor
  expect_equal(test_res$sensor_id, rep("Outer Beam Breaker", length(starts)))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete perching events are identified for the outer beam breaker pair with multiple temporal thresholds and run lengths", {
  
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  ths <- seq(0.5, 5, by = 0.5)
  
  # Since run lengths are calculated from the difference in timestamps, each run_length value should be applied to a clusters of detections of length rls + 1
  rls <- seq(1, 10, by = 1)
  
  # The threshold and run length vectors must have the same number of values
  invisible(lapply(1:length(ths), function(x){
    
    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], starts[i] + max(seq_len(rls[x]*rls[x])), by = ths[x])[1:(rls[x] + 1)]
      
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
        sensor_id = "Outer Beam Breaker",
        data_type = "IRBB",
        data_stage = "raw_combined",
        date_combined = Sys.Date()
      )
    
    write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
    
    detect_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = ths[x], run_length = rls[x], sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "perching_events_IRBB.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
        perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Check that the results contain the expected number of detection clusters
    expect_equal(nrow(test_res), length(starts))
    
    # Check that the start and end timestamps of each perching event are correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_start[i], starts[i])
      expect_equal(test_res$perching_end[i], tmp_end)
      
    }))
    
    # Check that the duration of each perching event is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_duration_s[i], as.numeric(tmp_end - starts[i]))
      
    }))
    
    # Check that 4 perching events were identified for this sensor
    expect_equal(test_res$sensor_id, rep("Outer Beam Breaker", length(starts)))
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of perching events are identified for the inner beam breaker pair with a single temporal threshold and run length", {
  
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Generate a file with raw timestamps for 1 beam breaker sensor
  
  # Create 4 clusters of detections: each cluster consists of a given number of detections (1 `run_length` value) spaced a given number of seconds apart (1 `threshold` value). Here this will be 2 perching events for each sensor
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  # Set a single value each for the temporal threshold and run_length  
  th <- 1
  run_length <- 2
  
  # Each cluster will contain detections spaced by less than the temporal threshold
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
      sensor_id = "Inner Beam Breaker",
      data_type = "IRBB",
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
  
  detect_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = th, run_length = run_length, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "perching_events_IRBB.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # Check that the start and end timestamps of each perching event are correct
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$perching_start[x], starts[x])
    expect_equal(test_res$perching_end[x], ends[x])
    
  }))
  
  # Check that the duration of each perching event is correct
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$perching_duration_s[x], as.numeric(ends[x] - starts[x]))
    
  }))
  
  # Check that 4 perching events were identified per sensor
  expect_equal(test_res$sensor_id, rep("Inner Beam Breaker", length(starts)))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete perching events are identified for the inner beam breaker pair with multiple temporal thresholds and run lengths", {
  
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  ths <- seq(0.5, 5, by = 0.5)
  
  # Since run lengths are calculated from the difference in timestamps, each run_length value should be applied to a clusters of detections of length rls + 1
  rls <- seq(1, 10, by = 1)
  
  # The threshold and run length vectors must have the same number of values
  invisible(lapply(1:length(ths), function(x){
    
    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], starts[i] + max(seq_len(rls[x]*rls[x])), by = ths[x])[1:(rls[x] + 1)]
      
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
        sensor_id = "Inner Beam Breaker",
        data_type = "IRBB",
        data_stage = "raw_combined",
        date_combined = Sys.Date()
      )
    
    write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
    
    detect_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = ths[x], run_length = rls[x], sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "perching_events_IRBB.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
        perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Check that the results contain the expected number of detection clusters
    expect_equal(nrow(test_res), length(starts))
    
    # Check that the start and end timestamps of each perching event are correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_start[i], starts[i])
      expect_equal(test_res$perching_end[i], tmp_end)
      
    }))
    
    # Check that the duration of each perching event is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_duration_s[i], as.numeric(tmp_end - starts[i]))
      
    }))
    
    # Check that 4 perching events were identified for this sensor
    expect_equal(test_res$sensor_id, rep("Inner Beam Breaker", length(starts)))
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of perching events are identified for 2 pairs of beam breakers with a single temporal threshold and run length", {
  
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Generate a file with raw timestamps for 2 beam breaker sensors
  
  # Create 4 clusters of detections: each cluster consists of a given number of detections (1 `run_length` value) spaced a given number of seconds apart (1 `threshold` value). Here this will be 2 perching events for each sensor
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 10
  
  # Set a single value each for the temporal threshold and run_length  
  th <- 1
  run_length <- 2
  
  # Each cluster will contain detections spaced by less than the temporal threshold
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
      sensor_id = c(rep("Outer Beam Breaker", length(event_ts[[1]]) + length(event_ts[[2]])), rep("Inner Beam Breaker", length(event_ts[[3]]) + length(event_ts[[4]]))),
      data_type = "IRBB",
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
  
  detect_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = th, run_length = run_length, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "perching_events_IRBB.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  # Check that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # Check that the start and end timestamps of each perching event are correct
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$perching_start[x], starts[x])
    expect_equal(test_res$perching_end[x], ends[x])
    
  }))
  
  # Check that the duration of each perching event is correct
  invisible(lapply(1:nrow(test_res), function(x){
    
    expect_equal(test_res$perching_duration_s[x], as.numeric(ends[x] - starts[x]))
    
  }))
  
  # Check that 2 perching events were identified per sensor
  expect_equal(test_res$sensor_id, c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("The correct number and timing of discrete perching events are identified for 2 pairs of beam breakers with multiple temporal thresholds and run lengths", {
  
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
  if(!dir.exists(file.path(tmp_path, "raw_combined"))){ 
    dir.create(file.path(tmp_path, "raw_combined"))
  }
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
  ths <- seq(0.5, 5, by = 0.5)
  
  # Since run lengths are calculated from the difference in timestamps, each run_length value should be applied to a clusters of detections of length rls + 1
  rls <- seq(1, 10, by = 1)
  
  # The threshold and run length vectors must have the same number of values
  invisible(lapply(1:length(ths), function(x){
    
    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], starts[i] + max(seq_len(rls[x]*rls[x])), by = ths[x])[1:(rls[x] + 1)]
      
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
        sensor_id = c(rep("Outer Beam Breaker", nrow(tmp_tstmps)/2), rep("Inner Beam Breaker", nrow(tmp_tstmps)/2)),
        data_type = "IRBB",
        data_stage = "raw_combined",
        date_combined = Sys.Date()
      )
    
    write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
    
    detect_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = ths[x], run_length = rls[x], sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
    # Read in the output, check the output, then delete all files
    test_res <- read.csv(file.path(tmp_path, "processed", "perching_events_IRBB.csv")) %>% 
      # Make sure the timestamps are in the right format
      dplyr::mutate(
        perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
        perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
      )
    
    # Check that the results contain the expected number of detection clusters
    expect_equal(nrow(test_res), length(starts))
    
    # Check that the start and end timestamps of each perching event are correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_start[i], starts[i])
      expect_equal(test_res$perching_end[i], tmp_end)
      
    }))
    
    # Check that the duration of each perching event is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_duration_s[i], as.numeric(tmp_end - starts[i]))
      
    }))
    
    # Check that 2 perching events were identified per sensor
    expect_equal(test_res$sensor_id, c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)))
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})


########## Testing error handling ##########

test_that("the function catches when the input file name does not contain the pattern 'RFID' or 'IRBB'", {
  
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
  rl <- 2
  
  expect_error(
    detect_perching_events(file_nm = "combined_raw_data_RFD.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The input file name does not contain the correct sensor suffix"
  )
  
  expect_error(
    detect_perching_events(file_nm = "combined_raw_data_irbb.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Outer Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "The input file name does not contain the correct sensor suffix"
  )
  
  expect_error(
    detect_perching_events(file_nm = "combined_raw_data.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  rl <- 2
  
  # General arguments that cannot ever be NULL:
  arg_nms <- c("file_nm", "sensor_id_col_nm", "timestamps_col_nm", "general_metadata_cols", "path", "data_dir", "out_dir", "out_file_prefix", "tz", "POSIXct_format")
  
  args <- list(
    `file_nm` = "combined_raw_data_RFID.csv",
    `sensor_id_col_nm` = "sensor_id",
    `timestamps_col_nm` = "timestamp_ms",
    `general_metadata_cols` = c("chamber_id"),
    `path` = path,
    `data_dir` = file.path(data_dir, "raw_combined"),
    `out_dir` = file.path(data_dir, "processed"),
    `out_file_prefix` = "perching_events",
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )
  
  
  # i <- 1
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      detect_perching_events(file_nm = args[["file_nm"]], threshold = th, run_length = rl, sensor_id_col_nm = args[["sensor_id_col_nm"]], timestamps_col_nm = args[["timestamps_col_nm"]], PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = args[["general_metadata_cols"]], path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], out_file_prefix = args[["out_file_prefix"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # Arguments that cannot be NULL depending on which sensor is specified:
  
  # When the input file name contains the pattern "RFID", the arguments "PIT_tag_col_nm" and "rfid_label" cannot be NULL
  arg_nms <- c("PIT_tag_col_nm", "rfid_label")
  
  args <- list(
    `PIT_tag_col_nm` = "PIT_tag_ID",
    `rfid_label` = "RFID"
  )

  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
  
    expect_error(
      detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = args[["rfid_label"]], outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # When the input file name contains the pattern "RFID", the arguments "outer_irbb_label", "inner_irbb_label" cannot be NULL
  
  sim_ts <- data.frame(timestamp_ms = c(event_ts[[1]], event_ts[[2]], event_ts[[3]], event_ts[[4]])) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = year(timestamp_ms),
      month = month(timestamp_ms),
      day = day(timestamp_ms),
      original_timestamp = gsub(" EST", "" , gsub("2023-01-01 ", "", timestamp_ms)),
      sensor_id = c(rep("Outer Beam Breaker", length(event_ts[[1]]) + length(event_ts[[2]])), rep("Inner Beam Breaker", length(event_ts[[3]]) + length(event_ts[[4]]))),
      data_type = "IRBB",
      data_stage = "raw_combined",
      date_combined = Sys.Date()
    )
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
  
  arg_nms <- c("outer_irbb_label", "inner_irbb_label")
  
  args <- list(
    `outer_irbb_label` = "Outer Beam Breaker",
    `inner_irbb_label` = "Inner Beam Breaker"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)
    
    expect_error(
      detect_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = args[["outer_irbb_label"]], inner_irbb_label = args[["inner_irbb_label"]], general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  rl <- 2
  
  # Arguments that must be NULL when RFID data is used as input
  arg_nms <- c("outer_irbb_label", "inner_irbb_label")
  
  args <- list(
    `outer_irbb_label` = NULL,
    `inner_irbb_label` = NULL
  )

  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- "irbb"
    
    expect_error(
      detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = args[["outer_irbb_label"]], inner_irbb_label = args[["inner_irbb_label"]], general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a NULL value but the argument", arg_nms[i], "is not NULL", sep = " ")
    )
    
  }))
  
  # Arguments that must be NULL when IRBB data is used as input
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"), row.names = FALSE)
  
  arg_nms <- c("PIT_tag_col_nm", "rfid_label")
  
  args <- list(
    `PIT_tag_col_nm` = NULL,
    `rfid_label` = NULL
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- "test"
    
    expect_error(
      detect_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = args[["rfid_label"]], outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  rl <- 2
  
  # Arguments that must always be strings (file_nm is checked in a different way above)
  arg_nms <- c("sensor_id_col_nm", "timestamps_col_nm", "general_metadata_cols", "path", "data_dir", "out_dir", "out_file_prefix", "tz", "POSIXct_format")
  
  args <- list(
    `sensor_id_col_nm` = "sensor_id",
    `timestamps_col_nm` = "timestamp_ms",
    `general_metadata_cols` = c("chamber_id"),
    `path` = path,
    `data_dir` = file.path(data_dir, "raw_combined"),
    `out_dir` = file.path(data_dir, "processed"),
    `out_file_prefix` = "perching_events",
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = rl, sensor_id_col_nm = args[["sensor_id_col_nm"]], timestamps_col_nm = args[["timestamps_col_nm"]], PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = args[["general_metadata_cols"]], path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], out_file_prefix = args[["out_file_prefix"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )
    
  }))
  
  # Arguments that must be strings when RFID data is used as input
  arg_nms <- c("PIT_tag_col_nm", "rfid_label")
  
  args <- list(
    `PIT_tag_col_nm` = "PIT_tag_ID",
    `rfid_label` = "RFID"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = args[["PIT_tag_col_nm"]], rfid_label = args[["rfid_label"]], outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )
    
  }))
  
  # Arguments that must be strings when IRBB data is used as input
  arg_nms <- c("outer_irbb_label", "inner_irbb_label")
  
  args <- list(
    `outer_irbb_label` = "Outer Beam Breaker",
    `inner_irbb_label` = "Inner Beam Breaker"
  )

  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      detect_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = NULL, rfid_label = NULL, outer_irbb_label = args[["outer_irbb_label"]], inner_irbb_label = args[["inner_irbb_label"]], general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  # 
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
  rl <- 2
  
  expect_error(
    detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = as.character(th), run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "Expected a numeric value but the argument threshold is not numeric"
  )
  
  expect_error(
    detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = as.character(rl), sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "Expected a numeric value but the argument run_length is not numeric"
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
  
  th <- 1
  rl <- 2
  
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  # Remove this file
  file.remove(file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"))
  
  expect_error(
    detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The file combined_raw_data_RFID.csv does not exist in the directory", file.path(tmp_path, "raw_combined"), sep = " ")
  )
  
  # Generate the file again, then remove the whole directory
  write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)
  
  # Remove the directory where this file is located
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
  expect_error(
    detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
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
  rl <- 2
  
  # Columns that must always be present in the raw data. All of these columns are used to make the timestamp_ms column
  col_nms <- c("sensor_id", "original_timestamp", "year", "month", "day", "chamber_id") # chmaber_id is from general_metadata_cols
  
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
    
    sim_ts <- sim_ts[, -grep(col_nms[i], names(sim_ts))]
    
    write.csv(sim_ts, file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"), row.names = FALSE)

    expect_error(
      detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("The column", col_nms[i], "was not found in the data frame", sep = " ")
    )
    
  }))
  
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
  rl <- 2
  
  # Columns that cannot ever have NAs in the raw data. Several of these columns are used to make the timestamp_ms column
  col_nms <- c("sensor_id", "original_timestamp", "year", "month", "day")
  
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
      detect_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = rl, sensor_id_col_nm = "sensor_id", timestamps_col_nm = "timestamp_ms", PIT_tag_col_nm = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_prefix = "perching_events", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
      regexp = paste("The column", col_nms[i], "has NA values", sep = " ")
    )
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})
