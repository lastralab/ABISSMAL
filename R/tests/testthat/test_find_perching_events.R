# G. Smith-Vidaurre
# 03 January 2023

# See more info on the testthat package: https://r-pkgs.org/testing-basics.html

# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

if (!require(testthat)) install.packages('testthat')
library(testthat)

find_perching_events <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_perching_events.R")$value

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

########## Testing output ########## 

# A single temporal threshold and run length, RFID data
test_that("The correct number and timing of perching events are identified for RFID data", {
  
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
  
  find_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = th, run_length = run_length, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_nm = "perching_events.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

# Multiple temporal thresholds and run lengths, RFID data
test_that("The correct number and timing of discrete perching events are identified for RFID data", {
  
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
  
  # threshold values are x, run length values are y
  invisible(mapply(function(x, y){

    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], starts[i] + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
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
    
    find_perching_events(file_nm = "combined_raw_data_RFID.csv", threshold = x, run_length = y, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = "PIT_tag_ID", rfid_label = "RFID", outer_irbb_label = NULL, inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_nm = "perching_events.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
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
      
      expect_equal(test_res$perching_start[i], tmp_starts[i])
      expect_equal(test_res$perching_end[i], tmp_end)
      
    }))
    
    # Check that the duration of each perching event is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_duration_s[i], as.numeric(tmp_end - tmp_starts[i]))
      
    }))
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# A single temporal threshold and run length, outer beam breaker data
test_that("The correct number and timing of perching events are identified for the outer beam breaker pair", {
  
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
  
  source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_perching_events.R")
  
  find_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = th, run_length = run_length, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_nm = "perching_events.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

# Multiple temporal thresholds and run lengths, outer beam breaker data
test_that("The correct number and timing of discrete perching events are identified for the outer beam breaker pair", {
  
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
  
  # threshold values are x, run length values are y
  invisible(mapply(function(x, y){
    
    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], starts[i] + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
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
    
    find_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = x, run_length = y, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = NULL, general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_nm = "perching_events.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
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
      
      expect_equal(test_res$perching_start[i], tmp_starts[i])
      expect_equal(test_res$perching_end[i], tmp_end)
      
    }))
    
    # Check that the duration of each perching event is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_duration_s[i], as.numeric(tmp_end - tmp_starts[i]))
      
    }))
    
    # Check that 4 perching events were identified for this sensor
    expect_equal(test_res$sensor_id, rep("Outer Beam Breaker", length(starts)))
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# A single temporal threshold and run length, inner beam breaker data
test_that("The correct number and timing of perching events are identified for the inner beam breaker pair", {
  
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
  
  find_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = th, run_length = run_length, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = NULL, rfid_label = NULL, outer_irbb_label = NULL, inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_nm = "perching_events.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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

# Multiple temporal thresholds and run lengths, inner beam breaker data
test_that("The correct number and timing of discrete perching events are identified for the inner beam breaker pair", {
  
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
  
  # threshold values are x, run length values are y
  invisible(mapply(function(x, y){
    
    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], starts[i] + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
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
    
    find_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = x, run_length = y, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = NULL, rfid_label = NULL, outer_irbb_label = NULL, inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_nm = "perching_events.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
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
      
      expect_equal(test_res$perching_start[i], tmp_starts[i])
      expect_equal(test_res$perching_end[i], tmp_end)
      
    }))
    
    # Check that the duration of each perching event is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_duration_s[i], as.numeric(tmp_end - tmp_starts[i]))
      
    }))
    
    # Check that 4 perching events were identified for this sensor
    expect_equal(test_res$sensor_id, rep("Inner Beam Breaker", length(starts)))
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# A single temporal threshold and run length, beam breaker data (two pairs)
test_that("The correct number and timing of perching events are identified for 2 pairs of beam breakers", {
  
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
  
  find_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = th, run_length = run_length, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_nm = "perching_events.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "perching_events_IRBB.csv")) %>% 
    # Make sure the timestamps are in the right format
    dplyr::mutate(
      perching_start = as.POSIXct(format(as.POSIXct(perching_start, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
      perching_end = as.POSIXct(format(as.POSIXct(perching_end, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  glimpse(test_res)
  
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

# Multiple temporal thresholds and run lengths, beam breaker data (two pairs)
test_that("The correct number and timing of discrete perching events are identified for 2 pairs of beam breakers", {
  
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
  
  # threshold values are x, run length values are y
  invisible(mapply(function(x, y){
    
    # Create the timestamps
    tmp_tstmps <- data.table::rbindlist(lapply(1:length(starts), function(i){
      
      sim_ts <- seq(starts[i], starts[i] + max(seq_len(y*y)), by = x)[1:(y + 1)]
      
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
    
    source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_perching_events.R")
    
    find_perching_events(file_nm = "combined_raw_data_IRBB.csv", threshold = x, run_length = y, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = NULL, rfid_label = NULL, outer_irbb_label = "Outer Beam Breaker", inner_irbb_label = "Inner Beam Breaker", general_metadata_cols = c("chamber_id", "sensor_id"), path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), out_file_nm = "perching_events.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
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
      
      expect_equal(test_res$perching_start[i], tmp_starts[i])
      expect_equal(test_res$perching_end[i], tmp_end)
      
    }))
    
    # Check that the duration of each perching event is correct
    invisible(lapply(1:nrow(test_res), function(i){
      
      # Get the expected end timestamp per simulated perching event cluster
      tmp_end <- max(tmp_tstmps$tstmps[tmp_tstmps$cluster == i])
      
      expect_equal(test_res$perching_duration_s[i], as.numeric(tmp_end - tmp_starts[i]))
      
    }))
    
    # Check that 2 perching events were identified per sensor
    expect_equal(test_res$sensor_id, c(rep("Outer Beam Breaker", 2), rep("Inner Beam Breaker", 2)))
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})



########## Testing error messages ##########

test_that("the raw data is a data frame", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  
  start <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end <- start + 240
  
  # 3 separate perching events (e.g. stretches of detections 1 second apart) separated by gaps of 1 minute 
  tstmps_1 <- seq(from = start, by = interval, to = end)
  tstmps_2 <- seq(from = (end + 60), by = interval, to = end + 120)
  tstmps_3 <- seq(from = (end + 120 + 60), by = interval, to = end + 240)
  
  test_df <- data.frame(RFID = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  df <- list(test_df)
  # glimpse(df)
  # class(df)
  
  expect_error(
    find_rfid_perching_events(df = df, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2),
    regexp = 'The input object needs to be a data frame'
  )
  
})


test_that("the temporal threshold is a number", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  
  start <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end <- start + 240
  
  # 3 separate perching events (e.g. stretches of detections 1 second apart) separated by gaps of 1 minute 
  tstmps_1 <- seq(from = start, by = interval, to = end)
  tstmps_2 <- seq(from = (end + 60), by = interval, to = end + 120)
  tstmps_3 <- seq(from = (end + 120 + 60), by = interval, to = end + 240)
  
  test_df <- data.frame(RFID = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = "1", rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'The temporal threshold needs to be numeric'
  )
  
})


test_that("the input dataset has the column of RFID events", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  
  start <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end <- start + 240
  
  # 3 separate perching events (e.g. stretches of detections 1 second apart) separated by gaps of 1 minute 
  tstmps_1 <- seq(from = start, by = interval, to = end)
  tstmps_2 <- seq(from = (end + 60), by = interval, to = end + 120)
  tstmps_3 <- seq(from = (end + 120 + 60), by = interval, to = end + 240)
  
  test_df <- data.frame(RFID = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  # Drop the RFID column
  test_df2 <- test_df[, -grep("RFID", names(test_df))]
  # glimpse(test_df2)
  
  expect_error(
    test_df2 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'The column specified in `rfid_col_nm` does not exist'
  )
  
})


test_that("the input dataset has the column of PIT tag IDs, and this column does not have NAs", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  library(tidyverse)
  library(lubridate)
  
  start <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end <- start + 240
  
  # 3 separate perching events (e.g. stretches of detections 1 second apart) separated by gaps of 1 minute 
  tstmps_1 <- seq(from = start, by = interval, to = end)
  tstmps_2 <- seq(from = (end + 60), by = interval, to = end + 120)
  tstmps_3 <- seq(from = (end + 120 + 60), by = interval, to = end + 240)
  
  test_df <- data.frame(RFID = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  # Drop the PIT tag ID column
  test_df2 <- test_df[, -grep("tag_id", names(test_df))]
  # glimpse(test_df2)
  
  expect_error(
    test_df2 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2),
        regexp = 'The column specified in `tag_id_col_nm` does not exist'
      )
  )
  
  # Replace one value in the PIT tag ID column with an NA value
  test_df$tag_id[1] <- NA
  # head(test_df)
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'The column specified in `tag_id_col_nm` does not exist or has NA'
  )
  
})


test_that("the input dataset has the year, month, and day columns, and that these columns do not have NAs", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  library(tidyverse)
  library(lubridate)
  
  start <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end <- start + 240
  
  # 3 separate perching events (e.g. stretches of detections 1 second apart) separated by gaps of 1 minute 
  tstmps_1 <- seq(from = start, by = interval, to = end)
  tstmps_2 <- seq(from = (end + 60), by = interval, to = end + 120)
  tstmps_3 <- seq(from = (end + 120 + 60), by = interval, to = end + 240)
  
  test_df <- data.frame(RFID = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  # Drop the year, month, and day columns one by one
  test_df2 <- test_df[, -grep("year", names(test_df))]
  # glimpse(test_df2)
  
  test_df3 <- test_df[, -grep("month", names(test_df))]
  # glimpse(test_df2)
  
  test_df4 <- test_df[, -grep("day", names(test_df))]
  # glimpse(test_df2)
  
  expect_error(
    test_df2 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  expect_error(
    test_df3 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  expect_error(
    test_df4 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  # Replace one value in each of the year, month, and day columns with an NA value
  test_df$year[1] <- NA
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  test_df$year[1] <- "2023"
  test_df$month[1] <- NA
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  test_df$month[1] <- "01"
  test_df$day[1] <- NA
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
})


test_that("the timestamps are in the right format", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  
  start <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end <- start + 240
  
  # 3 separate perching events (e.g. stretches of detections 1 second apart) separated by gaps of 1 minute 
  tstmps_1 <- seq(from = start, by = interval, to = end)
  tstmps_2 <- seq(from = (end + 60), by = interval, to = end + 120)
  tstmps_3 <- seq(from = (end + 120 + 60), by = interval, to = end + 240)
  
  test_df <- data.frame(RFID = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  # Convert timestamps to character format
  test_df2 <- test_df %>% 
    dplyr::mutate(
      RFID = as.character(RFID)
    )
  # glimpse(test_df2)
  
  expect_error(
    test_df2 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'One or more timestamps are in the wrong format'
  )
  
  # Replace one value in the RFID column with an NA value
  test_df$RFID[1] <- NA
  # head(test_df)
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
      ),
    regexp = 'One or more timestamps are in the wrong format'
  )
  
  
})
