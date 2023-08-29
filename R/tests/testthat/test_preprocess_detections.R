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
  preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "%Y-%m-%d %H:%M:%OS", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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
  preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "thin", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "%Y-%m-%d %H:%M:%OS", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
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
  
  # Create 4 clusters of detections: each cluster consists of a different number of detections (testing `run_length`) spaced different numbers of seconds apart (testing `threshold`)
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
    
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "%Y-%m-%d %H:%M:%OS", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
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
    preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = ths[x], mode = "thin", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "%Y-%m-%d %H:%M:%OS", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
    
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

########## Testing error messages ########## 

test_that("the raw data is a data frame", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  withr::local_package("Rmisc")
  
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
  
  test_df <- data.frame(timestamps = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
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
    preprocess_detections(df = df, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2),
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
  withr::local_package("tidyquant")
  withr::local_package("Rmisc")
  
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
  
  test_df <- data.frame(timestamps = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
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
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = "2")
      ),
    regexp = 'The temporal threshold needs to be numeric'
  )
  
})


test_that("the input dataset has the detections column", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  withr::local_package("Rmisc")
  
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
  
  test_df <- data.frame(timestamps = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  # Drop the detections column
  test_df2 <- test_df[, -grep("timestamps", names(test_df))]
  # glimpse(test_df2)
  
  expect_error(
    test_df2 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'The column specified in `detection_col_nm` does not exist'
  )
  
})


test_that("the input dataset has no NAs in the detections column", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  withr::local_package("Rmisc")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(tidyquant)
  # library(Rmisc)
  
  start <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end <- start + 240
  
  # 3 separate perching events (e.g. stretches of detections 1 second apart) separated by gaps of 1 minute 
  tstmps_1 <- seq(from = start, by = interval, to = end)
  tstmps_2 <- seq(from = (end + 60), by = interval, to = end + 120)
  tstmps_3 <- seq(from = (end + 120 + 60), by = interval, to = end + 240)
  
  test_df <- data.frame(timestamps = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  # Replace one value in the detections column with an NA value
  test_df$timestamps[1] <- NA
  # head(test_df)
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'One or more timestamps are in the wrong format. These need to be in POSIXct or POSIXt format, like %Y-%m-%d %H:%M:%OS6'
  )
  
})


test_that("the input dataset has the grouping column", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  withr::local_package("Rmisc")
  
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
  
  test_df <- data.frame(timestamps = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  # Drop the grouping column
  test_df2 <- test_df[, -grep("tag_id", names(test_df))]
  # glimpse(test_df2)
  
  expect_error(
    test_df2 %>%
      group_split(month) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'The column specified in `group_col_nm` does not exist'
  )
  
})


test_that("the input dataset has no NAs in the grouping column", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  withr::local_package("Rmisc")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(tidyquant)
  # library(Rmisc)
  
  start <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end <- start + 240
  
  # 3 separate perching events (e.g. stretches of detections 1 second apart) separated by gaps of 1 minute 
  tstmps_1 <- seq(from = start, by = interval, to = end)
  tstmps_2 <- seq(from = (end + 60), by = interval, to = end + 120)
  tstmps_3 <- seq(from = (end + 120 + 60), by = interval, to = end + 240)
  
  test_df <- data.frame(timestamps = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  # Replace one value in the PIT tag ID column with an NA value
  test_df$tag_id[1] <- NA
  # head(test_df)
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'The column specified in `group_col_nm` has NA values'
  )
  
})


test_that("the input dataset has the year, month, and day columns, and that these columns do not have NAs", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  withr::local_package("Rmisc")
  
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
  
  test_df <- data.frame(timestamps = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
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
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  expect_error(
    test_df3 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  expect_error(
    test_df4 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  # Replace one value in each of the year, month, and day columns with an NA value
  test_df$year[1] <- NA
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  test_df$year[1] <- "2023"
  test_df$month[1] <- NA
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'The data frame is missing columns `year`, `month`, or `day`, or there are NA values'
  )
  
  test_df$month[1] <- "01"
  test_df$day[1] <- NA
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
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
  withr::local_package("tidyquant")
  withr::local_package("Rmisc")
  
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
  
  test_df <- data.frame(timestamps = c(tstmps_1, tstmps_2, tstmps_3)) %>% 
    dplyr::mutate(
      year = year(start),
      month = month(start),
      day = day(start),
      tag_id = "01-02-03-AA-BB-CC"
    ) 
  
  # Convert timestamps to character format
  test_df2 <- test_df %>% 
    dplyr::mutate(
      timestamps = as.character(timestamps)
    )
  # glimpse(test_df2)
  
  expect_error(
    test_df2 %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'One or more timestamps are in the wrong format'
  )
  
  # Replace one value in the timestamps column with an NA value
  test_df$timestamps[1] <- NA
  # head(test_df)
  
  expect_error(
    test_df %>%
      group_split(tag_id) %>%
      map_dfr(
        ~ preprocess_detections(df = .x, detection_col_nm = "timestamps", group_col_nm = "tag_id", threshold = 2)
      ),
    regexp = 'One or more timestamps are in the wrong format'
  )
  
  
})