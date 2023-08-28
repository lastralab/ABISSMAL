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

# As of right now, this function retains only 1 detection per cluster. Is this the behavior I want? Because if not, then I need to update the function to retain all detections per cluster separated by more than the threshold. This needs to be a sequence starting from the first detection. Maybe these are 2 modes of pre-processing

# This test will depend on the threshold used
test_that("The correct number and timing of discrete movement events are retained per pre-processing mode for a single temporal threshold and run length", {
  
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
  
  source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/preprocess_detections.R", print = FALSE)
  
  th <- 1
  
  # `retain_first` mode
  preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = th, mode = "retain_first", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "%Y-%m-%d %H:%M:%OS", POSIXct_format = "%Y-%m-%d %H:%M:%OS")

  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv")) %>% 
  # Make sure the timestamps are in the right format
    dplyr::mutate(
      !!timestamps_col := as.POSIXct(format(as.POSIXct(!!sym(timestamps_col), tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
    )
  
  glimpse(test_res)
  class(test_res[[timestamps_col]])
  
  # Test that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # TKTK Test that each detection cluster is separated by more than the given temporal threshold
  diffs <- test_res$timestamp_ms - lag(test_res$timestamp_ms)
  diffs <- diffs[!is.na(diffs)]
  
  expect_true(all(diffs > th))
  
  # TKTK Test that the first timestamp from the raw data is returned as the timestamp per detection cluster
  expect_equal(nrow(test_res), length(starts))
  
  # TKTK Pre-processing was performed within a single group specified inside the grouping column
  # TKTK
  
  # `thin` mode
  preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = 1, mode = "thin", pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "processed", "pre_processed_data_RFID.csv"))
  
  # Test that the results contain the expected number of detection clusters
  expect_equal(nrow(test_res), length(starts))
  
  # TKTK Test that each detection cluster is separated by more than the given temporal threshold, as well as timestamps within each cluster
  expect_equal(nrow(test_res), length(starts))
  
  # TKTK Test that the every other timestamp from the raw data is returned as the timestamp per detection cluster
  expect_equal(nrow(test_res), length(starts))
  
  # TKTK Pre-processing was performed within a single group specified inside the grouping column
  # TKTK
  
})


# TKTK
test_that("An empty data frame is returned when the given threshold drops all events after pre-processing", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("tidyquant")
  withr::local_package("Rmisc")
  
  ### A single PIT tag ID
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
  
  threshold <- 0.1
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = threshold, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    )
  
  expect_equal(res_df$start, NA)
  expect_equal(res_df$end, NA)
  expect_equal(res_df$duration_seconds, NA)
  expect_equal(res_df$event_type, NA)
  
  ### Multiple PIT tag IDs
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
      tag_id = c(
        rep("01-02-03-AA-BB-CC", length(tstmps_1)),
        rep("01-02-03-XX-YY-ZZ", length(tstmps_2)),
        rep("33-55-99-XX-YY-ZZ", length(tstmps_3))
      )
    ) 
  
  threshold <- 0.1
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = threshold, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    )
  
  expect_equal(unique(res_df$start), NA)
  expect_equal(unique(res_df$end), NA)
  expect_equal(unique(res_df$duration_seconds), NA)
  expect_equal(unique(res_df$event_type), NA)
  
})

# TKTK repeat these tests over different thresholds and run lengths
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