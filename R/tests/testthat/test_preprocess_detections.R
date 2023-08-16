# G. Smith-Vidaurre
# 03 January 2023

# See more info on the testthat package: https://r-pkgs.org/testing-basics.html

# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

if (!require(testthat)) install.packages('testthat')
library(testthat)

preprocess_detections <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/preprocess_detections.R", print = FALSE)$value

# This testing file can be run by calling test_file("./path/to/this/file)

########## Testing output ########## 

# This test will depend on the threshold used
test_that("The correct number of discrete RFID events are retained after pre-processing", {
  
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
  
  # The function returns 1 timestamp per cluster. Is this the filtering that I want, or should there be less strict filtering here??
  preprocess_detections(sensor = "RFID", timestamps_col = "timestamp_ms", group_col_nm = "PIT_tag_ID", pixel_col_nm = NULL, thin_threshold = 1, pixel_threshold = NULL, path = path, data_dir = file.path(data_dir, "raw_combined"), out_dir = file.path(data_dir, "processed"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  
  
})

# TKTK
test_that("The pre-processed events are not closer together than the temporal threshold", {
  
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
  
  threshold <- 1
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = threshold, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    )
  
  # glimpse(res_df)
  
  # Filter the original timestamps for each perching event and calculate the gaps between consecutive timestamps within each perching event
  gaps <- res_df %>% 
    rowid_to_column() %>% 
    dplyr::rename(
      `uniq_event_id` = "rowid" 
    ) %>% 
    dplyr::select(uniq_event_id, start, end) %>% 
    pmap_dfr(., function(uniq_event_id, start, end){
      tmp <- test_df %>%
        dplyr::filter(RFID >= start & RFID <= end) %>%
        dplyr::mutate(
          uniq_event_id = uniq_event_id,
        ) 
      
      return(tmp)
      
    }) %>% 
    group_by(uniq_event_id) %>% 
    # This should be the same lag calculation as in the function itself
    dplyr::mutate(
      shift = dplyr::lag(RFID, default = first(RFID))
    ) %>% 
    dplyr::mutate(
      diff = floor(RFID - shift),
      diff = as.numeric(diff)
    ) %>% 
    dplyr::summarise(
      max = max(diff)
    ) %>% 
    pull(max)
  
  sapply(1:length(gaps), function(i){
    expect_lte(gaps[i], threshold)
  })
  
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
  
  threshold <- 1
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = threshold, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    )
  
  # glimpse(res_df)
  
  # Filter the original timestamps for each perching event and calculate the gaps between consecutive timestamps within each perching event
  gaps <- res_df %>% 
    rowid_to_column() %>% 
    dplyr::rename(
      `uniq_event_id` = "rowid" 
    ) %>% 
    dplyr::select(uniq_event_id, start, end) %>% 
    pmap_dfr(., function(uniq_event_id, start, end){
      tmp <- test_df %>%
        dplyr::filter(RFID >= start & RFID <= end) %>%
        dplyr::mutate(
          uniq_event_id = uniq_event_id,
        ) 
      
      return(tmp)
      
    }) %>% 
    group_by(uniq_event_id) %>% 
    # This should be the same lag calculation as in the function itself
    dplyr::mutate(
      shift = dplyr::lag(RFID, default = first(RFID))
    ) %>% 
    dplyr::mutate(
      diff = floor(RFID - shift),
      diff = as.numeric(diff)
    ) %>% 
    dplyr::summarise(
      max = max(diff)
    ) %>% 
    pull(max)
  
  sapply(1:length(gaps), function(i){
    expect_lte(gaps[i], threshold)
  })
  
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

# TKTK
test_that("The first timestamp of each run of consecutive raw detections is returned as the timestamp for a given pre-processed event", {
  
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
      tag_id = "01-02-03-AA-BB-CC",
      uniq_event_id = c(
        rep("1", length(tstmps_1)),
        rep("2", length(tstmps_2)),
        rep("3", length(tstmps_3))
      )
    ) %>% 
    group_by(uniq_event_id) %>% 
    dplyr::mutate(
      group_row_id = row_number()
    ) %>%
    ungroup()
  
  # glimpse(test_df)
  
  # Make a data frame of the min and max index of timestamps for each dummy perching event
  index_df <- test_df %>% 
    group_by(uniq_event_id) %>% 
    dplyr::summarise(
      min = min(group_row_id),
      max = max(group_row_id)
    )
  
  # glimpse(index_df)
  
  threshold <- 1
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = threshold, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    )
  
  # glimpse(res_df)
  
  # Find the group row indices for the start and end timestamps for each perching event
  comp_df <- res_df %>% 
    rowid_to_column() %>% 
    dplyr::rename(
      `uniq_event_id` = "rowid" 
    ) %>% 
    dplyr::select(uniq_event_id, start, end) %>% 
    pmap_dfr(., function(uniq_event_id, start, end){
      tmp <- test_df %>%
        dplyr::filter(RFID == start | RFID == end) %>%
        dplyr::mutate(
          uniq_event_id = uniq_event_id,
        ) 
      
      return(tmp)
      
    }) %>% 
    group_by(uniq_event_id) %>% 
    dplyr::arrange(-desc(RFID), .by_group = TRUE) %>% 
    dplyr::mutate(
      type = c("min", "max")
    ) %>% 
    ungroup() %>% 
    dplyr::select(uniq_event_id, RFID, group_row_id, type) %>% 
    pivot_wider(
      id_cols = "uniq_event_id",
      names_from = "type",
      values_from = "group_row_id"
    )
  
  expect_equal(index_df[["min"]], comp_df[["min"]])
  expect_equal(index_df[["max"]], comp_df[["max"]])
  
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
      ),
      uniq_event_id = c(
        rep("1", length(tstmps_1)),
        rep("2", length(tstmps_2)),
        rep("3", length(tstmps_3))
      )
    ) %>% 
    group_by(uniq_event_id) %>% 
    dplyr::mutate(
      group_row_id = row_number()
    ) %>%
    ungroup()
  
  # glimpse(test_df)
  
  # Make a data frame of the min and max index of timestamps for each dummy perching event
  index_df <- test_df %>% 
    group_by(uniq_event_id) %>% 
    dplyr::summarise(
      min = min(group_row_id),
      max = max(group_row_id)
    )
  
  # glimpse(index_df)
  
  threshold <- 1
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = threshold, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    )
  
  # glimpse(res_df)
  
  # Find the group row indices for the start and end timestamps for each perching event
  comp_df <- res_df %>% 
    rowid_to_column() %>% 
    dplyr::rename(
      `uniq_event_id` = "rowid" 
    ) %>% 
    dplyr::select(uniq_event_id, start, end) %>% 
    pmap_dfr(., function(uniq_event_id, start, end){
      tmp <- test_df %>%
        dplyr::filter(RFID == start | RFID == end) %>%
        dplyr::mutate(
          uniq_event_id = uniq_event_id,
        ) 
      
      return(tmp)
      
    }) %>% 
    group_by(uniq_event_id) %>% 
    dplyr::arrange(-desc(RFID), .by_group = TRUE) %>% 
    dplyr::mutate(
      type = c("min", "max")
    ) %>% 
    ungroup() %>% 
    dplyr::select(uniq_event_id, RFID, group_row_id, type) %>% 
    pivot_wider(
      id_cols = "uniq_event_id",
      names_from = "type",
      values_from = "group_row_id"
    )
  
  expect_equal(index_df[["min"]], comp_df[["min"]])
  expect_equal(index_df[["max"]], comp_df[["max"]])
  
})

# TKTK
test_that("Pre-processing was performed within a single group specified inside he grouping column", {
  
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
  
  ### A single PIT tag ID
  
  # 3 separate perching events on 3 different days
  # All of these perching events should occur on their single respective day
  start1 <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end1 <- start + 240
  tstmps_1 <- seq(from = start1, by = interval, to = end1)
  
  start2 <- as.POSIXct("2023-01-02 10:00:00 EST")
  interval <- 1
  end2 <- start2 + 200
  tstmps_2 <- seq(from = start2, by = interval, to = end2)
  
  start3 <- as.POSIXct("2023-01-05 15:00:00 EST")
  interval <- 1
  end3 <- start3 + 1000
  tstmps_3 <- seq(from = start3, by = interval, to = end3)
  
  # A fourth perching event that bleeds into another day
  # Given how the function is written (which is based on our data collection and transfer logic in the tracking system), this perching event should be split into 2 days
  start4 <- as.POSIXct("2023-01-08 23:30:30 EST")
  interval <- 1
  end4 <- start4 + 7000
  tstmps_4 <- seq(from = start4, by = interval, to = end4)
  
  # head(tstmps_4)
  # tail(tstmps_4)
  
  test_df <- data.frame(RFID = c(tstmps_1, tstmps_2, tstmps_3, tstmps_4)) %>% 
    dplyr::mutate(
      year = c(year(tstmps_1), year(tstmps_2), year(tstmps_3), year(tstmps_4)),
      month = c(month(tstmps_1), month(tstmps_2), month(tstmps_3), month(tstmps_4)),
      day = c(day(tstmps_1), day(tstmps_2), day(tstmps_3), day(tstmps_4)),
      tag_id = "01-02-03-AA-BB-CC",
      uniq_event_id = c(
        rep("1", length(tstmps_1)),
        rep("2", length(tstmps_2)),
        rep("3", length(tstmps_3)),
        rep("4", length(tstmps_4))
      )
    ) %>% 
    group_by(uniq_event_id) %>% 
    dplyr::mutate(
      group_row_id = row_number()
    ) %>%
    ungroup()
  
  # glimpse(test_df)

  threshold <- 1
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = threshold, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    ) %>% 
    dplyr::mutate(
      start_date = paste(year(start), month(start), day(start), sep = "-"),
      end_date = paste(year(end), month(end), day(end), sep = "-")
    )
  
  # looks great - the last perching event is split up into 2 separate events just before and just after midnight
  # View(res_df)
  # glimpse(res_df)
  
  expect_equal(res_df$start_date, res_df$end_date)
  
  ### Multiple PIT tag IDs
  
  # 3 separate perching events on 3 different days
  # All of these perching events should occur on their single respective day
  start1 <- as.POSIXct("2023-01-01 01:00:00 EST")
  interval <- 1
  end1 <- start + 240
  tstmps_1 <- seq(from = start1, by = interval, to = end1)
  
  start2 <- as.POSIXct("2023-01-02 10:00:00 EST")
  interval <- 1
  end2 <- start2 + 200
  tstmps_2 <- seq(from = start2, by = interval, to = end2)
  
  start3 <- as.POSIXct("2023-01-05 15:00:00 EST")
  interval <- 1
  end3 <- start3 + 1000
  tstmps_3 <- seq(from = start3, by = interval, to = end3)
  
  # A fourth perching event that bleeds into another day
  # Given how the function is written (which is based on our data collection and transfer logic in the tracking system), this perching event should be split into 2 days
  start4 <- as.POSIXct("2023-01-08 23:30:30 EST")
  interval <- 1
  end4 <- start4 + 7000
  tstmps_4 <- seq(from = start4, by = interval, to = end4)
  
  # head(tstmps_4)
  # tail(tstmps_4)
  
  test_df <- data.frame(RFID = c(tstmps_1, tstmps_2, tstmps_3, tstmps_4)) %>% 
    dplyr::mutate(
      year = c(year(tstmps_1), year(tstmps_2), year(tstmps_3), year(tstmps_4)),
      month = c(month(tstmps_1), month(tstmps_2), month(tstmps_3), month(tstmps_4)),
      day = c(day(tstmps_1), day(tstmps_2), day(tstmps_3), day(tstmps_4)),
      tag_id = c(
        rep("01-02-03-AA-BB-CC", length(tstmps_1)),
        rep("01-02-03-XX-YY-ZZ", length(tstmps_2)),
        rep("33-55-99-XX-YY-ZZ", length(tstmps_3)),
        rep("TT-QQ-R6-KK-89-34", length(tstmps_4))
      ),
      uniq_event_id = c(
        rep("1", length(tstmps_1)),
        rep("2", length(tstmps_2)),
        rep("3", length(tstmps_3)),
        rep("4", length(tstmps_4))
      )
    ) %>% 
    group_by(uniq_event_id) %>% 
    dplyr::mutate(
      group_row_id = row_number()
    ) %>%
    ungroup()
  
  # glimpse(test_df)
  
  threshold <- 1
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = threshold, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    ) %>% 
    dplyr::mutate(
      start_date = paste(year(start), month(start), day(start), sep = "-"),
      end_date = paste(year(end), month(end), day(end), sep = "-")
    )
  
  # The last perching event is also split up into 2 separate events just before and just after midnight
  # View(res_df)
  # glimpse(res_df)
  
  expect_equal(res_df$start_date, res_df$end_date)
  
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