# Use the raw radio frequency identification (RFID) data to identify perching events (e.g. periods of time when an individual was perched on the RFID antenna). This function is performed for each unique passive integrated transponder (PIT) tag in the dataset. The function identifies runs of RFID detections separated by the given temporal threshold or less, then takes the first and last detection of each run and return these timestamps with start and end labels. Note that unlike the RFID pre-processing function, this function groups the data frame not only by PIT tag ID but also by date to avoid artificially long perching periods

# See more info on the testthat package: https://r-pkgs.org/testing-basics.html

# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

if (!require(testthat)) install.packages('testthat')
library(testthat)

find_rfid_perching_events <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_rfid_perching_events.R")$value

# Need to test each check statement using expect_error TKTK

# 1) Test that the correct number of perching events is detected (make a dummy dataset)

test_that("The correct number of perching events is detected", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
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
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    )
  
  expect_equal(nrow(res_df), 3)
  
})


# 2) Test that the gaps between the filtered timestamps are no larger than the temporal threshold (make a dummy dataset)

test_that("Timestamps within each perching event are not farther apart than the temporal threshold", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
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
  
})


# test 3: test that when there are no perching events for the given temporal threshold that the empty data frame is returned for the dates of the original data

test_that("Timestamps within each perching event are not farther apart than the temporal threshold", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development for this test
  # library(lubridate)
  # library(tidyverse)
  
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

# test 4: test that the function takes the first and last timestamp of each run as the start and end (make a dummy dataset)

# test 5: test that each perching event starts and ends on a single day (cannot span more than 1 day)

# tests 6 and 7: repeat tests 5 and 6 but with a dummy dataset with more than one PIT tag ID