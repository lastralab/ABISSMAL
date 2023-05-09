# G. Smith-Vidaurre
# 03 January 2023

# See more info on the testthat package: https://r-pkgs.org/testing-basics.html


# TKTK need to test that there are no negative durations and also no duplicate durations indicating assignments within or between PIT tags

rfid_file_nm <- "combined_raw_data_RFID.csv"
threshold <- 2
run_length <- 2
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
general_metadata_cols <- c("chamber_id", "sensor_id")
rfid_dir <- "raw_combined"
out_dir <- "pre_processed"
out_file_nm = "perching_events.csv"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"


# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

if (!require(testthat)) install.packages('testthat')
library(testthat)

find_rfid_perching_events <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_rfid_perching_events.R")$value

# This testing file can be run by calling test_file("./path/to/this/file)

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


########## Testing expected behavior ########## 

test_that("The correct number of perching events is detected", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  
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
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    )
  
  expect_equal(nrow(res_df), 3)
  
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
    )) 
  
  res_df <- test_df %>%
    group_split(tag_id) %>%
    map_dfr(
      ~ find_rfid_perching_events(df = .x, threshold = 1, rfid_col_nm = "RFID", tag_id_col_nm = "tag_id", run_length = 2)
    )
  
  expect_equal(nrow(res_df), 3)
  
})


test_that("Timestamps within each perching event are not farther apart than the temporal threshold", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
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


test_that("An empty data frame is returned when no perching events are found for the given threshold", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
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


test_that("The first and last timestamp of each run of consecutive RFID detections are used for the perching events", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
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


test_that("The start and end times for each perching event occur during the same day", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  
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
