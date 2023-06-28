# This test should check that entrances and exits are being captured correctly

test_that("Directional labeling is correct for entrances and exits", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("pbapply")
  
  # Just for code development
  library(tidyverse)
  library(lubridate)
  library(pbapply)
  library(testthat)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop/tmp_tests"
  
  if(!dir.exists(path)){ 
    dir.create(path)
  }
  
  # Generate a file with pre-processed timestamps for 2 pairs of beam breakers
  
  # Create 2 entrance and 2 exit events
  ent_starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST"
  ))
  
  ent_ends <- as.POSIXct(c(
    "2023-01-01 01:01:00 EST",
    "2023-01-01 02:01:00 EST"
  ))
  
  exi_starts <- as.POSIXct(c(
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  exi_ends <- as.POSIXct(c(
    "2023-01-01 01:06:00 EST",
    "2023-01-01 02:06:00 EST"
  ))
  
  outer_ts <- c(ent_starts, exi_ends)
  inner_ts <- c(ent_ends, exi_starts)
  
  test_df <- data.frame(timestamps_ms = c(outer_ts, inner_ts)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = c(year(outer_ts), year(inner_ts)),
      month = c(month(outer_ts), month(inner_ts)),
      day = c(day(outer_ts), day(inner_ts)),
      sensor_id = c(rep("Outer beam breaker", length(outer_ts)),
      rep("Inner beam breaker", length(inner_ts))),
      data_type = "IRBB",
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(test_df, file.path(path, "test_label_irbb.csv"), row.names = FALSE)
  
  # Test the function
  label_beamBreaker_events(irbb_file_nm = "test_label_irbb.csv", l_th = 0, u_th = 2, sensor_id_col = "sensor_id", timestamps_col = "timestamps_ms", outer_irbb_nm = "Outer beam breaker", inner_irbb_nm = "Inner beam breaker", path = "/home/gsvidaurre/Desktop", data_dir = "tmp_tests", out_dir = "tmp_tests", out_file_nm = "labeled_beamBreaker_testData.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(path, "labeled_beamBreaker_testData.csv"))
  # glimpse(test_res)
  
  # The results should be two repetitions of an entrance followed by an exit
  expect_equal(test_res$irbb_direction_inferred, rep(c("entrance", "exit"), 2))
  
  # Then another test: switching the order of the timestamps between beam breaker pairs should yield 2 repetitions of an exit followed by an entrance
  inner_ts <- c(ent_starts, exi_ends)
  outer_ts <- c(ent_ends, exi_starts)
  
  test_df <- data.frame(timestamps_ms = c(outer_ts, inner_ts)) %>% 
    dplyr::mutate(
      chamber_id = "Box_01",
      year = c(year(outer_ts), year(inner_ts)),
      month = c(month(outer_ts), month(inner_ts)),
      day = c(day(outer_ts), day(inner_ts)),
      sensor_id = c(rep("Outer beam breaker", length(outer_ts)),
                    rep("Inner beam breaker", length(inner_ts))),
      data_type = "IRBB",
      thin_threshold_s = 1,
      data_stage = "pre-processing",
      date_pre_processed = Sys.Date()
    )
  
  write.csv(test_df, file.path(path, "test_label_irbb.csv"), row.names = FALSE)
  
  # Test the function
  label_beamBreaker_events(irbb_file_nm = "test_label_irbb.csv", l_th = 0, u_th = 2, sensor_id_col = "sensor_id", timestamps_col = "timestamps_ms", outer_irbb_nm = "Outer beam breaker", inner_irbb_nm = "Inner beam breaker", path = "/home/gsvidaurre/Desktop", data_dir = "tmp_tests", out_dir = "tmp_tests", out_file_nm = "labeled_beamBreaker_testData.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(path, "labeled_beamBreaker_testData.csv"))
  # glimpse(test_res)
  
  # The results should be two repetitions of an entrance followed by an exit
  expect_equal(test_res$irbb_direction_inferred, rep(c("exit", "entrance"), 2))
  
  # Remove the temporary directory and all files within it
  unlink(path, recursive = TRUE)
  
})



library(tidyverse)

irbb_file_nm <- "pre_processed_data_IRBB.csv"
l_th <- 0
u_th <- 2
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
outer_irbb_nm <- "Outer Beam Breaker"
inner_irbb_nm <- "Inner Beam Breaker"
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
data_dir <- "pre_processed"
out_dir <- "integrated"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"


ee_df <- read.csv(file.path(path, out_dir, "integrated_beamBreaker_data.csv")) %>% 
  # Make sure that the timestamps are in the right format
  dplyr::mutate(
    outer_beamBreaker_timestamp = as.POSIXct(format(as.POSIXct(outer_beamBreaker_timestamp, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6")),
    inner_beamBreaker_timestamp = as.POSIXct(format(as.POSIXct(inner_beamBreaker_timestamp, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
  ) 
glimpse(ee_df)

# Checking. How many entrances and exits detected?
ee_df %>% 
  group_by(type) %>% 
  dplyr::summarise(
    n = n()
  )

# Do all entrances have a negative difference in timestamps, and exits positive? # Yes, looks good
ee_df %>% 
  dplyr::mutate(
    diff = outer_beamBreaker_timestamp - inner_beamBreaker_timestamp
  ) %>% 
  group_by(type) %>% 
  dplyr::summarise(
    min_dff = min(diff),
    max_diff = max(diff)
  )

nrow(integr8d_df_noDups)
nrow(integr8d_df)

# Check duplicates....
length(which(duplicated(integr8d_df_noDups[[bb_ids[1]]])))
length(which(duplicated(integr8d_df_noDups[[bb_ids[2]]])))

wh <- which(duplicated(integr8d_df_noDups[[bb_ids[1]]]))[101]

integr8d_df_noDups %>% 
  dplyr::filter(!!sym(bb_ids[1]) == integr8d_df_noDups[[bb_ids[1]]][wh]) %>% 
  View()

# Check the distribution of temporal differences between adjacent events as well