
# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

# rm(list = ls())

if (!require(testthat)) install.packages('testthat')
library(testthat)

find_detectionClusters <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_detectionClusters.R")$value

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

########## Testing error messages ########## 

# Here I want to test that the function detects the expected clusters of detections when data from one or multiple sensors is used as input
test_that("The function detects the expected number of clusters using data from one sensor", {
  
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
  path <- "/home/gsvidaurre/Desktop/tmp_tests"
  
  if(!dir.exists(path)){ 
    dir.create(path)
  }
  
  # Generate a file with pre-processed timestamps for one sensor
  
  # Create 4 clusters of detections: each cluster consists of 2 detections spaced 1 second apart
  starts <- as.POSIXct(c(
    "2023-01-01 01:00:00 EST",
    "2023-01-01 02:00:00 EST",
    "2023-01-01 01:05:00 EST",
    "2023-01-01 02:05:00 EST"
  ))
  
  ends <- starts + 1

  # Write out a spreadsheet with these timestamps that will be used as input data for the function
  
  sim_ts <- data.frame(timestamp_ms = c(starts, ends)) %>% 
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
  
  # glimpse(sim_ts)
  
  write.csv(sim_ts, file.path(path, "simulated_single_sensor.csv"), row.names = FALSE)
  
  # Test the function
  # source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_detectionClusters.R")
  
  # There should be 4 different matches. Note that the run length needs to be set to 1 in order to correctly detect detection clusters of length 2
  # video_metadatacols = c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name")
  find_detectionClusters(file_nms = "simulated_single_sensor.csv", threshold = 2, run_length = 1, sensor_id_col = "sensor_id", timestamps_col = "timestamp_ms", PIT_tag_col = "PIT_tag_ID", rfid_label = "RFID", camera_label = NULL, drop_tag = NULL, preproc_metadata_cols = c("thin_threshold_s", "data_stage", "date_pre_processed"), general_metadata_cols = c("chamber_id", "year", "month", "day"), video_metadata_cols = NULL, path = "/home/gsvidaurre/Desktop", data_dir = "tmp_tests", out_dir = "tmp_tests", out_file_nm = "detection_clusters.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(path, out_dir, "detection_clusters.csv"))
  # glimpse(test_res)
  
  # The results should be 4 detection clusters, or the length of the start and end timestamps created above
  expect_equal(nrow(test_res), length(starts))

  # Remove the temporary directory and all files within it
  # unlink(file.path(path, data_dir), recursive = TRUE)
  
})


#### TKTK temporary code for more than one sensor

# Generate a file with pre-processed timestamps for one sensor

# Create 2 entrance (RFID triggered second) and 2 exit (RFID triggered first) events
ent_starts <- as.POSIXct(c(
  "2023-01-01 01:00:00 EST",
  "2023-01-01 02:00:00 EST"
))

ent_ends <- as.POSIXct(c(
  "2023-01-01 01:00:01 EST",
  "2023-01-01 02:00:01 EST"
))

exi_starts <- as.POSIXct(c(
  "2023-01-01 01:05:00 EST",
  "2023-01-01 02:05:00 EST"
))

exi_ends <- as.POSIXct(c(
  "2023-01-01 01:05:01 EST",
  "2023-01-01 02:05:01 EST"
))

rfid_ts <- c(ent_starts, exi_ends)
outer_ts <- c(ent_ends, exi_starts)

# I have to create separate spreadsheets for RFID and IRBB

# rfid_file_nm, irbb_file_nm,

# sensor_id_col, timestamps_col, PIT_tag_col, outer_irbb_col, inner_irbb_col, irbb_event_col, irbb_unique_col,


test_rfid <- data.frame(timestamps_ms = rfid_ts) %>% 
  dplyr::mutate(
    chamber_id = "Box_01",
    year = year(rfid_ts),
    month = month(rfid_ts),
    day = day(rfid_ts),
    sensor_id = "RFID",
    PIT_tag_ID = "test",
    thin_threshold_s = 1
    # data_stage = "pre-processing",
    # date_pre_processed = Sys.Date()
  )

# Adding outer timestamps twice to simulate inner pair timestamps that the function needs (for adding into the final output). These need to be in separate columns though
test_irbb <- data.frame(Outer_beam_breaker = outer_ts) %>% 
  dplyr::mutate(
    chamber_id = "Box_01",
    year = year(outer_ts),
    month = month(outer_ts),
    day = day(outer_ts),
    Inner_beam_breaker = outer_ts,
    irbb_direction_inferred = rep(c("Entrance", "Exit"), 2),
    unique_entranceExit = paste(rep(c("Entrance", "Exit"), 2), rep(seq(1, 2, 1), each = 2), sep = "-"),
    thin_threshold_s = 1
  )

glimpse(test_rfid)
glimpse(test_irbb)

write.csv(test_rfid, file.path(path, "test_rfid.csv"), row.names = FALSE)
write.csv(test_irbb, file.path(path, "test_irbb.csv"), row.names = FALSE)

# Test the function
source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/integrate_rfid_breamBreakers.R")

# Test the inner code of the function with View(lags_grpd$lags[[1]])
# There should be 4 different matches but the function isn't catching a single one. Check the lead and lag RFID to outer IRBB timestamp comparisons carefully
# Why aren't there NAs here as in the real datasets?
integrate_rfid_beamBreakers(rfid_file_nm = "test_rfid.csv", irbb_file_nm = "test_irbb.csv", l_th = 0, u_th = 2, sensor_id_col = "sensor_id", PIT_tag_col = "PIT_tag_ID", timestamps_col = "timestamps_ms", outer_irbb_col = "Outer_beam_breaker", inner_irbb_col = "Inner_beam_breaker", irbb_event_col = "irbb_direction_inferred", irbb_unique_col = "unique_entranceExit", preproc_metadata_cols = c("thin_threshold_s"), general_metadata_cols = c("chamber_id", "year", "month", "day"), path = "/home/gsvidaurre/Desktop", integrate_perching = FALSE, data_dir = "tmp_tests", out_dir = "tmp_tests", out_file_nm = "integ_rfid_beamBreaker_testData.csv", tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")

# Read in the output, check the output, then delete all files
test_res <- read.csv(file.path(path, "integ_rfid_beamBreaker_testData.csv"))
glimpse(test_res)

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


# Read in the output, check the output, then delete all files
test_res <- read.csv(file.path(path, "labeled_beamBreaker_testData.csv"))
# glimpse(test_res)

# The results should be two repetitions of an entrance followed by an exit
expect_equal(test_res$irbb_direction_inferred, rep(c("exit", "entrance"), 2))

# Remove the temporary directory and all files within it
unlink(path, recursive = TRUE)

})















library(tidyverse)

l_th <- 0
u_th <- 5
rfid_file_nm <- "pre_processed_data_RFID.csv"
irbb_file_nm <- "labeled_beamBreaker_data.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
outer_irbb_col <- "Outer_beam_breaker"
inner_irbb_col <- "Inner_beam_breaker"
irbb_event_col <- "irbb_direction_inferred"
irbb_unique_col <- "unique_entranceExit"
preproc_metadata_cols <- c("thin_threshold_s", "data_stage", "date_pre_processed", "lower_threshold_s", "upper_threshold_s", "date_labeled")
method <- "temporal"
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
data_dir <- "pre_processed"
out_dir <- "integrated"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"
integrate_perching <- TRUE

general_metadata_cols