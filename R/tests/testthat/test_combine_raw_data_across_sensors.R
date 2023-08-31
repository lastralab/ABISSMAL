# G. Smith-Vidaurre
# 03 January 2023

# See more info on the testthat package: https://r-pkgs.org/testing-basics.html

# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

if (!require(testthat)) install.packages('testthat')
library(testthat)

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/combine_raw_data_per_sensor.R")

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

########## Testing output ########## 

# Test that RFID data collected across dates is combined into a single file
test_that("The function combines RFID data collected across dates", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Then create an RFID folder inside this
  if(!dir.exists(file.path(tmp_path, "RFID"))){
    dir.create(file.path(tmp_path, "RFID"))
  }
  
  # Generate files of raw RFID data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")
  
  # Create a vector of RFID timestamps. Each file will have 4 timestamps
  event_ts <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(timestamp_ms = event_ts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "RFID",
        PIT_tag_ID = "test"
      )
    
    write.csv(sim_ts, file.path(tmp_path, "RFID", paste("RFID_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  combine_raw_data_per_sensor(sensors = "RFID", path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "raw_combined", "combined_raw_data_RFID.csv"))
  
  # Test that the results are the expected number of timestamps per date
  expect_equal(nrow(test_res), length(event_ts) * length(dates))
  
  # Test that all expected dates are in the combined file
  expect_equal(unique(paste(test_res$year, test_res$month, test_res$day, sep = "_")), gsub("-", "_", dates))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# Test that IRBB data from 2 beam breaker pairs collected across dates is combined into a single file
test_that("The function combines IRBB data collected across dates", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Then create an IRBB folder inside this path
  if(!dir.exists(file.path(tmp_path, "IRBB"))){
    dir.create(file.path(tmp_path, "IRBB"))
  }
  
  # Generate files of raw IRBB data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")
  
  # Create a vector of inner and outer beam breaker timestamps. Each file will have 8 timestamps (4 per beam breaker pair)
  event_ts_o <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  event_ts_i <- c(
    "01:00:02",
    "02:00:02",
    "01:05:02",
    "02:05:02"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(timestamp_ms = c(event_ts_o, event_ts_i)) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = c(rep("Outer Beam Breaker", length(event_ts_o)), rep("Inner Beam Breaker", length(event_ts_i)))
      )
    
    write.csv(sim_ts, file.path(tmp_path, "IRBB", paste("IRBB_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  combine_raw_data_per_sensor(sensors = "IRBB", path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "raw_combined", "combined_raw_data_IRBB.csv"))
  
  # Test that the results are the expected number of timestamps per date
  expect_equal(nrow(test_res), (length(event_ts_o) + length(event_ts_i))* length(dates))
  
  # Test that all expected dates are in the combined file
  expect_equal(unique(paste(test_res$year, test_res$month, test_res$day, sep = "_")), gsub("-", "_", dates))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# Test that video recording data collected across dates is combined into a single file
test_that("The function combines Video data collected across dates", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Then create a Video folder inside this path
  if(!dir.exists(file.path(tmp_path, "Video"))){
    dir.create(file.path(tmp_path, "Video"))
  }
  
  # Generate files of raw Video recording events data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")
  
  # Create a vector of timestamps of video recording events. Each file will have 4 timestamps
  event_ts <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function. Each timestamp is repeated twice to account for a pre- and post-motion trigger video recording per timestamp
  invisible(lapply(1:length(dates), function(i){
    
    file_nms <- paste(paste(rep(paste(paste("Box_01", gsub("-", "_", dates[i]), sep = "_"), gsub(":", "_", event_ts), sep = "_"), each = 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = "")
    
    sim_ts <- data.frame(time_video_started = rep(event_ts, each = 2)) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "Camera",
        video_file_name = file_nms,
        total_pixels_motionTrigger = 100
      )
    
    write.csv(sim_ts, file.path(tmp_path, "Video", paste("Video_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  combine_raw_data_per_sensor(sensors = "Video", path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "raw_combined", "combined_raw_data_Video.csv"))
  
  # Test that the results are the expected number of timestamps per date
  expect_equal(nrow(test_res), (length(event_ts) * 2) * length(dates))
  
  # Test that all expected dates are in the combined file
  expect_equal(unique(paste(test_res$year, test_res$month, test_res$day, sep = "_")), gsub("-", "_", dates))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# Test that temperature data collected across dates is combined into a single file
test_that("The function combines temperature data collected across dates", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Then create a Temperature folder inside this path
  if(!dir.exists(file.path(tmp_path, "Temp"))){
    dir.create(file.path(tmp_path, "Temp"))
  }
  
  # Generate files of raw temperature data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")

  # Create a vector of timestamps of temperature measurements taken each minute. Each file will have 4 timestamps
  event_ts <- c(
    "01:00:00",
    "01:01:00",
    "01:02:00",
    "01:03:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){

    sim_ts <- data.frame(time = event_ts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "Temp",
        degrees_Celsius = 23.9,
        degrees_Farenheit = 75
      )
    
    write.csv(sim_ts, file.path(tmp_path, "Temp", paste("Temp_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  combine_raw_data_per_sensor(sensors = "Temp", path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # Read in the output, check the output, then delete all files
  test_res <- read.csv(file.path(tmp_path, "raw_combined", "combined_raw_data_Temp.csv"))
  
  # Test that the results are the expected number of timestamps per date
  expect_equal(nrow(test_res), length(event_ts) * length(dates))
  
  # Test that all expected dates are in the combined file
  expect_equal(unique(paste(test_res$year, test_res$month, test_res$day, sep = "_")), gsub("-", "_", dates))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

# Test that data collected across all 4 sensor types and dates is combined into a single file
test_that("The function combines data from all 4 sensor types collected across dates", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Then create a folders for each sensor type inside this path
  sensrs <- c("RFID", "IRBB", "Video", "Temp")
  
  invisible(lapply(1:length(sensrs), function(i){
    
    if(!dir.exists(file.path(tmp_path,  sensrs[i]))){
      dir.create(file.path(tmp_path, sensrs[i]))
    }
    
  }))
  
  # Generate files of raw temperature data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")
  
  ###### RFID ######
  
  # Create a vector of RFID timestamps. Each file will have 4 timestamps
  rfid_event_ts <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(timestamp_ms = rfid_event_ts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "RFID",
        PIT_tag_ID = "test"
      )
    
    write.csv(sim_ts, file.path(tmp_path, "RFID", paste("RFID_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))

  ###### IRBB ######
  
  # Create a vector of inner and outer beam breaker timestamps. Each file will have 8 timestamps (4 per beam breaker pair)
  event_ts_o <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  event_ts_i <- c(
    "01:00:02",
    "02:00:02",
    "01:05:02",
    "02:05:02"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(timestamp_ms = c(event_ts_o, event_ts_i)) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = c(rep("Outer Beam Breaker", length(event_ts_o)), rep("Inner Beam Breaker", length(event_ts_i)))
      )
    
    write.csv(sim_ts, file.path(tmp_path, "IRBB", paste("IRBB_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  ###### Video ######
  
  # Create a vector of timestamps of video recording events. Each file will have 4 timestamps
  video_event_ts <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function. Each timestamp is repeated twice to account for a pre- and post-motion trigger video recording per timestamp
  invisible(lapply(1:length(dates), function(i){
    
    file_nms <- paste(paste(rep(paste(paste("Box_01", gsub("-", "_", dates[i]), sep = "_"), gsub(":", "_", video_event_ts), sep = "_"), each = 2), c("pre_trigger", "post_trigger"), sep = "_"), ".mp4", sep = "")
    
    sim_ts <- data.frame(time_video_started = rep(video_event_ts, each = 2)) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "Camera",
        video_file_name = file_nms,
        total_pixels_motionTrigger = 100
      )
    
    write.csv(sim_ts, file.path(tmp_path, "Video", paste("Video_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  ###### Temperature ######
  
  # Create a vector of timestamps of temperature measurements taken each minute. Each file will have 4 timestamps
  temp_event_ts <- c(
    "01:00:00",
    "01:01:00",
    "01:02:00",
    "01:03:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(time = temp_event_ts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "Temp",
        degrees_Celsius = 23.9,
        degrees_Farenheit = 75
      )
    
    write.csv(sim_ts, file.path(tmp_path, "Temp", paste("Temp_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  combine_raw_data_per_sensor(sensors = sensrs, path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS")
  
  # For each sensor type, read in and check the output before deleting temporary files
  invisible(lapply(1:length(sensrs), function(i){
    
    test_res <- read.csv(file.path(tmp_path, "raw_combined", paste("combined_raw_data_", sensrs[i], ".csv", sep = "")))
    
    # Test that the results are the expected number of timestamps per date
    if(sensrs[i] == "RFID"){
      
      expect_equal(nrow(test_res), length(rfid_event_ts) * length(dates))
      
    }  else if(sensrs[i] == "IRBB"){
      
      expect_equal(nrow(test_res), (length(event_ts_o) + length(event_ts_i)) * length(dates))
      
    } else if(sensrs[i] == "Video"){
      
      expect_equal(nrow(test_res), (length(video_event_ts) * 2) * length(dates))
      
    } else if(sensrs[i] == "Temp"){
      
      expect_equal(nrow(test_res), length(temp_event_ts) * length(dates))
      
    }
    
    # Test that all expected dates are in the combined file
    expect_equal(unique(paste(test_res$year, test_res$month, test_res$day, sep = "_")), gsub("-", "_", dates))
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

########## Testing error handling ########## 

test_that("the function catches when non-NULL arguments are NULL", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Then create an RFID folder inside this
  if(!dir.exists(file.path(tmp_path, "RFID"))){
    dir.create(file.path(tmp_path, "RFID"))
  }
  
  # Generate files of raw RFID data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")
  
  # Create a vector of RFID timestamps. Each file will have 4 timestamps
  event_ts <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(timestamp_ms = event_ts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "RFID",
        PIT_tag_ID = "test"
      )
    
    write.csv(sim_ts, file.path(tmp_path, "RFID", paste("RFID_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  # General arguments that cannot ever be NULL:
  arg_nms <- c("sensors", "path", "data_dir", "out_dir", "tz", "POSIXct_format")
  
  args <- list(
    `sensors` = "RFID",
    `path` = path,
    `data_dir` = file.path(data_dir, "raw_combined"),
    `out_dir` = file.path(data_dir, "processed"),
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )
  
  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- list(NULL)

    expect_error(
      combine_raw_data_per_sensor(sensors = args[["sensors"]], path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
      regexp = paste("Expected a non-NULL value but the argument", arg_nms[i], "is NULL", sep = " ")
    )
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when the sensor argument vector is not RFID, IRBB, Video, or Temp", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Then create an RFID folder inside this
  if(!dir.exists(file.path(tmp_path, "RFID"))){
    dir.create(file.path(tmp_path, "RFID"))
  }
  
  # Generate files of raw RFID data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")
  
  # Create a vector of RFID timestamps. Each file will have 4 timestamps
  event_ts <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(timestamp_ms = event_ts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "RFID",
        PIT_tag_ID = "test"
      )
    
    write.csv(sim_ts, file.path(tmp_path, "RFID", paste("RFID_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  expect_error(
    combine_raw_data_per_sensor(sensors = 1, path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "One or more values provided for the argument, sensors, are not correct. Check your spelling or captialization"
  )
  
  expect_error(
    combine_raw_data_per_sensor(sensors = "RFD", path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "One or more values provided for the argument, sensors, are not correct. Check your spelling or captialization"
  )
  
  expect_error(
    combine_raw_data_per_sensor(sensors = "rfid", path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "One or more values provided for the argument, sensors, are not correct. Check your spelling or captialization"
  )
  
  expect_error(
    combine_raw_data_per_sensor(sensors = c("rfid", "IRBB", "Video", "Temp"), path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "One or more values provided for the argument, sensors, are not correct. Check your spelling or captialization"
  )
  
  expect_error(
    combine_raw_data_per_sensor(sensors = c("rfid", "IRBB", "Video", "tmp"), path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = "One or more values provided for the argument, sensors, are not correct. Check your spelling or captialization"
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when character string arguments are not strings", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Then create an RFID folder inside this
  if(!dir.exists(file.path(tmp_path, "RFID"))){
    dir.create(file.path(tmp_path, "RFID"))
  }
  
  # Generate files of raw RFID data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")
  
  # Create a vector of RFID timestamps. Each file will have 4 timestamps
  event_ts <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(timestamp_ms = event_ts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "RFID",
        PIT_tag_ID = "test"
      )
    
    write.csv(sim_ts, file.path(tmp_path, "RFID", paste("RFID_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  # General arguments that should always be strings. Not including sensors here because this argument is checked in a separate test above
  arg_nms <- c("path", "data_dir", "out_dir", "tz", "POSIXct_format")
  
  args <- list(
    `path` = path,
    `data_dir` = file.path(data_dir, "raw_combined"),
    `out_dir` = file.path(data_dir, "processed"),
    `tz` = "America/New York",
    `POSIXct_format` = "%Y-%m-%d %H:%M:%OS"
  )

  invisible(lapply(1:length(arg_nms), function(i){
    
    args[arg_nms[i]] <- 1
    
    expect_error(
      combine_raw_data_per_sensor(sensors = "RFID", path = args[["path"]], data_dir = args[["data_dir"]], out_dir = args[["out_dir"]], tz = args[["tz"]], POSIXct_format = args[["POSIXct_format"]]),
      regexp = paste("Expected a string but the argument", arg_nms[i], "is not a string", sep = " ")
    )  
    
  }))
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})

test_that("the function catches when paths don't exist", {
  
  # Avoid library calls and other changes to the virtual environment
  # See https://r-pkgs.org/testing-design.html
  withr::local_package("tidyverse")
  withr::local_package("plyr")
  withr::local_package("dplyr")
  withr::local_package("lubridate")
  withr::local_package("data.table")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(testthat)
  # library(data.table)
  
  # Create a temporary directory for testing. Files will be written and read here
  path <- "/home/gsvidaurre/Desktop"
  data_dir <- "tmp_tests"
  tmp_path <- file.path(path, data_dir)
  
  if(!dir.exists(tmp_path)){ 
    dir.create(tmp_path)
  }
  
  # Then create an RFID folder inside this
  if(!dir.exists(file.path(tmp_path, "RFID"))){
    dir.create(file.path(tmp_path, "RFID"))
  }
  
  # Generate files of raw RFID data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")
  
  # Create a vector of RFID timestamps. Each file will have 4 timestamps
  event_ts <- c(
    "01:00:00",
    "02:00:00",
    "01:05:00",
    "02:05:00"
  )
  
  # Write out a spreadsheet per date with these timestamps that will be used as input data for the function
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(timestamp_ms = event_ts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "RFID",
        PIT_tag_ID = "test"
      )
    
    write.csv(sim_ts, file.path(tmp_path, "RFID", paste("RFID_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  # Remove these files
  invisible(file.remove(list.files(path = file.path(tmp_path, "RFID"), pattern = ".csv$", full.names = TRUE)))
  
  expect_error(
    combine_raw_data_per_sensor(sensors = "RFID", path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The directory", file.path(tmp_path, "RFID"), "does not have the correct files", sep = " ")
  )
  
  # Generate the files again, then remove the whole directory
  invisible(lapply(1:length(dates), function(i){
    
    sim_ts <- data.frame(timestamp_ms = event_ts) %>% 
      dplyr::mutate(
        chamber_id = "Box_01",
        year = year(dates[i]),
        month = month(dates[i]),
        day = day(dates[i]),
        sensor_id = "RFID",
        PIT_tag_ID = "test"
      )
    
    write.csv(sim_ts, file.path(tmp_path, "RFID", paste("RFID_Box_01_", gsub("-", "_", dates[i]), ".csv", sep = "")), row.names = FALSE)
    
  }))
  
  # Remove the directory where the files are located
  if(file.path(tmp_path, "RFID") == file.path(path, data_dir, "RFID")){
    unlink(file.path(tmp_path, "RFID"), recursive = TRUE)
  }
  
  expect_error(
    combine_raw_data_per_sensor(sensors = "RFID", path = path, data_dir = data_dir, out_dir = file.path(data_dir, "raw_combined"), tz = "America/New York", POSIXct_format = "%Y-%m-%d %H:%M:%OS"),
    regexp = paste("The directory", file.path(tmp_path, "RFID"), "does not exist", sep = " ")
  )
  
  # Remove the temporary directory and all files within it
  if(tmp_path == file.path(path, data_dir)){
    unlink(tmp_path, recursive = TRUE)
  }
  
})
