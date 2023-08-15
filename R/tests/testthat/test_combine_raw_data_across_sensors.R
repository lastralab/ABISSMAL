
if (!require(testthat)) install.packages('testthat')
library(testthat)

combine_raw_data_per_sensor <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/combine_raw_data_per_sensor.R")$value

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
  withr::local_package("pbapply")
  
  # Just for code development
  # library(tidyverse)
  # library(lubridate)
  # library(pbapply)
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
  dir.create(file.path(tmp_path, "RFID"))
  
  # Generate files of raw RFID data per unique date
  dates <- c("2023-8-1", "2023-8-2", "2023-8-3")
  dates
  
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

# Test that video recording data collected across dates is combined into a single file

# Test that temperature data collected across dates is combined into a single file

# Test that data collected across all sensor types and dates is combined into a single file