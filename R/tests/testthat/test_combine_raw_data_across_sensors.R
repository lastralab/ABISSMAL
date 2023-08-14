
if (!require(testthat)) install.packages('testthat')
library(testthat)

combine_raw_data_per_sensor <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/combine_raw_data_per_sensor.R")$value

source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/utilities.R")

# This testing file can be run by calling test_file("./path/to/this/file)

# TKTK or across all data types??

# Test that RFID data collected across dates is combined into a single file

combine_raw_data_per_sensor(sensors = c("IRBB", "RFID", "Video", "Temp"), path, data_dir, out_dir, tz, POSIXct_format = "%Y-%m-%d %H:%M:%OS")

# Test that IRBB data from 2 beam breaker pairs collected across dates is combined into a single file

# Test that video recording data collected across dates is combined into a single file

# Test that temperature data collected across dates is combined into a single file