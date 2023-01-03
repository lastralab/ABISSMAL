# Use the raw radio frequency identification (RFID) data to identify perching events (e.g. periods of time when an individual was perched on the RFID antenna). This function is performed for each unique passive integrated transponder (PIT) tag in the dataset. The function identifies runs of RFID detections separated by the given temporal threshold or less, then takes the first and last detection of each run and return these timestamps with start and end labels. Note that unlike the RFID pre-processing function, this function groups the data frame not only by PIT tag ID but also by date to avoid artificially long perching periods

# See more info on the testthat package: https://r-pkgs.org/testing-basics.html

# See examples on: 
# https://www.r-bloggers.com/2019/11/automated-testing-with-testthat-in-practice/

if (!require(testthat)) install.packages('testthat')
library(testthat)

find_rfid_perching_events <- source("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R/find_rfid_perching_events.R")

find_rfid_perching_events

# test 1: test that the gaps between the filtered timestamps are no larger than the temporal threshold (use a dummy dataset)

# test 2: test that the function takes the first and last timestamp of each run as the start and end (use a dummy dataset)

# tests 3 and 4: repeat tests 5 and 6 but with a dummy dataset with more than one PIT tag ID