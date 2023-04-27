
library(tidyverse)

irbb_file_nm <- "pre_processed_data_IRBB.csv"
threshold <- 2
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

# Check the distribution of temporal differences between adjacent events as well