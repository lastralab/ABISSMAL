ee_df <- read.csv(file.path(path, out_dir, "labeled_beamBreaker_data.csv")) %>% 
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