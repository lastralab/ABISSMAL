
library(tidyverse)

integrated_file_nm <- "integrated_rfid_beamBreakers_video_data.csv"
threshold <- 1
run_length = 2
timestamps_col <- "RFID"
PIT_tag_col <- "PIT_tag_ID"
preproc_metadata_cols <- c("thin_threshold_s", "data_stage", "date_pre_processed", "lower_threshold_s", "upper_threshold_s", "date_labeled")
general_metadata_cols <- c("chamber_id", "year", "month", "day")
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
data_dir <- "integrated"
out_dir <- "integrated"
out_file_nm = "rfid_activity_bouts.csv"
tz <- "America/New York"
POSIXct_format = "%Y-%m-%d %H:%M:%OS"