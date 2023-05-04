library(tidyverse)

l1_th <- 0
u1_th <- 5
l2_th <- 0
u2_th <- 5
video_rec_dur <- 9
rfid_file_nm <- "pre_processed_data_RFID.csv"
irbb_file_nm <- "labeled_beamBreaker_data.csv"
video_file_nm <- "pre_processed_data_Video.csv"
second_integration <- "irbb-video" # "rfid-video" 
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
outer_irbb_col <- "Outer_beam_breaker"
inner_irbb_col <- "Inner_beam_breaker"
irbb_event_col <- "irbb_direction_inferred"
irbb_unique_col <- "unique_entranceExit"
preproc_metadata_cols <- c("thin_threshold_s", "data_stage", "date_pre_processed")
general_metadata_cols <- c("chamber_id", "year", "month", "day")
video_metadata_cols <- c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name")
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
rfid_dir <- "pre_processed"
irbb_dir <- "pre_processed"
video_dir <- "pre_processed"
out_dir <- "integrated"
out_file_nm <- "integrated_rfid_beamBreakers_video_data.csv"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"

source(file.path("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R", "integrate_rfid_breamBreakers.R"))

source(file.path("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R", "integrate_rfid_video.R"))

source(file.path("/home/gsvidaurre/Desktop/GitHub_repos/Abissmal/R", "integrate_beamBreakers_video.R"))
