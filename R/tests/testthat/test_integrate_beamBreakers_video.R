library(tidyverse)

l_th <- 0
u_th <- 5
video_rec_dur <- 9
irbb_file_nm <- "labeled_beamBreaker_data.csv"
video_file_nm <- "pre_processed_data_Video.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
outer_irbb_col <- "Outer_beam_breaker"
inner_irbb_col <- "Inner_beam_breaker"
irbb_event_col <- "irbb_direction_inferred"
irbb_unique_col <- "unique_entranceExit"
preproc_metadata_cols <- c("thin_threshold_s", "data_stage", "date_pre_processed", "lower_threshold_s", "upper_threshold_s", "date_labeled", "diffs", "irbb_assignmnt_type")
general_metadata_cols <- c("chamber_id", "year", "month", "day")
video_metadata_cols <- c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name")
extra_cols2drop <- NA
integrate_perching <- TRUE
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
irbb_dir <- "pre_processed"
video_dir <- "pre_processed"
out_dir <- "integrated"
out_file_nm = "integrated_beamBreaker_video_data.csv"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"

# Lead and lag assignments
# table(integr8d_df$assignmnt_type)

# This temporal range looks good
# integr8d_df %>% 
# dplyr::filter(movement_inference == "motion_trigger") %>% 
# pull(rfid_video_diffs) %>% 
# range()


# Checking, looks good
# any(duplicated(integr8d_df_noDups[[rfid_col]]))
# (nrow(integr8d_df) - nrow(integr8d_df_noDups)) == length(dup_inds)

# Testing the second integration

irbb_file_nm = "tmp_rfid_irbb_integration.csv"
video_file_nm
l_th = l2_th
u_th = u2_th
video_rec_dur
sensor_id_col
timestamps_col
PIT_tag_col
outer_irbb_col
inner_irbb_col
irbb_event_col
irbb_unique_col
preproc_metadata_cols = c("data_stage", "date_integrated")
general_metadata_cols = c("chamber_id", "year", "month", "day")
extra_cols2drop = c("RFID", "PIT_tag_ID", "outer_rfid_diffs", "rfid_irbb_assignmnt_type", "rfid_irbb_lower_threshold_s", "rfid_irbb_upper_threshold_s")
video_metadata_cols
path
irbb_dir = "tmp"
video_dir
out_dir = "tmp"
out_file_nm = "tmp_rfid_irbb_video_integration.csv"
tz
POSIXct_format = "%Y-%m-%d %H:%M:%OS"