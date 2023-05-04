l_th <- 0
u_th <- 5
video_rec_dur <- 9
irbb_file_nm <- "integrated_beamBreaker_data.csv"
video_file_nm <- "pre_processed_data_Video.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
outer_irbb_col <- "Outer_beam_breaker"
inner_irbb_col <- "Inner_beam_breaker"
irbb_event_col <- "direction_inferred"
irbb_unique_col <- "unique_entranceExit"
method <- "temporal"
general_metadata_cols <- c("chamber_id", "year", "month", "day")
video_metadata_cols <- c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name")
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
data_dir <- "pre_processed"
out_dir <- "integrated"
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