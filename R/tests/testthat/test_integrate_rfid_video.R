library(tidyverse)

l_th <- 1
u_th <- 3
video_rec_dur <- 9
rfid_file_nm <- "pre_processed_data_RFID.csv"
video_file_nm <- "pre_processed_data_Video.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
preproc_metadata_cols <- c("thin_threshold_s", "data_stage", "date_pre_processed", "lower_threshold_s", "upper_threshold_s", "date_labeled")
general_metadata_cols <- c("chamber_id", "year", "month", "day")
video_metadata_cols <- c("total_pixels_motionTrigger", "pixel_threshold", "video_file_name")
extra_cols2drop <- NA
integrate_perching <- TRUE
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
rfid_dir <- "pre_processed"
video_dir <- "pre_processed"
out_dir <- "integrated"
out_file_nm = "integrated_rfid_video_data.csv"
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

# In rough initial checks I found that the duplicate code at the very end yields the same number of rows regardless of whether the sign or temporal methods are used. Check this again and carefully consider whether the sign difference is even useful