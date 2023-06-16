library(tidyverse)

l_th <- 0
u_th <- 5
rfid_file_nm <- "pre_processed_data_RFID.csv"
irbb_file_nm <- "labeled_beamBreaker_data.csv"
sensor_id_col <- "sensor_id"
timestamps_col <- "timestamp_ms"
PIT_tag_col <- "PIT_tag_ID"
outer_irbb_col <- "Outer_beam_breaker"
inner_irbb_col <- "Inner_beam_breaker"
irbb_event_col <- "irbb_direction_inferred"
irbb_unique_col <- "unique_entranceExit"
preproc_metadata_cols <- c("thin_threshold_s", "data_stage", "date_pre_processed", "lower_threshold_s", "upper_threshold_s", "date_labeled")
method <- "temporal"
path <- "/media/gsvidaurre/Anodorhynchus/Data_Testing/Box_02_31Dec2022/Data"
data_dir <- "pre_processed"
out_dir <- "integrated"
tz <- "America/New York"
POSIXct_format <- "%Y-%m-%d %H:%M:%OS"
integrate_perching <- TRUE