#' @title integrate_rfid_beamBreakers_video
#' @description Use lags between the pre-processed radio frequency identification (RFID) data, the pre-processed and labeled beam breaker data, and the video recording events to integrate these three datasets. Each RFID detection that remains must be accompanied by a labeled beam breaker event (an entrance or an exit), as well as a video recording event (e.g. the RFID detection occurred within a certain time of recording onset or during a video recording)
#' 
#' @param .x A data frame object that contains all the pre-processed RFID events, and all the video recording events. Each row is therefore a unique event from either the RFIF or beam breaker dataset. This data frame must contain all the columns in the subsequent arguments
#' @param video_df A data frame object that contains all the video recording events
#' @param l_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify RFID events that occurred just before a video recording and can be integrated with the given video recording event
#' @param u_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify RFID events that occurred just before a video recording and can be integrated with the given video recording event
#' @param p_th A numeric argument. This represents a temporal threshold in seconds to identify RFID events that occurred just after video recording onset, but still during the duration of the video recording, and can be integrated with the given video recording event
#' @param group_col TKTK
#' @param data_type_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "data_type")
#' @param timestamps A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param names_from A character value. This is the name of the metadata column that contains information about the data type (e.g. "data_type"). Used for data frame structure manipulations
#' @param values_from A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms"). Used for data frame structure manipulations
#' @param rfid_nm A character value. The column name that contains RFID timestamps. The data format must support calculations in milliseconds
#' @param video_nm A character value. The column name that contains video recording onset timestamps. The data format must support calculations in milliseconds
#' @param PIT_tag_nm A character value. The name of column that contains the unique PIT tag identifiers detected with each RFID timestamp
#' @param lead_sensor_nm A character value. The column name that contains timestamps for the "lead" pair of beam breakers (e.g. the first pair of beam breakers an animal encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param rear_sensor_nm A character value. The column name that contains timestamps for the "rear" pair of beam breakers (e.g. the second pair of beam breakers an animal encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param direction_nm A character argument. The column name that contains information about the direction of movement
#' 
#' @return A data frame object with all metadata columns in the original data frame, but each row represents a unique RFID detection that was assigned to a beam breaker movement (entrance or exit), as well as a video recording event



# Testing
# l_th <- 0 # testing
# u_th <- 10
# p_th <- 10 # to find videos that triggered right after a RFID detection
# group_col <- "data_type"
# timestamps <- "event_datetime_ms"
# names_from <- "data_type"
# values_from <- "event_datetime_ms"
# rfid_nm <- "RFID"
# video_nm <- "Video"
# PIT_tag_nm <- "PIT_tag_ID"
# lead_sensor_nm <- "lead_bb_timestamp"
# rear_sensor_nm <- "rear_bb_timestamp"
# direction_nm <- "direction"

integrate_rfid_beamBreakers_video <- function(.x, video_df, l_th, u_th, p_th, group_col, timestamps, names_from, values_from, rfid_nm, video_nm, PIT_tag_nm, lead_sensor_nm, rear_sensor_nm, direction_nm, sensor_event_nm){
  
  res_df <- .x %>%
    # irbb_rfid %>% # testing
    #   as_tibble() %>% 
    # Convert to long format 
    pivot_longer(
      cols = c("lead_bb_timestamp", "rear_bb_timestamp", "RFID"),
      names_to = "data_type",
      values_to = "event_datetime_ms"
    ) %>% 
    group_by(!!sym(PIT_tag_nm)) %>% 
    dplyr::rename(
      `group_col` = all_of(PIT_tag_nm)
    ) %>% 
    nest() %>% 
    dplyr::mutate(
      # Do the timestamp difference calculations
      lags = map(.x = data, .y = video_df, .f = ~ bind_rows(
        .x,
        # irbb_rfid %>% 
        # as_tibble() %>% 
        # # Convert to long format
        # pivot_longer(
        # cols = c("lead_bb_timestamp", "rear_bb_timestamp", "RFID"),
        # names_to = "data_type",
        # values_to = "event_datetime_ms"
        # ), # testing
        # Add back the beam breaker data to the subset data frame per PIT tag
        .y %>% 
          # pct_video_rfid %>% # testing
          dplyr::filter(grepl("Video", data_type))
      ) %>% 
        as_tibble() %>%
        # Order timestamps within each data type
        group_by(!!sym(data_type_col)) %>% 
        dplyr::arrange(!!sym(timestamps), desc = FALSE) %>%
        # Make unique row indices within groups
        dplyr::mutate(
          group_row_id = row_number()
        ) %>%
        ungroup() %>% 
        pivot_wider(
          names_from = !!sym(names_from),
          values_from = !!sym(values_from)
        ) %>% 
        # Make a leading and lagging RFID column for calculations and filtering below
        dplyr::mutate(
          leading_RFID = lead(!!sym(rfid_nm), default = first(!!sym(rfid_nm))),
          lagging_RFID = lag(!!sym(rfid_nm), default = first(!!sym(rfid_nm))),
          leading_direction = lead(!!sym(direction_nm), default = first(!!sym(direction_nm))),
          lagging_direction = lag(!!sym(direction_nm), default = first(!!sym(direction_nm))),
          leading_unique_event = lead(!!sym(sensor_event_nm), default = first(!!sym(sensor_event_nm))),
          lagging_unique_event = lag(!!sym(sensor_event_nm), default = first(!!sym(sensor_event_nm)))
        ) %>% 
        # Calculate the differences between the relevant pairs of timestamps: RFID compared to each video event
        # The lags are calculated per group in the grouped data frame
        dplyr::mutate(
          # Here positive differences mean the RFID triggered first
          video_withn_diffs = round(as.numeric(leading_RFID - !!sym(video_nm)), 2),
          # Here negative differences mean the RFID triggered first
          post_video_diffs = round(as.numeric(lagging_RFID - !!sym(video_nm)), 2)
        ) %>% 
        # Convert differences to boolean based on a threshold (in seconds)
        dplyr::mutate(
          binary_vals_wthn = (abs(video_withn_diffs) >= l_th & abs(video_withn_diffs) <= u_th),
          binary_vals_post = (abs(post_video_diffs) >= l_th & abs(post_video_diffs) <= p_th)
        ) %>% 
        dplyr::select(!!sym(video_nm), !!sym(rfid_nm), leading_RFID, lagging_RFID, video_withn_diffs, post_video_diffs, binary_vals_wthn, binary_vals_post, leading_direction, lagging_direction, !!sym(PIT_tag_nm), leading_unique_event, lagging_unique_event) 
      )
    ) %>% 
    dplyr::select(-c(data)) %>% 
    # Do more mapping to perform the integration depending on the temporal thresholds calculated above
    # This is done per PIT tag, and the integration is done separately for entrances and exits
    dplyr::mutate(
      
      # RFID detections that occurred within a video recording event
      matched_irbb_rfid_video = map(.x = lags, .f = ~ dplyr::mutate(
        .x,
        RFID = leading_RFID,
        video_rfid_diffs = video_withn_diffs,
        RFID_type = "within video",
        direction = leading_direction,
        unique_event = leading_unique_event
      ) %>% 
        dplyr::filter(
          binary_vals_wthn
        ) %>% 
        dplyr::select(RFID, !!sym(video_nm), video_rfid_diffs, direction, unique_event, RFID_type) %>%
        # RFID detections that occurred before a video recording event
        bind_rows(
          .x %>%
            dplyr::mutate(
              RFID = lagging_RFID,
              video_rfid_diffs = post_video_diffs,
              RFID_type = "pre-video",
              direction = lagging_direction,
              unique_event = lagging_unique_event
            ) %>%
            dplyr::filter(
              binary_vals_post
            ) %>%
            dplyr::select(RFID, !!sym(video_nm), video_rfid_diffs, direction, unique_event, RFID_type)
        )
      )
    ) %>%
    dplyr::select(-c(lags)) %>%
    unnest(`cols` = c(matched_irbb_rfid_video)) %>% 
    ungroup() %>% 
    # Make sure to add a column with the temporal threshold used
    dplyr::mutate(
      integration_lower_temporal_thresh = l_th,
      integration_upper_temporal_thresh = u_th
    ) %>% 
    # Now join back with the beam breaker timestamps using the unique event names
    # This was very tricky to do above with leads and lags, I kept getting missing values
    dplyr::inner_join(
      irbb_rfid %>% 
        dplyr::select(c("PIT_tag_ID", "unique_beamBreak_event", "lead_bb_timestamp", "rear_bb_timestamp", "RFID")),
      by = c("group_col" = "PIT_tag_ID", "unique_event" = "unique_beamBreak_event", "RFID")
    ) %>% 
    dplyr::select(group_col, lead_bb_timestamp, rear_bb_timestamp, !!sym(rfid_nm), !!sym(video_nm), video_rfid_diffs, direction, unique_event, integration_lower_temporal_thresh, integration_upper_temporal_thresh) %>% 
    glimpse()
  
  names(res_df)[grep("group_col", names(res_df))] <- PIT_tag_nm
  
  # glimpse(res_df)
  # View(res_df)
  
  return(res_df)
  
}

# Set up the integrated RFID and beam breaker data for additional integration with the raw video data
pct_video_rfid <- pct_df2 %>% 
  dplyr::filter(data_type == "Video") %>% 
  dplyr::select(data_type, event_datetime_ms) %>% 
  # Get rid of duplicate rows / timestamps (duplicates are the pre- and post-motion detection videos with the same timestamps)
  distinct(data_type, event_datetime_ms) %>% 
  dplyr::mutate(
    direction = NA, 
    unique_beamBreak_event = NA,
    PIT_tag_ID = NA
  ) 

glimpse(pct_video_rfid)


matched_irbb_rfid_video <- irbb_rfid %>% 
  # Make a fake grouping column to place all rows into the same group for map_dfr works
  dplyr::mutate(
    grping = 1
  ) %>% 
  group_split(grping) %>%
  map_dfr(
    ~ integrate_irbb_rfid_video(
      .x, 
      video_df = pct_video_rfid,
      l_th = 0, 
      u_th = 10, 
      p_th = 10, 
      group_col = "data_type", 
      timestamps = "event_datetime_ms", 
      names_from = "data_type", 
      values_from = "event_datetime_ms", 
      rfid_nm = "RFID", 
      video_nm = "Video", 
      PIT_tag_nm = "PIT_tag_ID",
      lead_sensor_nm = "lead_bb_timestamp",
      rear_sensor_nm = "rear_bb_timestamp",
      direction_nm = "direction",
      sensor_event_nm = "unique_beamBreak_event"
    )
  ) 

glimpse(matched_irbb_rfid_video)

matched_irbb_rfid_video %>% 
  View()

# Get any data that occurred outside of recording hours
irbb_rfid %>% 
  glimpse()

# Need to add back RFID events that occurred after 18:00
# The integrated beam breaker and RFID data also don't have RFOD detections beyond hour 18:00 
matched_irbb_rfid_video %>% 
  dplyr::mutate(
    hours = hour(RFID)
  ) %>% 
  dplyr::summarise(
    max(hours)
  )

rfid_irbb_pre_proc <- read.csv(file.path(out_path, "pre-processed_RFID_IRBB_data_firstRunBox01_Spring2022.csv")) %>% 
  dplyr::mutate(
    # event_datetime_ms = as_datetime(hms(format(as.POSIXct(event_datetime_ms, tz = "America/New York"), "%H:%M:%S")))
    event_datetime_ms = as.POSIXct(format(as.POSIXct(date_time_ms, tz = "America/New York"), "%Y-%m-%d %H:%M:%OS6"))
  )
glimpse(rfid_irbb_pre_proc)

# unique(rfid_irbb_pre_proc$data_type)

matched_irbb_rfid_video2 <- matched_irbb_rfid_video %>% 
  bind_rows(
    
    rfid_irbb_pre_proc %>%
      as_tibble() %>%
      dplyr::filter(data_type == "RFID") %>% 
      dplyr::mutate(
        hours = as.numeric(hour(event_datetime_ms))
      ) %>%
      dplyr::filter(
        hours > 18
      ) %>% 
      pivot_wider(
        names_from = "data_type",
        values_from = "event_datetime_ms"
      ) %>% 
      dplyr::mutate(
        lead_bb_timestamp = NA,
        rear_bb_timestamp = NA,
        Video = NA,
        video_rfid_diffs = NA,
        direction = NA,
        unique_event = NA,
        integration_lower_temporal_thresh = NA,
        integration_upper_temporal_thresh = NA
      ) %>% 
      dplyr::select(names(matched_irbb_rfid_video))
    
  )


glimpse(matched_irbb_rfid_video2)

# Write this out
matched_irbb_rfid_video2 %>% 
  write.csv(., file.path(out_path, "integrated_irbb_rfid_video_data.csv"), row.names = FALSE)
