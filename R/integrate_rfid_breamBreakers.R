#' @title integrate_rfid_beamBreakers
#' @description Use lags between the pre-processed radio frequency identification (RFID) data and the pre-processed and labeled beam breaker data to integrate these two datasets. Each RFID detection that remains must be accompanied by a labeled event from the beam breaker dataset (e.g. an entrance or exit movement)
#' 
#' @param .x A data frame object that contains all the pre-processed RFID events, and all the pre-processed and labeled beam breaker data. Each row is therefore a unique event from either the RFIF or beam breaker dataset. This data frame must contain all the columns in the subsequent arguments
#' @param l_th A numeric argument. This represents a lower or minimum temporal threshold in seconds to identify RFID and beam breaker events that are close enough together for integration
#' @param u_th A numeric argument. This represents an upper or maximum temporal threshold in seconds to identify RFID and beam breaker events that are close enough together for integration
#' @param data_type_col A character value. This is the name of the metadata column that contains information about the data type (e.g. "data_type")
#' @param timestamps A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms")
#' @param names_from A character value. This is the name of the metadata column that contains information about the data type (e.g. "data_type"). Used for data frame structure manipulations
#' @param values_from A character value. The name of the column that contains timestamps in a format that supports calculations in milliseconds (e.g. "event_datetime_ms"). Used for data frame structure manipulations
#' @param lead_sensor_nm A character value. The column name that contains timestamps for the "lead" pair of beam breakers (e.g. the first pair of beam breakers an animal encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param rear_sensor_nm A character value. The column name that contains timestamps for the "rear" pair of beam breakers (e.g. the second pair of beam breakers an animal encounters when moving into a nest container or area). The data format must also support calculations in milliseconds
#' @param rfid_nm A character value. The column name that contains RFID timestamps. The data format must also support calculations in milliseconds
#' @param sensor_event_nm A character value. The name of column that contains unique identifiers for each beam breaker event (e.g. "entrance" followed by a unique numeric identifier)
#' @param PIT_tag_nm A character value. The name of column that contains the unique PIT tag identifiers detected with each RFID timestamp
#' 
#' @return A data frame object with all metadata columns in the original data frame, but each row represents a unique RFID detection that was assigned to a beam breaker event (e.g. unique entrances and exits)

# testing
# l_th <- -3 # testing
# u_th <- 1
# data_type_col <- "data_type"
# timestamps <- "event_datetime_ms"
# names_from <- "data_type"
# values_from <- "event_datetime_ms"
# lead_sensor_nm <- "lead_bb_timestamp"
# rear_sensor_nm <- "rear_bb_timestamp"
# rfid_nm <- "RFID"
# sensor_event_nm <- "unique_beamBreak_event"
# PIT_tag_nm <- "PIT_tag_ID"

integrate_rfid_beamBreakers <- function(.x, l_th, u_th, data_type_col, timestamps, names_from, values_from, lead_sensor_nm, rear_sensor_nm, rfid_nm, sensor_event_nm, PIT_tag_nm){
  
  res_df <- .x %>% 
    # pct_irbb_rfid %>% # testing
    dplyr::filter(data_type == "RFID") %>% 
    group_by(!!sym(PIT_tag_nm)) %>% 
    dplyr::rename(
      `group_col` = all_of(PIT_tag_nm)
    ) %>% 
    nest() %>% 
    dplyr::mutate(
      # Do the timestamp difference calculations
      lags = map(.x = data, .f = ~ bind_rows(
        .x,
        # Add back the beam breaker data to the subset data frame per PIT tag
        pct_irbb_rfid %>%
          dplyr::filter(grepl("bb", data_type))
      ) %>% 
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
          lagging_RFID = lag(!!sym(rfid_nm), default = first(!!sym(rfid_nm)))
        ) %>% 
        # Calculate the differences between the relevant pairs of timestamps: RFID compared to each beam breaker to find entrances and exits
        # The lags are calculated per group in the grouped data frame
        dplyr::mutate(
          # Here negative differences mean a beam breaker triggered first
          lead_rfid_diffs_ent = round(!!sym(lead_sensor_nm) - leading_RFID, 2),
          rear_rfid_diffs_ent = round(!!sym(rear_sensor_nm) - leading_RFID, 2),
          # Here negative differences mean the RFID antenna triggered first
          lead_rfid_diffs_exi = round(lagging_RFID - !!sym(lead_sensor_nm), 2),
          rear_rfid_diffs_exi = round(lagging_RFID - !!sym(rear_sensor_nm), 2)
        ) %>%
        # Convert differences to boolean based on a threshold (in seconds)
        dplyr::mutate(
          binary_lead_ent = (lead_rfid_diffs_ent >= l_th & lead_rfid_diffs_ent <= u_th),
          binary_rear_ent = (rear_rfid_diffs_ent >= l_th & rear_rfid_diffs_ent <= u_th),
          binary_lead_exi = (lead_rfid_diffs_exi >= l_th & lead_rfid_diffs_exi <= u_th),
          binary_rear_exi = (rear_rfid_diffs_exi >= l_th & rear_rfid_diffs_exi <= u_th)
        ) %>%
        dplyr::select(group_row_id, !!sym(lead_sensor_nm), !!sym(rear_sensor_nm), leading_RFID, lagging_RFID, lead_rfid_diffs_ent, rear_rfid_diffs_ent, lead_rfid_diffs_exi, rear_rfid_diffs_exi, binary_lead_ent, binary_rear_ent, binary_lead_exi, binary_rear_exi, direction, !!sym(sensor_event_nm), !!sym(PIT_tag_nm))
      )
    ) %>%
    # Do more mapping to perform the integration depending on the temporal thresholds calculated above
    # This is done per PIT tag, and the integration is done separately for entrances and exits
    dplyr::mutate(
      
      # Entrances
      matched_irbb_rfid = map(.x = lags, .f = ~ dplyr::mutate(
        .x,
        RFID = leading_RFID,
        lead_rfid_diffs = lead_rfid_diffs_ent,
        rear_rfid_diffs = rear_rfid_diffs_ent
      ) %>% 
        dplyr::filter(
          binary_lead_ent | binary_rear_ent
        ) %>% 
        dplyr::select(!!sym(lead_sensor_nm), !!sym(rear_sensor_nm), RFID, lead_rfid_diffs, rear_rfid_diffs, direction, !!sym(sensor_event_nm)) 
      %>% 
        # Exits
        bind_rows(
          .x %>% 
            dplyr::mutate(
              RFID = lagging_RFID,
              lead_rfid_diffs = lead_rfid_diffs_exi,
              rear_rfid_diffs = rear_rfid_diffs_exi
            ) %>% 
            dplyr::filter(
              binary_lead_exi | binary_rear_exi
            ) %>% 
            dplyr::select(!!sym(lead_sensor_nm), !!sym(rear_sensor_nm), RFID, lead_rfid_diffs, rear_rfid_diffs, direction, !!sym(sensor_event_nm))
        )
      )
    ) %>% 
    dplyr::select(-c(data, lags)) %>% 
    unnest(`cols` = c(matched_irbb_rfid)) %>%
    ungroup() %>% 
    # Make sure to add a column with the temporal threshold used
    dplyr::mutate(
      integration_lower_temporal_thresh = l_th,
      integration_upper_temporal_thresh = u_th
    )
  
  names(res_df)[grep("group_col", names(res_df))] <- PIT_tag_nm
  
  # glimpse(res_df)
  # View(res_df)
  
  return(res_df)
  
}


# Organize the pre-processed and labeled beam breaker data to join with the pre-processed RFID data
pct_irbb_rfid <- irbb_entrances_exits %>% 
  dplyr::select(lead_bb_timestamp, rear_bb_timestamp, direction, unique_beamBreak_event) %>% 
  pivot_longer(
    cols = c(lead_bb_timestamp, rear_bb_timestamp),
    names_to = "data_type",
    values_to = "event_datetime_ms"
  ) %>% 
  dplyr::mutate(
    PIT_tag_ID = NA
  ) %>%
  # Drop the NAs in timestamps. This should leave lead beam breaker timestamps for entrances and rear beam breaker timestamps for exits
  # Keep unassigned beam breaker events for now
  dplyr::filter(!is.na(event_datetime_ms)) %>% 
  dplyr::select(data_type, PIT_tag_ID, event_datetime_ms, direction, unique_beamBreak_event) %>% 
  # Add the pre-processed RFID data back to the pre-processed and labeled beam breaker data. Make sure to take the ms format timestamps for the RFID data
  bind_rows(
    pct_pp %>% 
      dplyr::filter(grepl("RFID", data_type)) %>%
      dplyr::mutate(
        direction = NA,
        unique_beamBreak_event = ""
      ) %>% 
      dplyr::select(data_type, PIT_tag_ID, event_datetime_ms, direction, unique_beamBreak_event)
  )

glimpse(pct_irbb_rfid)


# Thresholds in seconds
# Some beam breaker exits occur well after RFID detections, like 30 seconds to a minute or more
integrated_irbb_rfid <- pct_irbb_rfid %>% 
  # Make a fake grouping column to place all rows into the same group for map_dfr works
  dplyr::mutate(
    grping = 1
  ) %>% 
  group_split(grping) %>%
  map_dfr(
    ~ integrate_rfid_beamBreakers(
      .x, 
      l_th = -3, 
      u_th = 1, 
      data_type_col = "data_type", 
      timestamps = "event_datetime_ms", 
      names_from = "data_type", 
      values_from = "event_datetime_ms", 
      lead_sensor_nm = "lead_bb_timestamp", 
      rear_sensor_nm = "rear_bb_timestamp", 
      rfid_nm = "RFID", 
      sensor_event_nm = "unique_beamBreak_event", 
      PIT_tag_nm = "PIT_tag_ID"
    )
  )

glimpse(integrated_irbb_rfid)

# Write out the integrated beam breaker and RFID data
integrated_irbb_rfid %>% 
  write.csv(., file.path(out_path, "integrated_irbb_rfid_data.csv"), row.names = FALSE)