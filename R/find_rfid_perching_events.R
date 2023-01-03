#' @title find_rfid_perching_events
#' @description Use the raw radio frequency identification (RFID) data to identify perching events (e.g. periods of time when an individual was perched on the RFID antenna). This function is performed for each unique passive integrated transponder (PIT) tag in the dataset. The function identifies runs of RFID detections separated by the given temporal threshold or less, then takes the first and last detection of each run and return these timestamps with start and end labels. Note that unlike the RFID pre-processing function, this function groups the data frame not only by PIT tag ID but also by date to avoid artificially long perching periods
#' 
#' @param rfid_col_nm A string with the column name for the RFID events
#' #' @param group_col_nm A string with the column name that contains PIT tag identifiers, so that perching events are identified for each unique PIT tag. This column name is used to group the data frame by unique PIT tags
#' @param threshold A single numeric value. This represents a temporal threshold in seconds that will be used to identify RFID detections that occurred in close succession (e.g. within 1 or 2 seconds) as perching events
#'
#' @return A data frame object with all metadata columns in the original data frame, as well as columns indicating the timestamp of the start of the perching period identified, the end of the given perching period, and the temporal threshold used for pre-processing (in seconds). In other words, each row is a perching period


# Testing
# threshold <- 2 # seconds
# group_col_nm <- "PIT_tag_ID"

# This function will group by date AND the specified column. The argument group_col_nm cannot be NULL here
find_rfid_perching_events <- function(.x, rfid_col_nm, group_col_nm, threshold){
  
  # Check that the raw data is a data frame
  if(!is.data.frame(.x)){
    stop('The input object needs to be a data frame')
  }
  
  # Check that the temporal threshold is a number
  if(!is.integer(threshold)){
    stop('The temporal threshold needs to be an integer in seconds')
  }
  
  # Check that the input dataset has the PIT tag ID column that will be grouped
  if(!group_col_nm %in% names(.x)){
    stop('The column name specified as `group_col_nm` does not exist')
  }
  
  # Check that the timestamps are in the right format TKTK
  # if(!class(.x$rfid_col_nm))
  
  # Need to look for perching events by PIT tag AND day
  # Otherwise the logic below ends up including the last event of a day and the first of the next day as the start and end indices, which leads to strangely long perching periods sometimes
  tmp_df <- 
    .x %>%
    # pct_df3 %>% # testing
    # dplyr::filter(data_type == "RFID") %>%
    dplyr::mutate(
      date = paste(month, day, sep = " - ")
    ) %>%
    group_by(date, !!sym(group_col_nm)) %>%
    dplyr::arrange(event_datetime_ms, .by_group = TRUE) %>% 
    # Make unique row indices within groups
    dplyr::mutate(
      group_row_id = row_number()
    ) %>% 
    dplyr::rename(
      `group_col` = all_of(group_col_nm)
    )
  
  # glimpse(tmp_df)
  
  # # Checking
  # tmp_df %>% 
  #   pull(group_row_id) %>% 
  #   table() %>% 
  #   head()
  
  
  # The lags are calculated per group in the grouped data frame
  lags <- tmp_df %>% 
    dplyr::mutate(
      shift = dplyr::lag(event_datetime_ms, default = first(event_datetime_ms))
    ) %>% 
    # Convert differences to boolean based on a threshold to be able to remove stretches of detection events very close together
    dplyr::mutate(
      diff = floor(event_datetime_ms - shift),
      diff = as.numeric(diff),
      binary_diff = (diff <= threshold) # Taking anything less than or equal to the threshold, see previous RFID pre-processing
    ) %>% 
    dplyr::select(date, group_col, event_datetime_ms, diff, binary_diff) %>% 
    dplyr::rename(
      `dates` = "date",
      `grouping_col` = "group_col"
    )
  
  # glimpse(lags)
  
  # Make a data frame of the first and last indices of each run longer than 2 events that contain values below or equal to the given threshold
  lags_runs <- lags %>% 
    dplyr::summarise(
      first_indices = cumsum(rle(binary_diff)[["lengths"]]) - (rle(binary_diff)[["lengths"]] - 1),
      last_indices = cumsum(rle(binary_diff)[["lengths"]]),
      run_values = rle(binary_diff)[["values"]],
      run_lengths = rle(binary_diff)[["lengths"]]
    ) %>% 
    dplyr::filter(run_values & run_lengths >= 2) %>% 
    ungroup()
  
  # glimpse(lags_runs)
  
  # Per group, retain the first and last indices of RFID detections flagged as perching events
  # For some reason nesting and subsequent filtering doesn't work with 2 group variables, even when these are pasted together, so I used pmap_dfr to iterate over rows in lags_runs instead
  filt_df <- lags_runs %>%
    dplyr::select(dates, grouping_col, first_indices, last_indices) %>% 
    pmap_dfr(., function(dates, grouping_col, first_indices, last_indices){
      
      starts <- tmp_df %>%
        ungroup() %>%
        dplyr::filter(date == dates & group_col == grouping_col) %>%
        slice(c(first_indices)) %>%
        pull(event_datetime_ms)
      
      ends <- tmp_df %>%
        ungroup() %>%
        dplyr::filter(date == dates & group_col == grouping_col) %>%
        slice(c(last_indices)) %>%
        pull(event_datetime_ms)
      
      return(data.frame(
        dates = dates,
        grouping_col = grouping_col,
        perching_starts = starts,
        perching_ends = ends
        
      ))
      
    }) %>% 
    as_tibble() %>% 
    # Make sure to add a column with the temporal threshold used
    dplyr::mutate(
      perching_temporal_thresh = threshold
    )
  
  # What is this!
  # 4 - 3  01-10-3F-65-01  2022-04-03 12:45:23  2022-04-03 14:46:56
  
  # row index 144
  # 4 - 3  01-10-3F-65-01  60  63  TRUE
  
  # View(lags)
  # View(lags_runs)
  # View(filt_df)
  
  # glimpse(filt_df)
  
  # Checking for grouped data, looks good
  # tmp_df %>%
  #   dplyr::summarise(
  #     n = n()
  #   ) %>%
  #   dplyr::inner_join(
  #     lags_runs %>%
  #       group_by(date, group_col) %>%
  #       dplyr::summarise(
  #         n_remain = n()
  #       ),
  #     by = c("date", "group_col")
  #   ) %>%
  #   dplyr::inner_join(
  #     filt_df %>%
  #       group_by(date, group_col) %>%
  #       dplyr::summarise(
  #         n_filtered = n()
  #       ),
  #     by = c("date", "group_col")
  #   )
  
  
  # Rename the group column back to its original name
  names(filt_df)[grep("grouping_col", names(filt_df))] <- group_col_nm
  
  return(filt_df)
  
}

# # Apply the function above
# rfid_perching <- pct_df3 %>% 
#   dplyr::filter(data_type == "RFID") %>% 
#   dplyr::mutate(grping = 1) %>% 
#   group_split(grping) %>% 
#   map_dfr(
#     ~ find_rfid_perching_events(.x, threshold = 1, group_col_nm = "PIT_tag_ID")
#   )
# 
# glimpse(rfid_perching)
# 
# rfid_perching %>% 
#   # head(n = 100) %>% 
#   View()
# 
# # Write this data out
# rfid_perching %>%
#   write.csv(., file.path(out_path, "rfid_perching_data.csv"), row.names = FALSE)