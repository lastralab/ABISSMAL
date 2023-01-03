#' @title preprocess_rfid_beamBreaker_events
#' @description Pre-process raw radio frequency identification (RFID) and beam breaker data by thinning. Events detected by a single sensor are compared to each other, and adjacent events that occurred within a specified temporal threshold are filtered out. For RFID data, this data thinning is performed for each unique passive integrated transponder (PIT) tag in the dataset. 
#' 
#' @param threshold A single numeric value representing a temporal threshold in seconds that will be used to thin the raw data
#' @param group_col_nm A character object that represents the column name that will be used to group raw data prior to thinning. This column should be used for RFID data to indicate the column name that contains PIT tag identifiers, so that data thinning is performed for each unique PIT tag

#' @return A data frame object with the pre-processed detections per sensor, all metadata columns in the original data frame, as well as a column indicating the temporal threshold used for pre-processing (in seconds). Each row of this data frame is a pre-processed detection event


# testing
# threshold <- 2 # testing
# group_col_nm <- "PIT_tag_ID"
# group_col_nm <- NULL


preprocess_rfid_beamBreaker_events <- function(.x, threshold, group_col_nm){
  
  if(
    .x %>% pull(data_type) %>%
    unique() == "RFID" & !is.null(group_col_nm)
  ) {
    # if(
    #   pct_df3 %>%
    #   dplyr::filter(data_type == "RFID") %>% 
    #   pull(data_type) %>% unique() == "RFID"
    # ){ # testing
    tmp_df <- .x %>%
      # pct_df3 %>% # testing
      # dplyr::filter(data_type == "RFID") %>%
      group_by(!!sym(group_col_nm)) %>%
      dplyr::arrange(event_datetime_ms, .by_group = TRUE) %>% 
      # Make unique row indices within groups
      dplyr::mutate(
        group_row_id = row_number()
      ) %>% 
      dplyr::rename(
        `group_col` = all_of(group_col_nm)
      )
    
    # glimpse(tmp_df)
    
  } else {
    tmp_df <- .x %>% 
      # pct_df3 %>% # testing
      # dplyr::filter(data_type == "beam breaker : lead") %>%
      dplyr::arrange(event_datetime_ms) %>% 
      # Make unique row indices with the same column name as the group indices above
      dplyr::mutate(
        group_row_id = row_number()
      )
  }
  
  # Looks good
  # tmp_df %>% 
  #   pull(group_row_id) %>% 
  #   table() %>% 
  #   head()
  
  # glimpse(tmp_df)
  
  # If the group_col is specified, then the lags are calculated per group in the grouped data frame
  lags <- tmp_df %>% 
    dplyr::mutate(
      shift = dplyr::lag(event_datetime_ms, default = first(event_datetime_ms))
    ) %>% 
    # Convert differences to boolean based on a threshold to be able to remove stretches of detection events very close together
    dplyr::mutate(
      diff = floor(event_datetime_ms - shift),
      diff = as.numeric(diff),
      binary_diff = (diff >= threshold)
    ) 
  
  if(!is.null(group_col_nm)){
    lags <- lags %>% 
      dplyr::select(event_datetime_ms, group_col, diff, binary_diff) 
  } else {
    lags <- lags %>% 
      dplyr::select(event_datetime_ms, diff, binary_diff) 
  }
  
  # Nest by each group, do the rle calculations and removing indices, then recombine
  lags_runs <- lags %>% 
    dplyr::summarise(
      run_indices = cumsum(rle(binary_diff)[["lengths"]]),
      run_values = rle(binary_diff)[["values"]],
      run_lengths = rle(binary_diff)[["lengths"]]
    ) %>% 
    dplyr::filter(!run_values) 
  
  if(!is.null(group_col_nm)){
    
    lags_runs2 <- lags_runs %>% 
      dplyr::select(group_col, run_values, run_lengths, run_indices) %>% 
      pmap_dfr(., function(group_col, run_values, run_lengths, run_indices){
        # In the runs of FALSE values, remove all including the first index. Should work for all runs with length == 1 or > 1
        return(
          data.frame(
            group_col = group_col,
            rem_indices = seq((run_indices - (run_lengths - 1)), run_indices, 1)
          )
        )
      }) %>% 
      # Make sure to drop the first index per group, since these first observations should be retained
      dplyr::filter(rem_indices != 1)
    
    # glimpse(lags_runs2)
    
    # Per group, remove the indices that represent the RFID detections too close together
    filt_df <- tmp_df %>% 
      nest() %>%
      # Filter each nested data frame by the correct row indices per group
      dplyr::mutate(
        filtered = map(.x = data, .f = ~ dplyr::filter(.x, !group_row_id %in% lags_runs2$rem_indices[group_col == lags_runs2$group_col]))
      ) %>% 
      # Return the filtered data frames as a single data frame
      unnest(`cols` = c(filtered)) %>% 
      dplyr::select(-c(data)) %>% 
      ungroup() %>% 
      # Make sure to add a column with the temporal threshold used
      dplyr::mutate(
        preProc_temporal_thresh = threshold
      )
    
    # Rename the group column back to its original name
    names(filt_df)[grep("group_col", names(filt_df))] <- group_col_nm
    
    # glimpse(filt_df)
    
    # Checking for grouped data, looks good
    # tmp_df %>%
    #   dplyr::summarise(
    #     n = n()
    #   ) %>%
    #   dplyr::inner_join(
    #     lags_runs2 %>%
    #       group_by(group_col) %>%
    #       dplyr::summarise(
    #         n_sub = n()
    #       ),
    #     by = "group_col"
    #   ) %>%
    #   dplyr::mutate(
    #     filt_n = n - n_sub
    #   )
    
    
  } else {
    
    lags_runs2 <- lags_runs %>% 
      dplyr::select(run_values, run_lengths, run_indices) %>% 
      pmap_dfr(., function(run_values, run_lengths, run_indices){
        
        return(
          data.frame(
            rem_indices = seq((run_indices - (run_lengths - 1)), run_indices, 1)
          )
        )
      }) %>% 
      dplyr::filter(rem_indices != 1)
    
    
    filt_df <- tmp_df %>% 
      # Filter the data frame by the correct row indices
      dplyr::filter(!group_row_id %in% lags_runs2$rem_indices) %>% 
      # Remove the grouping column
      dplyr::select(-c(group_col)) %>% 
      # Make sure to add a column with the temporal threshold used
      dplyr::mutate(
        preProc_temporal_thresh = threshold
      )
    
    # glimpse(filt_df)
    
    # Checking, looks good
    # (nrow(tmp_df) - nrow(filt_df)) == nrow(lags_runs2)
    
  }
  
  return(filt_df)
  
}

# Pre-process the RFID and beam breaker data
rfid_irbb_pre_proc <- pct_df3 %>% 
  dplyr::filter(data_type == "RFID") %>% 
  # glimpse()
  # dplyr::select(data_type, event_datetime) %>% 
  group_split(data_type) %>%
  map_dfr(
    ~ preprocess_rfid_beamBreaker_events(.x, threshold = 2, group_col_nm = "PIT_tag_ID")
  ) %>%
  # The beam breaker data is missing the PIT tag ID column, and this should be filled with NAs after the row bind
  bind_rows(
    pct_df3 %>% 
      dplyr::filter(data_type %in% c("beam breaker : lead", "beam breaker : rear")) %>% 
      # dplyr::select(data_type, event_datetime) %>% 
      group_split(data_type) %>%
      map_dfr(
        ~ preprocess_rfid_beamBreaker_events(.x, threshold = 5, group_col_nm = NULL)
      )
  )

glimpse(rfid_irbb_pre_proc)
head(rfid_irbb_pre_proc)

# Checking the beam breakers have NAs in the PIT tag column, looks good
rfid_irbb_pre_proc %>% 
  dplyr::filter(data_type != "RFID") %>% 
  pull(PIT_tag_ID) %>% 
  unique()

# Threshold values look good too
rfid_irbb_pre_proc %>% 
  group_by(data_type) %>% 
  dplyr::summarise(threshold = unique(preProc_temporal_thresh))

# Checking. There should no longer be lags below 2 seconds for RFID, and 5 seconds for IRBB, looks good. NEED TO UPDATE GROUPING FOR RFID

# Checking RFID pre-processing, looks good
rfid_irbb_pre_proc %>% 
  dplyr::filter(data_type == "RFID") %>% 
  dplyr::mutate(data_type = factor(data_type)) %>%
  group_by(data_type, PIT_tag_ID) %>%
  dplyr::arrange(event_datetime_ms) %>% 
  dplyr::mutate(
    shift = lag(event_datetime_ms), default = first(event_datetime_ms),
    diff = round(as.numeric(floor(event_datetime_ms - shift)))
  ) %>% 
  group_map(
    ~ range(.x$diff, na.rm = TRUE)
  )

# Checking beam breaker pre-processing, also looks good
rfid_irbb_pre_proc %>% 
  dplyr::filter(data_type != "RFID") %>% 
  dplyr::mutate(data_type = factor(data_type)) %>%
  group_by(data_type) %>%
  dplyr::arrange(event_datetime_ms) %>% 
  dplyr::mutate(
    shift = lag(event_datetime_ms), default = first(event_datetime_ms),
    diff = round(as.numeric(floor(event_datetime_ms - shift)))
  ) %>% 
  group_map(
    ~ range(.x$diff, na.rm = TRUE)
  )