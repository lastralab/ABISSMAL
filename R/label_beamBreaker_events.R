#' @title label_beamBreaker_events
#' @description Use data from two pairs of beam breakers to label either entrance and exit movements (in or out of a nest container or nesting area).
#' 
#' @param threshold A single numeric value representing a temporal threshold in seconds that will be used to assign events across the beam breaker pairs to unique entrance or exit movements
#' @param col_nm A character argument that represents the column name containing lag times between the pairs of beam breakers
#' @param type A character argument that specifies which type of movement should be identified (e.g. "entrance" or "exit")
#' 
#' @return A data frame object with all metadata columns in the original data frame, as well as columns indicating each of the timestamps of the lead and rear beam breaker pairs, and a unique label for the given event (e.g. entrance or exit plus a unique numeric identifier). In other words, each row is a labeled event across the beam breaker pairs
#' 
#' TKTK needs a column for the threshold used

# Make a function to label entrances and exits
label_beamBreaker_events <- function(.x, threshold, col_name, type){
  
  tmp_df <- .x %>% 
    # pct_pp2 %>% # testing
    # Convert differences to boolean based on a threshold (in seconds)
    dplyr::mutate(
      binary_vals = (abs((round(!!sym(col_name), 2))) <= threshold)
    ) %>% 
    dplyr::select(`beam breaker : lead`, `beam breaker : rear`, !!sym(col_name), binary_vals) 
  
  # Find indices of entrances for lead beam breaker, or indices of exits for the rear beam breaker 
  e1 <- which(tmp_df$binary_vals)
  # head(e1)
  # length(e1)  
  
  # Find indices of entrances for rear beam breaker, or indices of exits for the lead beam breaker
  # Make sure to add 1
  e2 <- e1 + 1
  # head(e2)
  
  # Now use these indices to filter the data frame for entrances or exits
  if(type == "entrance"){
    
    res_df <- data.frame(
      lead_bb_timestamp = tmp_df %>% 
        slice(e1) %>% 
        pull(`beam breaker : lead`),
      rear_bb_timestamp = tmp_df %>% 
        slice(e2) %>% 
        pull(`beam breaker : rear`)
    ) 
    # Double-checking. No rounded absolute differences in timestamps greater than 0, looks good!
    # %>% 
    #   dplyr::mutate(
    #     direction = paste(type, seq(1, nrow(.), ), sep = "-")
    #   ) %>% dplyr::mutate(
    #   # Check entrances
    #   diff = abs(round(as.numeric(lead_bb_timestamp - rear_bb_timestamp), 2))
    # ) %>%
    #   dplyr::filter(
    #     diff > 1
    #   )
    
    # head(res_df)
    
  } else if(type == "exit"){
    
    res_df <- data.frame(
      lead_bb_timestamp = tmp_df %>% 
        slice(e2) %>% 
        pull(`beam breaker : lead`),
      rear_bb_timestamp = tmp_df %>% 
        slice(e1) %>% 
        pull(`beam breaker : rear`)
    )  
    # Double-checking. No rounded absolute differences in timestamps greater than 0, looks good!
    # %>% dplyr::mutate(
    #   # Check exits
    #   diff = abs(round(as.numeric(rear_bb_timestamp - lead_bb_timestamp), 2))
    # ) %>%
    #   dplyr::filter(
    #     diff > 1
    #   )
    
    # head(res_df)
    
  }
  
  return(
    res_df %>% 
      dplyr::mutate(
        unique_beamBreak_event = paste(type, seq(1, nrow(.), ), sep = "-")
      )
  )
  
}

glimpse(pct_pp2)

# Get the beam breaker entrances 
irbb_entrances_exits <- pct_pp2 %>% 
  # Make a fake grouping column to place all rows into the same group for map_dfr works
  dplyr::mutate(
    grping = 1
  ) %>% 
  group_split(grping) %>%
  map_dfr(
    ~ label_beamBreaker_events(.x, col_name = "lead_rear_diffs", type = "entrance", threshold = 3)
  ) %>% 
  bind_rows(
    pct_pp2 %>% 
      # Make a fake grouping column to place all rows into the same group for map_dfr works
      dplyr::mutate(
        grping = 1
      ) %>% 
      group_split(grping) %>%
      map_dfr(
        ~ label_beamBreaker_events(.x, col_name = "rear_lead_diffs", type = "exit", threshold = 3)
      )
  ) %>% 
  # Make a new column for type
  # Also make a new column for the differences to triple-check
  dplyr::mutate(
    direction = gsub("-([0-9]+)", "", unique_beamBreak_event),
    diff = as.numeric(lead_bb_timestamp - rear_bb_timestamp)
  )

glimpse(irbb_entrances_exits)

# Looks great. 2022-03-31 18:23:51 coded as an entrance, and is not also coded as an exit
irbb_entrances_exits %>% 
  dplyr::mutate(
    month = month(lead_bb_timestamp),
    day = day(lead_bb_timestamp)
  ) %>% 
  dplyr::filter(month == "3", day == "31") %>%
  dplyr::arrange(lead_bb_timestamp, rear_bb_timestamp, desc = FALSE) %>% 
  View()

# Checking. How many entrances and exits detected?
irbb_entrances_exits %>% 
  group_by(direction) %>% 
  dplyr::summarise(
    n = n()
  )

# Do all entrances have a negative difference in timestamps, and exits positive? # Yes, looks good
irbb_entrances_exits %>% 
  group_by(direction) %>% 
  dplyr::summarise(
    min_dff = min(diff),
    max_diff = max(diff)
  )

# Write these out
irbb_entrances_exits %>% 