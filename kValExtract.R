kValExtract <- function(data_in){
  
  end_practice <- filter(data_in, display == "Go on trials")$Event_Index
  
  # Extract only responses and remove practice trials
  data_in %>%
    filter(Zone_Name == "Zone4" | Zone_Name == "Zone5") %>%
    filter(Event_Index > end_practice) ->
    data_responses
  
  # Calculate proportion of check trials "correctly" answered
  data_responses %>%
    filter(Spreadsheet_Row == 28 | Spreadsheet_Row == 42) %>%
    # Set to NA if there are more than 2 trials
    summarise(ID = unique(Participant_Private_ID),
              checks_prop = if_else(n()>2, NA_real_, mean(LDR))) ->
    data_checks
  
  # Calculate the number of missing trials
  data_responses %>%
    filter(Spreadsheet_Row != 28 & Spreadsheet_Row != 42) %>%
    summarise(ID = unique(Participant_Private_ID),
              missing_trials = sum(is.na(Response))) ->
    data_missing
  
  # Prep data for IDing k-value
  data_responses %>%
    drop_na(Response) %>%
    filter(Spreadsheet_Row != 28 & Spreadsheet_Row != 42) %>%
    arrange(k) %>%
    mutate(lagged_k = lag(k)) %>%
    rowwise() %>%
    mutate(adj_k = geometric.mean(c(k, lagged_k))) %>%
    mutate(consistency = NA_integer_) %>%
    ungroup () ->
    prep_data
  
  n_rows <- nrow(prep_data)
  
  # If there are trials to process
  if (n_rows > 1) {
    # Calculate consistency for each response
    for (row_n in seq_len(n_rows)){
      if (row_n == 1) {
        prep_data$consistency[row_n] <- sum(prep_data$LDR[(row_n+1):n_rows] == 1)
      } else if (row_n == n_rows) {
        prep_data$consistency[row_n] <- sum(prep_data$LDR[1:(row_n-1)] == 0)
      } else {
        prep_data$consistency[row_n] <- sum(prep_data$LDR[(row_n+1):n_rows] == 1,
                                            prep_data$LDR[1:(row_n-1)] == 0)
      }
    }
    
    prep_data %>%
      # Find highest consistency k-values
      filter(consistency == max(consistency)) %>%
      # Summarise to show:
      # Overall geometric mean for k-values
      # Proportion for consistency
      # Total number of trials included
      summarise(ID = unique(Participant_Private_ID),
                k = geometric.mean(adj_k),
                consistency = max(consistency)/n_rows,
                included_trials = n_rows) %>%
      # Join all the data frames into a single output
      left_join(data_checks) %>%
      left_join(data_missing) ->
      indv_out
  } else {
    # If all the trials are missing
    data_missing %>%
      left_join(data_checks) %>%
      bind_cols(tibble(k = NA,
                       consistency = NA,
                       included_trials = 0)) ->
      indv_out
  }
  
  return(indv_out)
  
}