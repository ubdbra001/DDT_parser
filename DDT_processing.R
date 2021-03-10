library(tidyverse)
library(psych)

source("kValExtract.R")

filepath = "C:/Users/Daniel/Downloads/Delay Discounting.csv"

filepath %>%
  read_csv() %>% # Load csv file
  # Two functions to 'fix' column names
  rename_with(str_replace_all, pattern = " ", replacement = "_") %>%
  rename_with(str_remove_all, pattern = "`") %>%
  # Remove variables that are not of interest here
  select(Event_Index, Participant_Private_ID, Spreadsheet_Row, Trial_Number,
         Zone_Name, Zone_Type, display, Response, Amount1, Amount2) ->
  DDT_data

DDT_data %>%
  drop_na(Participant_Private_ID) %>%
  # Extract text for Value, Month, and Week
  mutate(Value1 = str_extract(Amount1, "(?<=£)[:digit:]{1,3}"),
         Value2 = str_extract(Amount2, "(?<=£)[:digit:]{1,3}"),
         Time1 = str_extract(Amount1, "[:digit:]{1,3}(?= day)"),
         Time2 = str_extract(Amount2, "[:digit:]{1,3}(?= day)"),
         ResponseVal = str_extract(Response, "(?<=£)[:digit:]{1,3}")) %>%
  # Convert values to numbers and replace NAs with 0s
  mutate(across(ends_with(c("1", "2")), parse_number),
         across(ends_with(c("1", "2")), replace_na, replace = 0),
         ResponseVal = parse_number(ResponseVal)) %>%
  # Set the next mutate operations to be rowwise
  rowwise() %>%
  # Calculate variables for k
  mutate(A = max(Value1, Value2),
         V = min(Value1, Value2),
         D = abs(Time1 - Time2),
         k = ((A/V)-1)/D,
         HighVal = if_else(A==Value1, Value1, Value2),
         LDR = if_else(ResponseVal == HighVal, 1, 0),
         check = (Value1 >= Value2 && Time1 > Time2) ||
                 (Value1 <= Value2 && Time1 < Time2)) %>%
  ungroup() %>%
  # Split the data up by participant ID for further processing
  group_split(Participant_Private_ID) ->
  DDT_list

DDT_out <- map_df(DDT_list, kValExtract)

filename_out <- str_replace("DDT_dataOut_%s.csv", "%s",
                            format(Sys.time(), "%Y%m%d%H%M"))

write_csv(DDT_out, filename_out)
