library(tidyverse)
library(psych)

source("kValExtract.R")

filepath = "C:/Users/Daniel/Downloads/data_exp_19488-v17_task-6y5m.csv"

filepath %>%
  read_csv() %>% # Load csv file
  # Two functions to 'fix' column names
  rename_with(str_replace_all, pattern = " ", replacement = "_") %>%
  rename_with(str_remove_all, pattern = "`") %>%
  # Remove variables that are not of interest here
  select(Event_Index, Participant_Private_ID, Spreadsheet_Row, Trial_Number,
         Zone_Name, Zone_Type, display, Response, Choice_1, Choice_2) ->
  DDT_data

DDT_data %>%
  drop_na(Participant_Private_ID) %>%
  # Extract text for Value, Month, and Week
  mutate(Value1 = str_extract(Choice_1, "[:digit:]{1,3}(?= in)"),
         Month1 = str_extract(Choice_1, "[:digit:]{1,3}(?= month)"),
         Week1 = str_extract(Choice_1, "[:digit:]{1,3}(?= week)"),
         Value2 = str_extract(Choice_2, "[:digit:]{1,3}(?= in)"),
         Month2 = str_extract(Choice_2, "[:digit:]{1,3}(?= month)"),
         Week2 = str_extract(Choice_2, "[:digit:]{1,3}(?= week)"),
         .keep = "unused") %>%
  # Convert values to numbers and replace NAs with 0s
  mutate(across(ends_with(c("1", "2")), parse_number),
         across(ends_with(c("1", "2")), replace_na, replace = 0)) %>%
  # Calculate time in Days
  mutate(Day1 = Month1 * 30 + Week1 * 7,
         Day2 = Month2 * 30 + Week2 * 7) %>%
  # Set the next mutate operations to be rowwise
  rowwise() %>%
  # Calculate variables for k
  mutate(A = max(Value1, Value2),
         V = min(Value1, Value2),
         D = abs(Day1 - Day2),
         k = ((A/V)-1)/D,
         HighVal = if_else(A==Value1, "Choice 1", "Choice 2"),
         LDR = if_else(Response == HighVal, 1, 0)) %>%
  ungroup() %>%
  # Split the data up by participant ID for further processing
  group_split(Participant_Private_ID) ->
  DDT_list

DDT_out <- map_df(DDT_list, kValExtract)

filename_out <- str_replace("DDT_dataOut_%s.csv", "%s",
                            format(Sys.time(), "%Y%m%d%H%M"))

write_csv(DDT_out, filename_out)
