library(tidyverse)
library(dplyr)
library(scales)
library(ggrepel)

# Ranking the Broadband Speed

# Import cleaned broadband speed csv
broadband_speed_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Broadband_speed_cleaned_data.csv")

# Rank broadband speeds by Town/City
broadband_speed_ranked = broadband_speed_cleaned %>% 
  group_by(`Town/City`) %>% 
  summarise(`Average Download Speed` = mean(`Average download speed (Mbit/s)`), 
            County = first(County)) %>% # Simplifying the table by grouping towns that are in the same county
  arrange(`Average Download Speed`) %>% 
  #lower speeds should have a lower rank----
mutate(BroadbandScore = (`Average Download Speed` / 10)) %>%  # Calculating the score for broadband speed
  select(`Town/City`, County, BroadbandScore)

# Define path to save the broadband ranking csv
broadband_ranking_csv_path = "C:/Users/USER/Desktop/Recommended System/Broadband Speed Ranks.csv"

# Save the cleaned dataset
write.csv(broadband_speed_ranked, broadband_ranking_csv_path, row.names = FALSE)







