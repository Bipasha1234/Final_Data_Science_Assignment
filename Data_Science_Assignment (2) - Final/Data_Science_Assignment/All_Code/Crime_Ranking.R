library(tidyverse)
library(dplyr)

# Import cleaned crime data
crime_data_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Crime_Cleaned.csv")

# Ensure 'Crime Date' is correctly handled and extract the year
crime_data_2023 = crime_data_cleaned %>%
  mutate(`Crime Date` = substr(`Crime Date`, 1, 4)) %>% # Extracting the year from 'Crime Date'
  filter(`Crime Date` == "2023") %>% # Filtering for the year 2023
  group_by(`Town/City`,  `County`) %>% # Grouping by relevant columns
  tally() %>% # Count occurrences
  rename(`Crime Count` = n) # Renaming the count column

# Calculate maximum crime count for normalization
max_crime_count = max(crime_data_2023$`Crime Count`)

# Create a ranking based on crime count, with highest counts getting the highest scores
crime_ranking = crime_data_2023 %>%
  arrange(desc(`Crime Count`)) %>% # Sorting by crime count in descending order
  mutate(CrimeScore = (`Crime Count` / max_crime_count) * 10) %>% # Calculate the ranking score
  select(`Town/City`, `County`, `CrimeScore`) # Selecting relevant columns

# Define path to save the crime ranking csv
crime_ranking_csv_path = "C:/Users/USER/Desktop/Recommended System/Crime Ranking.csv"

# Save the crime ranking dataset
write.csv(crime_ranking, crime_ranking_csv_path, row.names = FALSE)
