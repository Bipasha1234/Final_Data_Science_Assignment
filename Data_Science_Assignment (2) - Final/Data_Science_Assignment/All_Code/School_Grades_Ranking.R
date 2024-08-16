library(tidyverse)
library(dplyr)
library(scales)
library(ggrepel)

# Ranking the School Grades

# Import cleaned school grades csv
school_grades_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/School_Dataset_Cleaned.csv")
View(school_grades_cleaned)
# Rank school grades by Town/City
school_grades_ranked = school_grades_cleaned %>% 
  filter(Year == 2023) %>%  # Filtering for the year 2023
  group_by(`Town`) %>% 
  summarise(`Average Attainment Score` = mean(`Attainment Score`), 
            County = first(County)) %>% # Simplifying the table by grouping towns that are in the same county
  arrange(desc(`Average Attainment Score`)) %>% 
  rename(`Town/City` = Town) %>% # Renaming Town to Town/City
  # Calculate the score, assuming higher attainment scores should have a higher rank
  mutate(SchoolScore = (`Average Attainment Score` / 10)) %>%  # Calculating the score for school grades
  select(`Town/City`, County, SchoolScore)

# Define path to save the school grades ranking csv
school_grades_ranking_csv_path = "C:/Users/USER/Desktop/Recommended System/School Grades Ranks.csv"

# Save the cleaned dataset
write.csv(school_grades_ranked, school_grades_ranking_csv_path, row.names = FALSE)
