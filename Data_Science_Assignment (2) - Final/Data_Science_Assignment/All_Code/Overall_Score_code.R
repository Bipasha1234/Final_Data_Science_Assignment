#Overall Score Calculation ----------- 
library(tidyverse)

# Load and standardize datasets
houseprices_ranked = read_csv("C:/Users/USER/Desktop/Recommended System/House Pricing Ranks.csv") %>%
  mutate(`Town/City` = tools::toTitleCase(tolower(`Town/City`)))

school_grades_ranking = read_csv("C:/Users/USER/Desktop/Recommended System/School Grades Ranks.csv") %>%
  mutate(`Town/City` = tools::toTitleCase(tolower(`Town/City`)))

crime_ranking = read_csv("C:/Users/USER/Desktop/Recommended System/Crime Ranking.csv") %>%
  mutate(`Town/City` = tools::toTitleCase(tolower(`Town/City`)))

broadband_ranking = read_csv("C:/Users/USER/Desktop/Recommended System/Broadband Speed Ranks.csv") %>%
  mutate(`Town/City` = tools::toTitleCase(tolower(`Town/City`)))

# Combine datasets using full_join to include all towns/cities
combined_scores = houseprices_ranked %>%
  full_join(broadband_ranking, by = "Town/City") %>%
  full_join(school_grades_ranking, by = "Town/City") %>%
  full_join(crime_ranking, by = "Town/City") %>%
  mutate(
    # Handle missing values
    HouseScore = ifelse(is.na(HouseScore), 0, HouseScore),
    BroadbandScore = ifelse(is.na(BroadbandScore), 0, BroadbandScore),
    SchoolScore = ifelse(is.na(SchoolScore), 0, SchoolScore),
    CrimeScore = ifelse(is.na(CrimeScore), 0, CrimeScore),
    # Calculate the overall score
    Overall_Score = (HouseScore + BroadbandScore + SchoolScore - CrimeScore) / 4 # Adjust weighting as needed
  ) %>%
  # Filter to include only City of Bristol and Cornwall
  filter(County.x %in% c("CITY OF BRISTOL", "CORNWALL")) %>%
  arrange(desc(Overall_Score)) %>%
  select(`Town/City`, County.x, HouseScore, BroadbandScore, SchoolScore, CrimeScore, Overall_Score)
View(combined_scores)
write_csv(combined_scores, "C:/Users/USER/Desktop/Recommended System/Overall_Scores.csv")
