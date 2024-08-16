library(tidyverse)
library(dplyr)
library(scales)
library(ggrepel)

#Ranking the House Price
# Import cleaned population csv
population_data_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Population_Cleaned.csv")
population_data_cleaned = population_data_cleaned %>% 
  select(`Short Postcode`, `Town/City`, District, County)

# Import cleaned house price csv
houseprices_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Cleaned_houseprices.csv")

houseprices_ranked= houseprices_cleaned %>% 
  filter(`Date of Transfer` == "2023") %>% 
  group_by(`Town/City`) %>% 
  summarise(Price=mean(Price),County=first(County)) %>% #simplifying the table by grouping towns that are in the same county
  arrange(Price) %>% 
  #substracting by 10 because lower house prices should have higher rank----
  mutate(HouseScore=10-(Price/100000)) %>%  # Calculating the score for house prices
  select(`Town/City`,County, HouseScore)

house_ranking_csv_path = "C:/Users/USER/Desktop/Recommended System/House Pricing Ranks.csv"

write.csv(houseprices_ranked, house_ranking_csv_path, row.names = FALSE)







