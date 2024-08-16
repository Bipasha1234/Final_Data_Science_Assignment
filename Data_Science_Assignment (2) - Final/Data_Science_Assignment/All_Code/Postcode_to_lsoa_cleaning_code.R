library(tidyverse)
library(dplyr)
library(lubridate)

# cleaned house price dataset  -  load
cleaned_data_houseprices = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Cleaned_houseprices.csv")

# Postcode to LSOA - load and clean
postcode_to_lsoa_cleaned = read_csv("D:/Data_Science_Assignment/Obtained_Data/Postcode_to_LSOA_dataset/Postcode to LSOA.csv") %>%
  mutate(S_No = row_number()) %>%  #adding serial no.
  select(S_No, pcd7, lsoa11cd) %>%  # select the important columns only
  rename(Postcode = pcd7, LSOA_Code = lsoa11cd) %>%  # Renaming the columns
  right_join(cleaned_data_houseprices, by = "Postcode") %>%  # Merging with the cleaned house prices csv on Postcode
  select(LSOA_Code, Postcode, `Short Postcode`,`Town/City`, District, County)%>% # select only needed columns
  filter(County == "CITY OF BRISTOL" | County == "CORNWALL") # only filtering the county that we need and we will work on.

# View the cleaned postcode to LSOA csv
View(postcode_to_lsoa_cleaned)

# File path for saving the cleaned csv
postcode_to_lsoa_path = "D:/Data_Science_Assignment/Cleaned_Data/postcode_to_lsoa_cleaned.csv"

# Saving the dataset.
write.csv(postcode_to_lsoa_cleaned, postcode_to_lsoa_path, row.names = FALSE)
