library(tidyverse)
library(dplyr)
library(lubridate)

# Set the path to the directory containing all the year-month subdirectories
data_directory = "D:/Data_Science_Assignment/Obtained_Data/Crime_dataset"

# Create a pipeline to generate the final combined dataset
merged_crime_data = list.files(data_directory, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE) %>%
  lapply(read.csv) %>%  # Read each CSV file into a dataframe
  bind_rows() %>%       # Combine all the dataframes into one
  as_tibble()           # Convert the final dataframe into a tibble

View(merged_crime_data)

# Load the cleaned postcode to LSOA dataset.
postcode_lsoa_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/postcode_to_lsoa_cleaned.csv")

# cleaning Process of the final crime dataset
final_crime_dataset = merged_crime_data %>%
  select(LSOA.code, Falls.within, Month, Crime.type) %>%  # Keep only relevant columns
  rename(`Crime Date`= `Month`, LSOA_Code= LSOA.code) %>%  # Rename columns 
  right_join(postcode_lsoa_cleaned, join_by(LSOA_Code)) %>% # Merge with postcode mapping to get town/city information
  mutate(S_No = row_number()) %>%
  select(`Crime Date`, Falls.within, Crime.type, LSOA_Code, `Postcode`, `Short Postcode`,`Town/City`,County) %>%  # selcting onle the essential columns
  na.omit() %>% # Remove missing values
  distinct() 

View(final_crime_dataset)

# Define the file path for saving the cleaned dataset.
output_file_path = "D:/Data_Science_Assignment/Cleaned_Data/Crime_Cleaned.csv"

# Save the cleaned dataset to a CSV file.
write.csv(final_crime_dataset, output_file_path, row.names = FALSE)









