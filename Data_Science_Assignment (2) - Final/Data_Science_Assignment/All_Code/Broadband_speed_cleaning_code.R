#cleaning the broadbandspeed data
library(tidyverse)
library(dplyr)
library(lubridate)

# Read the CSV files
coverage_data = read_csv("C:/Users/USER/Downloads/Obtain/Obtain_Datascience/201809_fixed_pc_r03/201809_fixed_pc_coverage_r01.csv")
performance_data = read_csv("C:/Users/USER/Downloads/Obtain/Obtain_Datascience/201809_fixed_pc_r03/201805_fixed_pc_performance_r03.csv")

# Merge the datasets on the common key "Postcode"
merged_data = merge(coverage_data, performance_data, by = "postcode", all = TRUE)

# View the merged dataset
summary(merged_data)

View(merged_data)
colnames(merged_data)

# Write the cleaned and merged data to a new CSV file
write_csv(merged_data, "D:/Data_Science_Assignment/Cleaned_Data/Merged Broadband Speed/merged_coverage_performance_broadbandSpeed_data.csv")

# Importing postcode to lsoa cleaned csv 
postcode_to_lsoa_cleaned_path ="D:/Data_Science_Assignment/Cleaned_Data/postcode_to_lsoa_cleaned.csv"
postcode_to_lsoa_cleaned = read_csv(postcode_to_lsoa_cleaned_path)
View(postcode_to_lsoa_cleaned)

#cleaning the broadband speed
broadband_speed_cleaned = merged_data %>%
  select(`Average download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`, postcode_space) %>%  # Only selecting required columns
  rename(Postcode= `postcode_space`) %>%  # Renaming the postcode_space column to Postcode
  right_join(postcode_to_lsoa_cleaned, by = "Postcode") %>%  # Joining with the postcode_to_lsoa_cleaned by matching Postcode
  select(`Average download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`, Postcode, `Short Postcode`, `Town/City`, District, County) %>%  # Selecting only required columns
  mutate(`Short Postcode`= substr(Postcode, 1,5)) %>% #Fill the missing short code valuess
  na.omit() # Removing rows with null values


View(broadband_speed_cleaned)

broadband_speed_cleaned_path ="D:/Data_Science_Assignment/Cleaned_Data/Broadband_speed_cleaned_data.csv"

write.csv(broadband_speed_cleaned,broadband_speed_cleaned_path, row.names = FALSE) 

