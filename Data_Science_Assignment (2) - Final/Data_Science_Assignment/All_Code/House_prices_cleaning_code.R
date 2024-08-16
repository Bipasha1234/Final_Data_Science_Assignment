#Cleaning the data for house prices
library(dplyr)
library(tidyverse)
library(lubridate)

# Define column names for the datasets
column_names = c("Transaction_ID", "Price", "Date of Transfer", "Postcode", "Property_Type", "Old/New", "Duration", "PAON",
                 "SAON", "Street", "Locality", "Town/City", "District", "County", "PPD_Category_type", "Record_Status")

# Read and combine data for the years 2020 to 2023
pp_2020 = read_csv("D:/Data_Science_Assignment/Obtained_Data/House Price Dataset/pp-2020.csv", col_names = FALSE) %>%
  setNames(column_names)
pp_2021 = read_csv("D:/Data_Science_Assignment/Obtained_Data/House Price Dataset/pp-2021.csv", col_names = FALSE) %>%
  setNames(column_names)
pp_2022 = read_csv("D:/Data_Science_Assignment/Obtained_Data/House Price Dataset/pp-2022.csv", col_names = FALSE) %>%
  setNames(column_names)
pp_2023 = read_csv("D:/Data_Science_Assignment/Obtained_Data/House Price Dataset/pp-2023.csv", col_names = FALSE) %>%
  setNames(column_names)

# Combine the datasets into one data frame
combined_data_houseprices = bind_rows(pp_2020,pp_2021,pp_2022,pp_2023)

# Clean the combined data of house prices
cleaned_data_houseprices = combined_data_houseprices %>%
  as_tibble() %>%  #convert to the tibble
  na.omit() %>%    #rows which have null values-removing that
  mutate(Price = as.numeric(Price)) %>%
  filter(County == "CITY OF BRISTOL" | County == "CORNWALL") %>%   #filtering rows with bristol(city of bristol) and cornwall as county
  mutate(`Date of Transfer` = year(ymd(`Date of Transfer`))) %>% #converting date of transfer column to a date format and then extract the year
  mutate(S_No = row_number()) %>%  #adding a S_No column
  select(S_No,Price, `Date of Transfer`, Postcode, `Town/City`, District, County) %>% #selecting required columns only 
  mutate(`Short Postcode`= substr(Postcode, 1,5)) #now adding the another column to the combine dataset

# Define path to save the cleaned dataset
cleaned_houseprices_path = "D:/Data_Science_Assignment/Cleaned_Data/Cleaned_houseprices.csv"

# Save the cleaned dataset
write.csv(cleaned_data_houseprices, cleaned_houseprices_path, row.names = FALSE)

print(cleaned_data_houseprices)
View(cleaned_data_houseprices)
str(cleaned_data_houseprices)