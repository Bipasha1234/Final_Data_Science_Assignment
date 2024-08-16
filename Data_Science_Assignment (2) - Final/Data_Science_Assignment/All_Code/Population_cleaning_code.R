library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)

# Import cleaned postcode to LSOA csv into R
postcode_to_LSOA_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/postcode_to_lsoa_cleaned.csv")

# Import population csv, manage the postcode column, and convert population to 2023
population = read_csv("D:/Data_Science_Assignment/Obtained_Data/Population2011_1656567141570.csv") %>%
  rename(`Short Postcode` = Postcode) %>% # Rename the postcode to short postcode
  mutate(`Short Postcode` = gsub(" ", "", `Short Postcode`),  # Remove all spaces
         `Short Postcode` = if_else(nchar(`Short Postcode`) == 5, 
                                    paste0(substr(`Short Postcode`, 1, 4), " ", substr(`Short Postcode`, 5, 6)), 
                                    paste0(substr(`Short Postcode`, 1, 3), " ", substr(`Short Postcode`, 4, 5)))) %>% # Fixing inconsistent spacing in postcode column
  mutate(Population2023 = 1.00561255390388033 * Population) %>% # Convert population to 2023
  select(-Population) # Optionally remove the 2011 population column

# Cleaning the population csv more then joining with Postcode to LSOA table
population = population %>% 
  as_tibble() %>% # Convert into tibble
  right_join(postcode_to_LSOA_cleaned, by = "Short Postcode") %>%  # Joining with the cleaned Postcode to LSOA dataset by matching Postcode
  na.omit()  

View(population)

population_path = "D:/Data_Science_Assignment/Cleaned_Data/Population_Cleaned.csv"

write.csv(population, population_path, row.names = FALSE)

