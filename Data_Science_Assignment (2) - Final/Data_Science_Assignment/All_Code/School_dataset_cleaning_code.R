library(tidyverse)
library(dplyr)
library(lubridate)

# 2022-2023 Cornwall School Dataset Cleaning

Cornwall_2022_2023_school = read_csv("D:/Data_Science_Assignment/Cleaned_Data/School Dataset Merged/Cornwall_School_Dataset_2022_2023.csv") 

Cornwall_2022_2023_school_cleaning=Cornwall_2022_2023_school%>%
select(SCHNAME,ATT8SCR, TOWN, PCODE ) %>%  #Selecte  the required columns only--
  rename(`School Name`=SCHNAME, Town= TOWN, `Postcode`= PCODE, `Attainment Score`=ATT8SCR) %>% 
  as_tibble() %>% #Converte to tibble--
  mutate('Short Post Code'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  #Remove the rows with null value
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  #removing NE and SUPP from Attainment Score row
  mutate(County = "CORNWALL") %>% #adding a new column for county 
  mutate(Year= "2023")

# 2022-2023 Bristol School Dataset Cleaning
Bristol_2022_2023_school = read_csv("D:/Data_Science_Assignment/Cleaned_Data/School Dataset Merged/Bristol_School_Dataset_2022_2023.csv") 

Bristol_2022_2023_school_cleaning=Bristol_2022_2023_school%>%
  select(SCHNAME,ATT8SCR, TOWN, PCODE ) %>%  #Selecte  the required columns only--
  rename(`School Name`=SCHNAME, Town= TOWN, `Postcode`= PCODE, `Attainment Score`=ATT8SCR) %>% 
  as_tibble() %>% #Converte to tibble--
  mutate('Short Post Code'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  #Remove the rows with null value
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  #removing NE and SUPP from Attainment Score row
  mutate(County = "CITY OF BRISTOL") %>% #adding a new column for county 
  mutate(Year= "2023")

# 2021-2022 Cornwall School Dataset Cleaning
Cornwall_2021_2022_school = read_csv("D:/Data_Science_Assignment/Cleaned_Data/School Dataset Merged/Cornwall_School_Dataset_2021_2022.csv") 

Cornwall_2021_2022_school_cleaning=Cornwall_2021_2022_school%>%
  select(SCHNAME,ATT8SCR, TOWN, PCODE ) %>%  #Selecte  the required columns only--
  rename(`School Name`=SCHNAME, Town= TOWN, `Postcode`= PCODE, `Attainment Score`=ATT8SCR) %>% 
  as_tibble() %>% #Converte to tibble--
  mutate('Short Post Code'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  #Remove the rows with null value
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  #removing NE and SUPP from Attainment Score row
  mutate(County = "CORNWALL") %>% #adding a new column for county 
  mutate(Year= "2022")

# 2021-2022 Bristol School Dataset Cleaning
Bristol_2021_2022_school = read_csv("D:/Data_Science_Assignment/Cleaned_Data/School Dataset Merged/Bristol_School_Dataset_2021_2022.csv") 

Bristol_2021_2022_school_cleaning=Bristol_2021_2022_school%>%
  select(SCHNAME,ATT8SCR, TOWN, PCODE ) %>%  #Selecte  the required columns only--
  rename(`School Name`=SCHNAME, Town= TOWN, `Postcode`= PCODE, `Attainment Score`=ATT8SCR) %>% 
  as_tibble() %>% #Converte to tibble--
  mutate('Short Post Code'= substr(Postcode, 1, 5)) %>% 
  na.omit() %>%  #Remove the rows with null value
  filter (`Attainment Score` != "NE" & `Attainment Score` != "SUPP") %>%  #removing NE and SUPP from Attainment Score row
  mutate(County = "CITY OF BRISTOL") %>% #adding a new column for county 
  mutate(Year= "2022")

# NOW combining all the cleaned data of school into a single tibble
school_dataset_cleaned_combined= bind_rows(Bristol_2021_2022_school_cleaning,Bristol_2022_2023_school_cleaning,Cornwall_2021_2022_school_cleaning,Cornwall_2022_2023_school_cleaning)

cleanedSchool_path = "D:/Data_Science_Assignment/Cleaned_Data/School_Dataset_Cleaned.csv"

#save now the merged $ cleaned csv
write.csv(school_dataset_cleaned_combined,cleanedSchool_path, row.names = FALSE) 
View(school_dataset_cleaned_combined)
