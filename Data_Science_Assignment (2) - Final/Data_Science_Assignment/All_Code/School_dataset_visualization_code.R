library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# Importi the Cleaned_School csv
school_dataset_cleaned= read_csv("D:/Data_Science_Assignment/Cleaned_Data/School_Dataset_Cleaned.csv")
View(school_dataset_cleaned)

#   Creating the new dataset which includes ---- District & Short Postcode
SP_District= read_csv("D:/Data_Science_Assignment/Cleaned_Data/Population_Cleaned.csv") %>% 
  select(`Short Postcode`, District) %>% 
  rename(`Short Post Code`= `Short Postcode`) #school dataset has short postcode as different then in population csv so renaming that.

# then Join the district dataset to the cleaned School Dataset by Short Postcode
school_dataset_cleaned = school_dataset_cleaned %>% 
  left_join(SP_District, by = "Short Post Code") %>% 
  na.omit() 

# 2023 Average Attainment Score Box plot
# BY TOWN,DISTRICT,County and year grouping---
School_grouping = school_dataset_cleaned %>% 
  group_by(`Town`,District,County,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) %>% 
  ungroup(`Town`,District,County,`Year`) 

# box plot to visualize the average attainment score for 2023
School_grouping %>% 
  filter(Year==2023) %>% #only showing data of 2023, so filterinng by that
  group_by(County) %>% 
  ggplot(aes(x = County, y = `Average Attainment Score`, fill=County)) + 
  scale_y_continuous(limits=c(0,90), breaks = seq(0,90,5))+
  geom_boxplot() +
  labs(title="Boxplot for 2023 Average Attainment Score By County") 



# Average Attainment Score for 2022-2023 Line Graph For District --CITY OF BRISTOL
# Group cleaned school csv
school_grouping_next <- school_dataset_cleaned %>% 
  filter(County=="CITY OF BRISTOL") %>% #filter to show only rows with county as city of bristol
  group_by(District,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) 

# Convert Year to a factor
school_grouping_next = school_grouping_next %>%
  mutate(Year = as.factor(Year))

# Line graph of average Attainment score - 2022-2023
school_grouping_next %>%
  group_by(District, Year) %>%  # By District and Year -grouping in order to compare the average Attainment Score across districts over multiple years
  ggplot( aes(x = `Year`, y = `Average Attainment Score`, group = District, color = District)) + 
  geom_line(linewidth = 1) +  
  geom_point(size = 2, color = "black") + 
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10)) +
  labs(title = "Average Attainment Score Line Graph For CITY OF BRISTOL - District - 2022-2023",  
       x = "Year",
       y = "Average Attainment Score") 

# Average Attainment Score for 2022-2023 Line Graph For District -- CORNWALL
# Group cleaned school csv
school_grouping_next2 = school_dataset_cleaned %>% 
  filter(County=="CORNWALL") %>% #filter to show only rows with county as cornwall
  group_by(District,Year) %>% 
  summarise(`Average Attainment Score`= mean(`Attainment Score`)) 

# Convert Year to a factor
school_grouping_next2 = school_grouping_next2 %>%
  mutate(Year = as.factor(Year))

# Line graph of average Attainment score - 2022-2023
school_grouping_next2 %>%
  group_by(District, Year) %>%  # By District and Year -grouping in order to compare the average Attainment Score across districts over multiple years
  ggplot( aes(x = `Year`, y = `Average Attainment Score`, group = District, color = District)) + 
  geom_line(linewidth = 1) +  
  geom_point(size = 2, color = "black") + 
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10)) +
  labs(title = "Average Attainment Score Line Graph For Cornwall - District - 2022-2023",  
       x = "Year",
       y = "Average Attainment Score") 
