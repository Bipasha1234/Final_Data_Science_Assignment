
#Exploratory data analysis
library(ggplot2)
library(scales)
library(tidyverse)
library(dplyr)
library(lubridate)

cleaned_data_houseprices=read_csv("D:/Data_Science_Assignment/Cleaned_Data/Cleaned_houseprices.csv")

# Filter data for the year 2023
data_2023 = cleaned_data_houseprices %>%
  filter(`Date of Transfer` == 2023)

# Find the average house price for each Town/City, District, and County in 2023
avg_houseprices = data_2023 %>%
  group_by(`Town/City`,District,County,`Date of Transfer`) %>% 
  summarise(`Average Price`= mean(Price)) %>% 
  ungroup(`Town/City`,District,County,`Date of Transfer`) 

# Box plot to visualize average house prices of bristol and cornwall in 2023.
avg_houseprices %>% 
  group_by(County) %>% 
  ggplot(aes(x =County, y = `Average Price`, fill = County)) + 
  geom_boxplot() +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,300000))+ #setting limits and breaks
  labs(title = "Box Plot for 2023 Average House Prices By County")


# Bar chart to visualize the average house prices of bristol and cornwall in 2023.
avg_houseprices %>%
  group_by(County) %>%
  ggplot(aes(x = County, y = `Average Price`, fill = County)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits=c(0,8000000), breaks = seq(0,8000000,800000))+ #setting limits and breaks
  labs(title = "Bar Chart for 2023 Average House Prices by County",
       x = "County",
       y = "Average Price")




#Line Graph
#average house prices Line graph from 2020-2023

# Calculate average house prices by county and year
avg_houseprices2 = cleaned_data_houseprices %>%
  group_by(County, `Date of Transfer`) %>%
  summarise(`Average Price` = mean(Price, na.rm = TRUE), .groups = "drop")

#creating line graph of average house prices from 2020-2023
ggplot(avg_houseprices2 %>%
         filter(`Date of Transfer` %in% 2020:2023),
       aes(x = `Date of Transfer`, y = `Average Price`, color = County, group = County)) +
  geom_line(size = 1) +  # Line width
  geom_point(size = 2) +  # Point size
  scale_y_continuous(limits=c(0,700000), breaks = seq(0,700000,100000), labels = label_number()) +
  labs(title = "Average House Prices (2020-2023) Line Chart",
       x = "Year",
       y = "Average Price")

