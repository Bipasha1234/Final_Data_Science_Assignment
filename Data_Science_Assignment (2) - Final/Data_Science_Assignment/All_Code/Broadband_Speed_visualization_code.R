#Exploratory data analysis
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

broadband_speed_cleaned=read_csv("D:/Data_Science_Assignment/Cleaned_Data/Broadband_speed_cleaned_data.csv")

# Box plot for average download speed of bristol and cornwall
broadband_speed_cleaned %>%
  group_by(County) %>%
  ggplot(aes(x = County, y = `Average download speed (Mbit/s)`, fill = County)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,150), breaks = seq(0,150,10))+
  labs(title = "Average Download Speed of Bristol and Cornwall",
       x = "County",
       y = "Average Download Speed (Mbit/s)") 


#Bar Chart
# Filter the data to include only towns from Cornwall.
cornwall_data = broadband_speed_cleaned %>%
  filter(County == "CORNWALL")

# Group the filtered data by town and calculate the average and maximum download speeds.
cornwall_speeds = cornwall_data %>%
  group_by(`Town/City`) %>%
  summarise(
    `Average Download Speed (Mbit/s)` = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),  # Calculate average speed
    `Maximum Download Speed (Mbit/s)` = max(`Maximum download speed (Mbit/s)`, na.rm = TRUE)   # Calculate maximum speed
  ) %>%
  pivot_longer(cols = c(`Average Download Speed (Mbit/s)`, `Maximum Download Speed (Mbit/s)`), 
               names_to = "Speed_Type", values_to = "Speed")  # Transform the data so that each speed type has its own row

# Create a bar chart to display average and maximum download speeds for towns in Cornwall.
ggplot(cornwall_speeds, aes(x = `Town/City`, y = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +  # Create the bars and separate them by speed type
  scale_y_continuous(limits=c(0,350), breaks = seq(0,350,30))+  
  coord_flip() + 
  labs(title = "Average and Maximum Download Speeds by Town for Cornwall",  
       x = "Town/City",
       y = "Speed (Mbit/s)")




# Filter the data to include only towns from Bristol.
bristol_data = broadband_speed_cleaned %>%
  filter(County == "CITY OF BRISTOL")

# Group the filtered data by town and calculate the average and maximum download speeds.
bristol_speeds = bristol_data %>%
  group_by(`Town/City`) %>%
  summarise(
    `Average Download Speed (Mbit/s)` = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),  # Calculate average speed
    `Maximum Download Speed (Mbit/s)` = mean(`Maximum download speed (Mbit/s)`, na.rm = TRUE)   # Calculate maximum speed
  ) %>%
  pivot_longer(cols = c(`Average Download Speed (Mbit/s)`, `Maximum Download Speed (Mbit/s)`), 
               names_to = "Speed_Type", values_to = "Speed")  # Transform the data so that each speed type has its own row

# Bar chart to display average and maximum download speeds for towns in Bristol.
ggplot(bristol_speeds, aes(x = `Town/City`, y = Speed, fill = Speed_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +  # Create the bars and separate them by speed type
  coord_flip() + 
  labs(title = "Average and Maximum Download Speeds by Town for Bristol", 
       x = "Town/City",
       y = "Speed (Mbit/s)") +
  scale_y_continuous(limits=c(0,200), breaks = seq(0,200,10))







