# Average download speed  VS Attainment 8 Score
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

# Import Cleaned Broadband Speed
broadband_speed_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Broadband_speed_cleaned_data.csv") 

# Importing Cleaned School csv
school_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/School_Dataset_Cleaned.csv")

# By town and county -- grouping the broadband SPeed & Average Download Speed --- Finding----for each group
broadband_speed_grouping = broadband_speed_cleaned %>%
  group_by(`Town/City`,County) %>%
  mutate(`Town/City`= tolower(`Town/City`)) %>% 
  summarise(`Average download speed (Mbit/s)`= mean(`Average download speed (Mbit/s)`))

# By Town and county -- grouping school dataset & Average Score --FIND-- for each group
school_grouping = school_cleaned %>%
  group_by(`Town`,County) %>%
  mutate(Town= tolower(Town)) %>% 
  summarise(`Attainment Score`=mean(`Attainment Score`))

# Broadband & School Data --- JOINING---- in a single table
broadband_school = broadband_speed_grouping %>% 
  left_join(school_grouping,by=c("Town/City"="Town")) %>% 
  na.omit 

# Create Linear Model 
# This model predicts Average download speed based on Drug offence rate
linear_model = lm(data = broadband_school, `Average download speed (Mbit/s)`~`Attainment Score`) 

# Summary of Linear Model
summary(linear_model) 

# Create Linear Model Graph
ggplot( broadband_school,aes(x=`Attainment Score`,y=`Average download speed (Mbit/s)`)) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10))+ 
  # Red as City of Bristol data point.
  geom_point(data = filter(broadband_school , County.x=="CITY OF BRISTOL"),aes(color=c("Red"="CITY OF BRISTOL")))+
  # Green for Cornwall data point
  geom_point(data = filter(broadband_school , County.x=="CORNWALL"), aes(color=c("Green"="CORNWALL"))) + 
  # LInear Regression Line adding------and "Removing error bands"
  geom_smooth(method = lm , se = FALSE ,color = "blue")+ 
  labs(x="Attainment Score",
       y="Average Download Speed (Mbit/s)",
       title="Average Download Speed Vs Attainment Score",color="County") 

