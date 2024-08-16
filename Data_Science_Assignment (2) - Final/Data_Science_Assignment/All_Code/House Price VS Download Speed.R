library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
 
#Cleaned houseprices - import
houseprices_cleaned = read_csv ("D:/Data_Science_Assignment/Cleaned_Data/Cleaned_houseprices.csv") 

#Cleaned broadband speed - import
broadband_speed_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Broadband_speed_cleaned_data.csv") 

# Finding average price for each group.
housePrices_grouped = houseprices_cleaned %>%
  filter(`Date of Transfer`=="2023") %>%
  group_by(`Town/City`,County) %>%
  summarise(Price=mean(Price))

# Finding average download speed for each group.
broadbandSpeeds_grouped = broadband_speed_cleaned %>%
  group_by(`Town/City`,County) %>%
  summarise(`Average download speed (Mbit/s)`= mean(`Average download speed (Mbit/s)`))

# Merging the house price & broadband speed data in the single table
housePrice_broadband = housePrices_grouped %>% 
  left_join(broadbandSpeeds_grouped,by="Town/City")

# Now, creating the Linear Model 
# This model predicts Price based on Average download speed (Mbit/s).
linear_model = lm(data=housePrice_broadband, Price~`Average download speed (Mbit/s)`) 

# Summary of the Linear Model
summary(linear_model) 

# Fnally, Creating the Linear Model graph
ggplot(housePrice_broadband ,aes(x=`Average download speed (Mbit/s)`,y=Price)) +
  scale_y_continuous(limits=c(0,3000000), breaks = seq(0,3000000,1000000))+
  #red for CITY OF BRISTOL data point
  geom_point(data = filter(housePrice_broadband,County.x=="CITY OF BRISTOL"),aes(color=c("Red"="CITY OF BRISTOL")))+ 
  #green for Cornwall data point
  geom_point(data = filter(housePrice_broadband,County.x=="CORNWALL"), aes(color=c("Green"="CORNWALL"))) + 
  # LInear Regression Line adding------and "Removing error bands"
  geom_smooth(method = lm,se = FALSE,color = "blue")+  
  labs(x="Average Download Speed (Mbit/s)",
       y="Price",
       title="House Prices vs Average Download Speed for 2023",color="County")

