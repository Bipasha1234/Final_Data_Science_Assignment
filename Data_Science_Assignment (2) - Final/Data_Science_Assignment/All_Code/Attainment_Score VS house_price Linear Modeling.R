#Attainment 8 Score VS house price (both counties combined)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# Import Cleaned House Prices
houseprices_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Cleaned_houseprices.csv") 

# Import Cleaned School csv
school_dataset_cleaned= read_csv("D:/Data_Science_Assignment/Cleaned_Data/School_Dataset_Cleaned.csv")

# By Town and County grouping the House Prices & Average Price --- Finding for each group
house_prices_grouping = houseprices_cleaned %>%
  filter(`Date of Transfer`=="2023") %>%
  group_by(`Town/City`,County) %>%
  # From uppercase to all lowercase to the Town column
  mutate(`Town/City` = tolower(`Town/City`)) %>%
  summarise(Price=mean(Price))

#grouping school data by town and county and finding average score for each group
school_dataset_grouped = school_dataset_cleaned %>%
  filter(`Year`=="2023") %>%
  group_by(`Town`,County) %>%
  # Convert town to all lowercase
  mutate(Town= tolower(Town)) %>%  
  summarise(`Attainment Score`=mean(`Attainment Score`))

# School and House Price Combining -  in a single table
school_houseprice = school_dataset_grouped %>% 
  left_join(house_prices_grouping,by=c("Town"="Town/City")) %>% 
  na.omit

# Create a Linear Model 
# This model predicts Average attainment score based on Average House Prices
linear_model = lm(data = school_houseprice, `Attainment Score`~Price) 

# Summary of the Linear Model
summary(linear_model) 

# Create the Linear Model Graph
ggplot(school_houseprice,aes(x=Price,y= `Attainment Score`)) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10))+ 
  scale_x_continuous(labels = label_number(scale = 1, big.mark = ",")) + # Format the x-axis labels
  # Red as City of Bristol data point.
  geom_point(data = filter(school_houseprice,County.x=="CITY OF BRISTOL"),aes(color=c("Red"="CITY OF BRISTOL")))+ 
  # Green for Cornwall data point.
  geom_point(data = filter(school_houseprice,County.x=="CORNWALL"), aes(color=c("Green"="CORNWALL"))) + 
  # LInear Regression Line adding------and "Removing error bands"
  geom_smooth(method = lm , se = FALSE,color = "blue")+ 
  labs(x="House Price",
       y="Attainment Score",
       title="Attainment Score Vs House Prices (2023) ",color="County") 
