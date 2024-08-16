#Average download speed VS drug offence rates (per ten thousand people)

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

# Import population cleaned csv
population_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Population_Cleaned.csv")

# Import Crime cleaned csv
crime_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Crime_Cleaned.csv") 

# Import cleaned broadband speed
broadband_speed_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Broadband_speed_cleaned_data.csv") 

# By town and county -- grouping the broadband SPeed & Average Download Speed --- Finding----for each group
broadband_speed_grouping = broadband_speed_cleaned %>%
  group_by(`Town/City`,County) %>%
  summarise(`Average download speed (Mbit/s)`= mean(`Average download speed (Mbit/s)`))

# Now, Crime dataset shows the drug offence rate and crime count  as we modified it. 
crime_drugs = crime_cleaned %>% 
  mutate(`Crime Date`= substr(`Crime Date`, 1, 4)) %>% # Only showing year
  group_by(`Short Postcode`,`Crime.type`,`Crime Date`, `Falls.within`) %>% #Grouping to display the yearly crime count for each postcode
  select(`Short Postcode`,`Crime.type`,`Crime Date`, `Falls.within`) %>% 
  na.omit() %>% 
  tally() %>% # Crime count column ----- create
  rename(`Crime Count`=n) %>%
  right_join(population_cleaned, by = "Short Postcode") %>% # Join with population cleaned csv in order to show the District and Population2023
  select(`Short Postcode`,`Crime.type`,`Crime Count`, `Population2023`, `Crime Date`, `Falls.within`, `Town/City`, District) %>% 
  na.omit() %>% 
  filter(`Crime.type`== "Drugs") %>% # shows crime type of drugs only
  mutate(`Drug Offence Rate` = (`Crime Count` / Population2023) * 10000) # Calculation of drug offence rate.

# Then, By County and Town ----grouping ----Drug Crime Dataset & Displaying the rate for each group for the year 2023.
drug_crime_grouping = crime_drugs %>% 
  filter(`Crime Date`=="2023") %>% 
  group_by(`Falls.within`,`Town/City`) %>% 
  summarise(`Drug Offence Rate`= mean(`Drug Offence Rate`))

# Broadband and Drug crime rate data -- JOIN----in a single table
broadband_crime = broadband_speed_grouping %>% 
  left_join(drug_crime_grouping,by="Town/City") %>% 
  na.omit

# Create a Linear Model 
# This model predicts Average download speed based on Drug offence rate
linear_model = lm(data = broadband_crime, `Average download speed (Mbit/s)`~`Drug Offence Rate`) 

# Summary of Linear Model
summary(linear_model) 

# Create Linear Model Graph
ggplot(broadband_crime,aes(x=`Drug Offence Rate`,y=`Average download speed (Mbit/s)`)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10))+ 
  # Red as City of Bristol data point.
  geom_point(data = filter(broadband_crime , County=="CITY OF BRISTOL"),aes(color=c("Red"="CITY OF BRISTOL")))+ 
  # Green for Cornwall data point
  geom_point(data = filter(broadband_crime , County=="CORNWALL"), aes(color=c("Green"="CORNWALL"))) + 
  # LInear Regression Line adding------and "Removing error bands"
  geom_smooth(method = lm,se = FALSE,color = "blue")+
  labs(x="Drug Offence Rate",
       y="Average Download Speed (Mbit/s)",
       title="Average Download Speed Vs Drug Offence Rate (2023)",color="County") 



