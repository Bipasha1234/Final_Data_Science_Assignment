library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# Import population cleaned csv -----
population_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Population_Cleaned.csv")

# Import the cleaned house prices
houseprices_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Cleaned_houseprices.csv") 

# Import the cleaned crime csv
crime_cleaned= read_csv("D:/Data_Science_Assignment/Cleaned_Data/Crime_Cleaned.csv") 
View(crime_cleaned)

# By Town and County -- group and  Average Price for each group --- Find
housePrices_grouping = houseprices_cleaned %>%
  filter(`Date of Transfer`=="2023") %>%
  group_by(`Town/City`,County) %>%
  summarise(Price=mean(Price))

# Now, including drug offence rate and crime count in crime dataset.
crime_drugs_dataset = crime_cleaned %>% 
  mutate(`Crime Date`= substr(`Crime Date`, 1, 4)) %>% # Only shows the year
  group_by(`Short Postcode`,`Crime.type`,`Crime Date`, `Falls.within`) %>% #Grouping to display the yearly crime count for each postcode
  select(`Short Postcode`,`Crime.type`,`Crime Date`, `Falls.within`) %>% 
  na.omit() %>% 
  tally() %>% #crime count column--create
  rename(`Crime Count`=n) %>%  # Rename the Crime Count Column
  right_join(population_cleaned, by = "Short Postcode") %>% # Join with population cleaned csv in order to show the District and Population2023
  select(`Short Postcode`,`Crime.type`,`Crime Count`, `Population2023`, `Crime Date`, `Falls.within`, `Town/City`, District) %>%
  na.omit() %>% 
  filter(`Crime.type`== "Drugs") %>% #filtering to show only drug crimes of 2023
  mutate(`Drug Offence Rate` = (`Crime Count` / Population2023)) #calculating drug offence rate

# By county and town grouping the crime and drug csv & Showing the rate for each group for the year 2023
drug_crime_grouping = crime_drugs_dataset %>% 
  filter(`Crime Date`=="2023") %>% 
  group_by(`Falls.within`,`Town/City`) %>% 
  summarise(`Drug Offence Rate`= mean(`Drug Offence Rate`))

# House price data and drug crime rate data ----JOINING---in a single table
housePrice_drugCrime = housePrices_grouping %>% 
  left_join(drug_crime_grouping,by="Town/City") %>% 
  na.omit 

# Create a Linear Model 
# This model predicts House Price based on Drug Rates.
linear_model = lm(data = housePrice_drugCrime , Price~`Drug Offence Rate`) 

# Show Summary of Linear Model
summary(linear_model) 

# Create Linear Model Graph
ggplot(housePrice_drugCrime,aes(x=`Drug Offence Rate`,y=Price)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,500000))+ 
  scale_x_continuous(labels = label_number(scale = 1, big.mark = ",")) + # Format the x-axis labels
  # Red as City of Bristol data point.
  geom_point(data = filter(housePrice_drugCrime,County=="CITY OF BRISTOL"),aes(color=c("Red"="CITY OF BRISTOL")))+ 
  # Green for Cornwall data point.
  geom_point(data = filter(housePrice_drugCrime,County=="CORNWALL"), aes(color=c("Green"="CORNWALL"))) + 
  # LInear Regression Line adding------and "Removing error bands"
  geom_smooth(method = lm , se = FALSE , color = "blue")+ 
  labs(x="Drug Offence Rate",
       y="Price",
       title="House Prices vs Drug Offence Rate (2023) ",color="County") 

