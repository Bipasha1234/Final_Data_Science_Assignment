#Exploratory data analysis
#Box Plot for the drug offense rate of 2023
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

# Import the cleaned crime dataset and cleaned population data
crime_data_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Crime_Cleaned.csv")
population_data_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Population_Cleaned.csv")

# Ensure 'Crime Date' is correctly handled and extract the year
crime_data_2023 = crime_data_cleaned %>%
  mutate(`Crime Date` = substr(`Crime Date`, 1, 4)) %>% # Extracting the year from 'Crime Date'
  filter(`Crime Date` == "2023") %>% # Filtering for the year 2023
  group_by(`Short Postcode`, `Crime.type`, `County`) %>% # Grouping by relevant columns
  tally() %>% # Count occurrences
  rename(`Crime Count` = n) %>% # Renaming the count column
  filter(`Crime.type` == "Drugs") # Filtering for drug-related crimes

# Join with population data and calculate the rate
crime_data_with_population = crime_data_2023 %>%
  left_join(population_data_cleaned, by = "Short Postcode", suffix = c("_crime", "_pop")) %>%
  rename(`County` = `County_crime`) %>% # Renaming columns to consistent names
  mutate(`Drug Offense Rate` = (`Crime Count` / Population2023)) %>% 
  select(`County`, `Drug Offense Rate`) # Selecting the relevant columns

# Box plot for the drug offense rate of 2023
ggplot(crime_data_with_population, aes(x = `County`, y = `Drug Offense Rate`, fill = `County`)) +
  geom_boxplot() +
  labs(title = "Drug Offense Rate by County (2023)",
       x = "County",
       y = "Drug Offense Rate") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()




# Vehicle crime rate from 2020 to 2023 (Radar chart)
library(dplyr)
library(tidyr)
library(fmsb)

crime_data_cleaned = read.csv("D:/Data_Science_Assignment/Cleaned_Data/Crime_Cleaned.csv")
vehicle_crime_types = c("Bicycle theft", "Vehicle crime", "Criminal damage and arson")

# Filtering and summarizeing data for the years 2020 to 2023
crime_counts = crime_data_cleaned %>%
  filter(Crime.type %in% vehicle_crime_types) %>%
  mutate(Year = substr(`Crime.Date`, 1, 4)) %>%
  filter(Year >= "2020" & Year <= "2023") %>%
  group_by(Year, Crime.type) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Crime.type, values_from = Count, values_fill = list(Count = 0))

# Prepare data for radar chart
radar_data = as.data.frame(crime_counts)
row.names(radar_data) = radar_data$Year
radar_data = radar_data %>% select(-Year)

# Ensure at least three columns (crime types)
if (ncol(radar_data) < 3) {
  stop("Not enough variables for radar chart. Ensure you have at least three crime types.")
}
# Add max and min values for radar chart scaling
max_values = rep(max(radar_data, na.rm = TRUE), ncol(radar_data))
min_values = rep(0, ncol(radar_data))
radar_data = rbind(max_values, min_values, radar_data)

# Plot the radar chart
radarchart(radar_data, axistype = 1,
           pcol = rgb(0.2, 0.5, 0.5, 0.7), pfcol = rgb(0.2, 0.5, 0.5, 0.5),
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, max(radar_data[-c(1,2),], na.rm = TRUE), length.out = 5),
           title = "Vehicle Crime Rate (Theft, Accident) 2020 to 2023")


#Pie chart for a specific month of 2023 (robbery rate)
library(dplyr)
library(ggplot2)

# Read the cleaned crime and population data
crime_data_cleaned = read.csv("D:/Data_Science_Assignment/Cleaned_Data/Crime_Cleaned.csv")
population_data = read.csv("D:/Data_Science_Assignment/Cleaned_Data/Population_Cleaned.csv")

# The month and year to analyze
specific_month = "2023-12"

# Filtering the data for the specific month and year
monthly_data = crime_data_cleaned %>%
  filter(substr(Crime.Date, 1, 7) == specific_month)

# Counting the number of robberies by county
robbery_count = monthly_data %>%
  filter(Crime.type == "Robbery") %>%
  group_by(County,Town.City) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Joining with the population data
robbery_rate = robbery_count %>%
  left_join(population_data, by = "County","Town.City.y") %>%
  mutate(Robbery_Rate = (Count / Population2023)) %>%
  rename(`Town/City` = Town.City.y)

# Creating the pie chart for robbery rate
#in the month of 2023-12, the crime rate of Town/city in the county cornwall is very low.
#So, in pie chart, it is not clear.
ggplot(robbery_rate, aes(x = "", y = Robbery_Rate, fill = `Town/City`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = paste("Pie Chart of Robbery Rate by Town/City for 2023-DECEMBER"))





# Drug offense rate - Line chart for both county's per 10k people
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

# Import the cleaned crime dataset and updated population data
crime_dataset_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Crime_Cleaned.csv")
population_data_cleaned = read_csv("D:/Data_Science_Assignment/Cleaned_Data/Population_Cleaned.csv")

# Ensure 'Crime Date' is correctly handled and extract the year
crime_summary = crime_dataset_cleaned %>%
  mutate(`Crime Date` = substr(`Crime Date`, 1, 4)) %>% # Extract year from 'Crime Date'
  filter(`Crime Date` %in% c("2021", "2022", "2023")) %>% # Filter for the years 2020 to 2023
  group_by(`Short Postcode`, `Crime.type`, `County`, `Crime Date`) %>% # Group by relevant columns
  tally() %>% # Counting the occurrences
  rename(`Crime Count` = n) %>% # Renaming the count column
  filter(`Crime.type` == "Drugs") # Filtering for drug-related crimes

# Joining with population data and rename columns to avoid confusion
joined_data = crime_summary %>%
  left_join(population_data_cleaned, by = "Short Postcode", suffix = c("_crime", "_pop")) %>%
  rename(`County` = `County_crime`) # Rename columns to consistent names

# Calculate Drug Offense Rate
crime_dataset_with_rate = joined_data %>%
  mutate(`Drug Offense Rate` = (`Crime Count` / Population2023) * 10000) %>%
  select(`County`, `Crime Date`, `Drug Offense Rate`,`Short Postcode`, `Crime.type`) %>%
  group_by(`County`, `Crime Date`) %>%
  summarize(`Drug Offense Rate` = mean(`Drug Offense Rate`, na.rm = TRUE), .groups = 'drop')

# Create a line chart for drug offense rates by county over the years 2020 to 2023
ggplot(crime_dataset_with_rate, aes(x = `Crime Date`, y = `Drug Offense Rate`, color = `County`, group = `County`)) +
  geom_line() +
  geom_point() +
  labs(title = "Drug Offense Rate by County (2020-2023)",
       x = "Year",
       y = "Drug Offense Rate (per 10,000 residents)") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(limits = c("2021", "2022", "2023"))

