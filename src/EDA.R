#Select the local working directory
proj_directory <- "SET DIRECTORY"
setwd(proj_directory)

#load packages
library(tidyverse)
library(dplyr)
library(ggplot2)

#Import data from local .csv
col_data <- read.csv("cost-of-living_v2.csv")

#EDA

#summary of what the dataset looks like
View(col_data)
str(col_data)
dim(col_data) # obs./row:4956  variables/columns:58
length(unique(col_data$country)) #215 distinct countries
summary(col_data)

#Index table explaining the meaning of each column label
index_table <- data.frame(
  column_name = c("city", "country", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31", "x32", "x33", "x34", "x35", "x36", "x37", "x38", "x39", "x40", "x41", "x42", "x43", "x44", "x45", "x46", "x47", "x48", "x49", "x50", "x51", "x52", "x53", "x54", "x55", "data_quality"), 
  description = c("Name of the city","Name of the country","Meal, Inexpensive Restaurant (USD)","Meal for 2 People, Mid-range Restaurant, Three-course (USD)","McMeal at McDonalds (or Equivalent Combo Meal) (USD)","Domestic Beer (0.5 liter draught, in restaurants) (USD)","Imported Beer (0.33 liter bottle, in restaurants) (USD)","Cappuccino (regular, in restaurants) (USD)","Coke/Pepsi (0.33 liter bottle, in restaurants) (USD)","Water (0.33 liter bottle, in restaurants) (USD)","Milk (1 liter) (USD)","Loaf of Fresh White Bread (500g) (USD)","Rice (white), (1kg) (USD)","Eggs (regular) (12) (USD)","Local Cheese (1kg) (USD)","Chicken Fillets (1kg) (USD)","Beef Round (1kg) (or Equivalent Back Leg Red Meat) (USD)","Apples (1kg) (USD)","Banana (1kg) (USD)","Oranges (1kg) (USD)","Tomato (1kg) (USD)","Potato (1kg) (USD)","Onion (1kg) (USD)","Lettuce (1 head) (USD)","Water (1.5 liter bottle, at the market) (USD)","Bottle of Wine (Mid-Range, at the market) (USD)","Domestic Beer (0.5 liter bottle, at the market) (USD)","Imported Beer (0.33 liter bottle, at the market) (USD)","Cigarettes 20 Pack (Marlboro) (USD)","One-way Ticket (Local Transport) (USD)","Monthly Pass (Regular Price) (USD)","Taxi Start (Normal Tariff) (USD)","Taxi 1km (Normal Tariff) (USD)","Taxi 1hour Waiting (Normal Tariff) (USD)","Gasoline (1 liter) (USD)","Volkswagen Golf 1.4 90 KW Trendline (Or Equivalent New Car) (USD)","Toyota Corolla Sedan 1.6l 97kW Comfort (Or Equivalent New Car) (USD)","Basic (Electricity, Heating, Cooling, Water, Garbage) for 85m2 Apartment (USD)","1 min. of Prepaid Mobile Tariff Local (No Discounts or Plans) (USD)","Internet (60 Mbps or More, Unlimited Data, Cable/ADSL) (USD)","Fitness Club, Monthly Fee for 1 Adult (USD)","Tennis Court Rent (1 Hour on Weekend) (USD)","Cinema, International Release, 1 Seat (USD)","Preschool (or Kindergarten), Full Day, Private, Monthly for 1 Child (USD)","International Primary School, Yearly for 1 Child (USD)","1 Pair of Jeans (Levis 501 Or Similar) (USD)","1 Summer Dress in a Chain Store (Zara, H&M, …) (USD)","1 Pair of Nike Running Shoes (Mid-Range) (USD)","1 Pair of Men Leather Business Shoes (USD)","Apartment (1 bedroom) in City Centre (USD)","Apartment (1 bedroom) Outside of Centre (USD)","Apartment (3 bedrooms) in City Centre (USD)","Apartment (3 bedrooms) Outside of Centre (USD)","Price per Square Meter to Buy Apartment in City Centre (USD)","Price per Square Meter to Buy Apartment Outside of Centre (USD)","Average Monthly Net Salary (After Tax) (USD)","Mortgage Interest Rate in Percentages (%), Yearly, for 20 Years Fixed-Rate","0 if Numbeo considers that more contributors are needed to increase data quality, else 1"))

subset(index_table, column_name=="x34") #Find one specific row given the column label, just replace "x34"

#Visualising the summary as boxplots -> Significant larger distribution for metrics x35, x38 and x52 compared to the rest of the variables
library(tidyr)

col_data_long <- gather(col_data, key = "metric", value = "value", -city, -country)

ggplot(col_data_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Distribution of Cost of Living Metrics",
       x = "Metric", y = "Cost (USD)")

#check for missing values - Note: good quality data has no missing values
sum(is.na(col_data)) #45858 -> there are a lot of missing values to manage
AAA <- colSums(is.na(col_data)) #check by column 

#Do all variables miss the same amount of values? In %
tot <- nrow(col_data)
percent_AAA <- (AAA / tot) * 100

#Imputation is discouraged for variables with +30% of data missing
  #x28 => 30% One-way Ticket (Local Transport)
  #x29 => 44% Monthly Pass (Transport) 
  #x40 => 48% Tennis Court Rent 
  #x43 => 33% International Primary School, Yearly for 1 Child
  #x51 => 30% Apartment (3 bedrooms) Outside of Centre 
  #x52 => 44% Price per Square Meter to Buy Apartment in City Centre
  #x53 => 46% Price per Square Meter to Buy Apartment Outside of Centre
#If removing the above values, variable x50 (29.8%) has no longer meaning

#Imputation with Mean of data grouped by country & Dropping missing values
library(magrittr) #so we can use the pipe operator %>%

col_imputed_country <- col_data
col_imputed_country <- col_imputed_country %>%
  group_by(country) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) #Group by 'country' and calculate mean for each numeric column

col_imputed_na <- na.omit(col_imputed_country) #drop remaining missing values

data <- select(col_imputed_na, -x28, -x29, -x40, -x43, -x50, -x51, -x52, -x53) #Lastly remove from data the variables highlighted above as missing too many values for analysis.

View(data)
#ANALYSIS
sum(is.na(data)) #0
dim(data) # obs./row:4849  variables/columns:50 -> little rows/columns lost
summary(data) #compared to summary(col_imputed) -> distribution and relationships are impacted as it now pulls towards each country avg. 
length(unique(data$country)) #152 distinct countries -> 63 lost countries

#Visualising the summary of the imputed dataset as boxplots grouped by category
meal <- c("x1", "x2", "x3") #columns to visualise
meal1 <- data[, c("city", "country", meal)] #subset data to include only the specified columns
meal1_long <- gather(meal1, key = "metric", value = "value", -city, -country) #gather the subsetted data into key-value pairs

restaurants <- c("x4", "x5", "x6", "x7", "x8")
restaurants1 <- data[, c("city", "country", restaurants)]
restaurants1_long <- gather(restaurants1, key = "metric", value = "value", -city, -country)

markets <- c("x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27")
markets1 <- data[, c("city", "country", markets)]
markets1_long <- gather(markets1, key = "metric", value = "value", -city, -country)

transportation <- c("x30", "x31", "x32", "x33")
transportation1 <- data[, c("city", "country", transportation)]
transportation1_long <- gather(transportation1, key = "metric", value = "value", -city, -country)

car <- c("x34", "x35")
car1 <- data[, c("city", "country", car)]
car1_long <- gather(car1, key = "metric", value = "value", -city, -country)

utilities <- c("x36", "x37", "x38")
utilities1 <- data[, c("city", "country", utilities)]
utilities1_long <- gather(utilities1, key = "metric", value = "value", -city, -country)

sports_and_leisure <- c("x39", "x41")
sports_and_leisure1 <- data[, c("city", "country", sports_and_leisure)]
sports_and_leisure1_long <- gather(sports_and_leisure1, key = "metric", value = "value", -city, -country)

childcare <- c("x42")
childcare1 <- data[, c("city", "country", childcare)]
childcare1_long <- gather(childcare1, key = "metric", value = "value", -city, -country)

clothing_and_shoes <- c("x44", "x45", "x46", "x47")
clothing_and_shoes1 <- data[, c("city", "country", clothing_and_shoes)]
clothing_and_shoes1_long <- gather(clothing_and_shoes1, key = "metric", value = "value", -city, -country)

rent_per_month <- c("x48", "x49")
rent_per_month1 <- data[, c("city", "country", rent_per_month)]
rent_per_month1_long <- gather(rent_per_month1, key = "metric", value = "value", -city, -country)

salaries_and_financing <- c("x54", "x55")
salaries_and_financing1 <- data[, c("city", "country", salaries_and_financing)]
salaries_and_financing1_long <- gather(salaries_and_financing1, key = "metric", value = "value", -city, -country)

#Plots
meal_plot <- ggplot(meal1_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Distribution of Restaurant Meals",
       x = "Variable", y = "Cost (USD)")

restaurant_plot <- ggplot(restaurants1_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Distribution of Restaurants Drinks",
       x = "Variable", y = "Cost (USD)")

market_plot <- ggplot(markets1_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Markets",
       x = "Variable", y = "Cost (USD)")

transport_plot <- ggplot(transportation1_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "red", color = "black") +
  labs(title = "Distribution of Transportation",
       x = "Variable", y = "Cost (USD)")

car_plot <- ggplot(car1_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Distribution of Car",
       x = "Variable", y = "Cost (USD)")

utilities_plot <- ggplot(utilities1_long, aes(x = metric, y = value)) + 
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Distribution of Utilities",
       x = "Variable", y = "Cost (USD)")

sport_plot <- ggplot(sports_and_leisure1_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Sports And Leisure",
       x = "Variable", y = "Cost (USD)")

childcare_plot <- ggplot(childcare1_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Distribution of Childcare",
       x = "Variable", y = "Cost (USD)")

clothing_plot <- ggplot(clothing_and_shoes1_long, aes(x = metric, y = value)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Distribution of Clothing And Shoes",
       x = "Variable", y = "Cost (USD)")

rent_plot <- ggplot(rent_per_month1_long, aes(x = metric, y = value)) + #Seoul and Shanghai are outliers they might be mistakes
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Distribution of Rent Per Month",
       x = "Variable", y = "Cost (USD)")

salary_plot <- ggplot(salaries_and_financing1_long, aes(x = metric, y = value)) + #Seoul salary outlier again, this might be another error
  geom_boxplot(fill = "violet", color = "black") +
  labs(title = "Distribution of Salaries And Financing",
       x = "Variable", y = "Cost (USD)")

#Formatting for the plots to be displayed in a grid
#par(mfrow = c(3, 4)) -> this command works for base R plots, but not for ggplot2 plots
library(patchwork)
meal_plot + restaurant_plot + market_plot + transport_plot + car_plot + utilities_plot + sport_plot + childcare_plot + clothing_plot + rent_plot + salary_plot

#ANALYSIS -> Outliers present in x2, x4, x15, x32, x35, x36, x39, x46, x54
  #x2 Meal for 2 People, Mid-range -> Mendrisio, Switzerland USD 213.69 -> considering that this looks at mid-range restaurants the price can be accepted (despite it being on the higher end)
  #x4 Domestic Beer (in restaurant) -> Al Wakrah, Qatar USD 20.60 -> cost of beer tends to be higher in countries whose majority of the population is Muslim due to scarcity and high avg cost of living
  #x15 Beef Round (1kg) -> top outliers are both cities from Switzerland 
  #x32 Taxi 1hour Waiting (Normal Tariff) -> Niamay, Nigeria -> False data
  #x34, x35 New cars -> outliers are both Iranian cities, the cost of owning a car compared to the rest of the cost of living doesn't seem to match
  #x39 Fitness Club -> outliers are oultindish, not sure they can be relied on
  #x46 Fitenss shoes -> 900+USD for a pair of common running shoes seems outlandish for any country?
  #x54 Average Monthly Net Salary (After Tax) -> Schaan, Liechtenstein, the country is so small that for analysis purposes I think we can disregard this

#ACTIONS -> Trim oultiers for x15 (top 2), x32(top 1), x39 (top 2), x46 (top 2)

#Other considerations
  #in original analysis Solomon islands outlier in internet, looks like wrong data.
  #Niamey, Niger -> the categorical data seems incorrect, manually fact check this row against Numbeo data -> incorrect values x32, x34, x37, x38
  #Countries such as Greenland might simply lack the information for certain values (eg cars) so results should be taking this factors into consideration.
