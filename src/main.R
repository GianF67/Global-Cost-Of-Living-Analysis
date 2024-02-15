# dataset: https://www.kaggle.com/datasets/mvieira101/global-cost-of-living/data

setwd("/Users/gianmarcofistani/Desktop/BABD/courses/9_FOS/_project/statistics-project")

# Load data from a CSV file
data <- read.csv("./data/cost-of-living-formatted.csv")
View(data)

# Rename the column
#names(data)[4] <- "meal_inexpensive_restaurant"

# Save the modified dataset to a CSV file
#write.csv(data, "./data/cost-of-living-formatted.csv", row.names = FALSE)

#formatted_data <- read.csv("./data/cost-of-living-formatted.csv")
#View(formatted_data)

# REGRESSION ANALYSIS
# 1) Explore Your Data: head(), summary(), and str()

head(data)

summary(data)


# x = predictor variable
# y = response variable
regression <- lm(m200 ~ m100, data=data)
regression


