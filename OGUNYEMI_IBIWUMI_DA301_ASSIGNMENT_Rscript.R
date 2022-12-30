# ASSIGNMENT ACTIVITY FOUR

# 1. Prepare the work station.

# Import tidyverse library.
library(tidyverse)

# Import the data set.
turtle_s <- read.csv(file.choose(), header=TRUE)

# View the data frame.
View(turtle_s)
as_tibble(turtle_s)

# View a summary of the data frame.
summary(turtle_s)

# Creating a subset of the data.
turtle_sa <- select(turtle_s, -Ranking, -Year, -Genre, -Publisher)

# view te output.
view(turtle_sa)
head(turtle_sa)
as_tibble(turtle_sa)
summary(turtle_sa)
glimpse(turtle_sa)
dim(turtle_sa)

#  Scatterplot

# Specify X as Product, y as NA_Sales, and turtle_sa as the data source 
# (the x-axis variable is passed first, followed by the y-axis,
#  and then the source of the data is specified).
qplot(Product, NA_Sales, data=turtle_sa)

# To create a scatterplot y = NA_Sales only.
qplot(y = NA_Sales, data=turtle_sa)

# Specify x as product, y as EU_Sales, and turtle_sa as source data.
qplot(Product, EU_Sales, data=turtle_sa)

# To create a scatterplot y = EU_Sales only.
qplot(y = EU_Sales, data=turtle_sa)

# Specify x as Product, y as Global_Sales, and turtle_sa as data source.
qplot(Product, Global_Sales, data=turtle_sa)

# To create a scatterplot y = Global_Sales only.
qplot(y = Global_Sales, data=turtle_sa)


# Create a histogram for NA_Sales, EU_Sales and Global_Sales.

# NA_Sale histogram
qplot(NA_Sales, data=turtle_sa)
# Adjusted histogram bins(NA_Sales)
qplot(NA_Sales, bins=5, data=turtle_sa)

# EU_Sales histogram
qplot(EU_Sales, data=turtle_sa)
# Adjusted histogram bins(EU_Sales)
qplot(EU_Sales, bins=5, data=turtle_sa)

# Global_Sales histogram
qplot(Global_Sales, data=turtle_sa)
# Adjusted histogram bins(Global_Sales)
qplot(Global_Sales, bins=5, data=turtle_sa)


# Create a Boxplot for NA_Sales, EU_Sales, Global_Sales
boxplot(turtle_sa$NA_Sales)
boxplot(turtle_sa$EU_Sales)
boxplot(turtle_sa$Global_Sales)

qplot(NA_Sales, data=turtle_sa, geom='boxplot')







# ASSIGNMENT ACTIVITY FIVE


# 1. Prepare the workstation.

# Import tidyverse library.
library(tidyverse)

# Data wrangling.
library(dplyr)
turtle <- read.csv(file.choose(), header=TRUE)

# View the output.
view(turtle)

turtle_sa <- select(turtle, -Ranking, -Year, -Genre, -Publisher)

# View the output of the subset
view(turtle_sa)
dim(turtle_sa)
# Determine min, max and mean of NA_Sales.
min(turtle_sa$NA_Sales)
max(turtle_sa$NA_Sales)
mean(turtle_sa$NA_Sales)
median(turtle_sa$NA_Sales)
sd(turtle_sa$NA_Sales)
summary(turtle_sa$NA_Sales)


#2. Determine min, max and mean of EU_Sales.
min(turtle_sa$EU_Sales)
max(turtle_sa$EU_Sales)
mean(turtle_sa$EU_Sales)
median(turtle_sa$EU_Sales)
sd(turtle_sa$EU_Sales)
summary(turtle_sa$EU_Sales)


# Determine min, max and mean of Global_Sales.
min(turtle_sa$Global_Sales)
max(turtle_sa$Global_Sales)
mean(turtle_sa$Global_Sales)
median(turtle_sa$Global_Sales)
sd(turtle_sa$Global_Sales)
summary(turtle_sa$Global_Sales)

# Determine the summary
summary(turtle_sa)

# 3. Determine the impact of sales per product_id.

aggregate(NA_Sales~Product, turtle_sa, sum)
aggregate(EU_Sales~Product, turtle_sa, sum)
aggregate(Global_Sales_Sales~Product, turtle_sa, sum)

# 3b. Using group_by.
# Specify the column names for grouping the data.

df_turtle <- turtle_sa %>% group_by(Product) %>%
  summarise(NA_Sales_sum = sum(NA_Sales),
            EU_Sales_sum = sum(EU_Sales),
            Global_Sales_sum = sum(Global_Sales),
            .groups='drop')

# View the results.
view(df_turtle)
summary(df_turtle)

library(ggplot2)

# To plot boxplot.
boxplot(df_turtle$NA_Sales_sum)
boxplot(df_turtle$EU_Sales_sum)
boxplot(df_turtle$Global_Sales_sum)

# To plot Histogram.
hist(df_turtle$NA_Sales_sum)
hist(df_turtle$EU_Sales_sum)
hist(df_turtle$Global_Sales_sum)


#4. To check for normal distribution.

# Draw a qqplot using the NA_Sales.
qqnorm(df_turtle$NA_Sales_sum, col = 'blue')
# Add a reference line to the qqplot.
qqline(df_turtle$NA_Sales_sum, col = 'red')

# Draw a qqplot using the EU_Sales.
qqnorm(df_turtle$EU_Sales_sum, col = 'blue')
# Add a reference line to the qqplot.
qqline(df_turtle$EU_Sales_sum, col = 'black')

# Draw a qqplot using the Global_Sales.
qqnorm(df_turtle$Global_Sales_sum, col = 'blue')
# Add a reference line to the qqplot.
qqline(df_turtle$Global_Sales_sum, col = 'green')



#5. Run a Shapiro-Wilk test for NA_Sales
shapiro.test(df_turtle$NA_Sales_sum)
# Run a Shapiro-Wilk test for EU_Sales
shapiro.test(df_turtle$EU_Sales_sum)
# Run a Shapiro-Wilk test for Global_Sales
shapiro.test(df_turtle$Global_Sales_sum)


library(moments)

#6. Specify the skewness and kurtosis functions.
skewness(df_turtle$NA_Sales_sum)
kurtosis(df_turtle$NA_Sales_sum)

# Specify the skewness and kurtosis functions.
skewness(df_turtle$EU_Sales_sum)
kurtosis(df_turtle$EU_Sales_sum)

# Specify the skewness and kurtosis functions.
skewness(df_turtle$Global_Sales_sum)
kurtosis(df_turtle$Global_Sales_sum)

#7. Determine the correlation.

turtle_sales <- select(df_turtle, -Product)
# view output
view(turtle_sales)

# Determine correlation for the whole dataframe.
round (cor(turtle_sales),
       digits=2)


#8. Scatterplot to indicate relation between NA_Sales_sum and Global_Sales_sum.
ggplot(data = turtle_sales,
       mapping = aes(x = NA_Sales_sum, y = Global_Sales_sum)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 1.5) +
  
  labs(title = "Relationship between North America Sales and Global Sales",
       caption = "Source: website")



# Scatterplot to indicate relation between EU_Sales_sum and Global_Sales_sum.
ggplot(data = turtle_sales,
       mapping = aes(x = EU_Sales_sum, y = Global_Sales_sum)) +
  geom_point(color = 'orange', alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 1.5) +
  
  labs(title = "Relationship between Europe Sales and Global Sales",
       caption = "Source: website")




# Scatterplot to indicate relation between NA_Sales_sum and EU_Sales_sum.
ggplot(data = turtle_sales,
       mapping = aes(x = NA_Sales_sum, y = EU_Sales_sum)) +
  geom_point(color = 'green', alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 1.5) +
  
  labs(title = "Relationship between North America Sales and Europe Sales",
       caption = "Source: website")








# ASSIGNMENT ACTIVITY SIX

#1. Prepare the workstation
library(tidyverse)
# Import the data set (turtle_sale.csv). 
turtle_sales <- read.csv('turtle_sales', header=TRUE)

# View the dataframe
head(turtle_sales)

# Determine a summary of the data frame.
dim(turtle_sales)
summary(turtle_sales)


# 2. Create a simple linear regression model

## 2a) Determine the correlation between columns
cor(turtle_sales)

# Create a linear regression model.
modelc <- lm(NA_Sales_sum ~ Global_Sales_sum, data = turtle_sales)

summary(modelc)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(modelc$residuals)
plot(turtle_sales$Global_Sales_sum, turtle_sales$NA_Sales_sum)
abline(coefficients(modelc))



modeld <- lm(EU_Sales_sum ~ Global_Sales_sum, data = turtle_sales)
summary(modeld)

plot(modeld$residuals)
plot(turtle_sales$Global_Sales_sum, turtle_sales$EU_Sales_sum)
abline(coefficients(modeld))


# 3. Create a multiple linear regression model
# Load the library.
library(psych)

# Determine the correlation plot.
# character size (cex=1).
corPlot(turtle_sales, cex=1)

# Multiple linear regression model.
modele = lm(Global_Sales_sum ~ NA_Sales_sum + EU_Sales_sum, data=turtle_sales)

# Print the summary statistics for modele
summary(modele)

# 4. Predictions based on given values
# Compare with observed values for a number of records.


#define new turtle_sale
new <- data.frame(NA_Sales_sum=c(34.02, 3.93, 2.73, 2.26, 22.08),
                  EU_Sales_sum=c(23.80, 1.56, 0.65, 0.97, 0.52))

#use the fitted model to predict the Global_sales_sum
predict(modele, newdata=new,
        interval = 'confidence')
