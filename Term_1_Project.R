install.packages("mice")
install.packages("Hmisc")
install.packages("tidyr")
library(Hmisc)
library(e1071)
library(ggplot2)
library(tidyr)
library(mice)
# Data Preparation and Summarization to get a look on it.
project <- read.csv("housingdata.csv")
summary(project)
data_dictionary <- describe(project)
sink("data_dictionary.txt", append = TRUE)
data_dictionary
sink()
# This command shows the data in the console. The displayed results describe the mean, missing values and other details.
file.show(file = "data_dictionary.txt", pager = "console") # Pager = "internal" - to show in a different window. "console" - to show in the console.
# Checking for missing values
md.pattern(project,plot = TRUE)
# Since there are too many missing values, we can not omit these values.Replacing the missing values using mice library.
# Values are iterated and the missing values are replaced with simulated values. Method used is pmm - predictive mean matching
simulation <- mice(project,method = "pmm", m=5)
project_complete <- complete(simulation, action = 1)
# From the density plot we can see that the replaced values are approximately close to the given data.
densityplot(simulation)
# Checking the dependent variable distribution
par(mfrow = c(1,2))
hist(project$MEDV, main = "dependent")
# From the histogram we can see that the graph is positively skewed.
# Checking correlation of the complete data
cor(project_complete,project_complete$MEDV)
# Let's create training and testing data- 70 : 30 ratio.
library(caret)
set.seed(123)

partition <- createDataPartition(project_complete$MEDV, list = FALSE, p = 0.70)
training <- project_complete[partition,]
testing <- project_complete[-partition,] 

# Now performing linear regression. Installing car package. For vif.
install.packages("car")
library(car)

boxplot(project_complete$CRIM)
boxplot(project_complete$ZN)
boxplot(project_complete$INDUS)
boxplot(project_complete$CHAS)
boxplot(project_complete$NOX)
boxplot(project_complete$RM)
hist(project_complete$RM)
boxplot(project_complete$AGE)
boxplot(project_complete$DIS)
boxplot(project_complete$RAD)
boxplot(project_complete$TAX)
boxplot(project_complete$PTRATIO)
boxplot(project_complete$B)
boxplot(project_complete$LSTAT)
hist(project_complete$LSTAT)
boxplot(project_complete$MEDV)

project_complete$MEDV <- log(project_complete$MEDV)

data1 <- lm(MEDV ~., data = training)
summary(data1)
vif(data1)
# Using vif gives highest value for TAX value. So we will remove it and see if the model accuracy is increasing or not.

# The vif value is less than 5. So its ok.
# From summary five variabless are insignificant. So removing them and creating the model.


data2 <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + PTRATIO + B + log(LSTAT), data = training)
summary(data2)
vif(data2)

# We can see that CRIM variable place an important role. Because after removing it. The accuracy of model decreases.
# Predicting the outcome.
prediction <- predict(data2,testing)
summary(prediction)
summary(testing$MEDV)
durbinWatsonTest(data5)