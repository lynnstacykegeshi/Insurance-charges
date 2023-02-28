## Loading the necessary libraries
library(dplyr) #data manipulation
library(ggplot2) #data visualization
library(tidyverse) #data manipulation and visualization

# Load the dataset
insurance <- read.csv("insurance.csv", header = TRUE, stringsAsFactors = FALSE)

# View the structure of the dataset
str(insurance)

# Check for missing values
sum(is.na(insurance))

# Calculate summary statistics for age and charges
summary(insurance$age)
summary(insurance$charges)

# Plot the distribution of charges using a histogram
ggplot(data = insurance, aes(x = charges)) +
  geom_histogram(binwidth = 1000, fill = "#69b3a2", color = "#e9ecef") +
  scale_x_continuous(labels = scales::dollar_format(scale = 1), breaks = seq(0, 70000, by = 10000)) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Distribution of Medical Charges", x = "Charges", y = "Frequency")

# Scatter plot of age vs. charges with color-coded points for smoker status
plot(insurance$age, insurance$charges, xlab = "Age", ylab = "Charges", col = ifelse(insurance$smoker == "yes", "red", "blue"), main = "Age vs. Charges by Smoking Status")
legend("topleft", legend = c("Smoker", "Non-smoker"), col = c("red", "blue"), pch = 1)


# Create a box plot of charges for smokers vs. non-smokers
boxplot(insurance$charges ~ insurance$smoker, xlab = "Smoker", ylab = "Charges")

# Bar chart of average charges by smoking status
insurance %>%
  group_by(smoker) %>%
  summarise(avg_charges = mean(charges)) %>%
  ggplot(aes(x = smoker, y = avg_charges, fill = smoker)) +
  geom_col() +
  labs(title = "Average Insurance Charges by Smoking Status", x = "Smoker Status", y = "Average Charges")


#Density plot of BMI by smoker status
ggplot(insurance, aes(x = bmi, fill = smoker)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "BMI by Smoking Status", x = "BMI", y = "Density")

# Calculate the mean charges for different regions
tapply(insurance$charges, insurance$region, mean)

# Box plot of charges by region
boxplot(insurance$charges ~ insurance$region, main = "Insurance Charges by Region", xlab = "Region", ylab = "Charges")

# Test whether there is a significant difference in charges between males and females
t.test(insurance$charges ~ insurance$sex)

# Test whether the number of children has a significant impact on charges
lm_model <- lm(charges ~ children, data = insurance)
summary(lm_model)



#####
#Creating a cost calculator that takes into account the patient’s age, sex, smoker status, region of residence and other factors to accurately predict the medical bills a person will pay in a year

library(randomForest) #predictive modelling
library(caret) #model training and evaluation


# Split the dataset into a training set and a test set
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(insurance$charges, p = .8, list = FALSE)
trainData <- insurance[trainIndex, ]
testData <- insurance[-trainIndex, ]

# Train the model on the training set
model <- randomForest(charges ~ age + sex + bmi + children + smoker + region, data = trainData)

# Use the trained model to make predictions on the test set
predictions <- predict(model, testData)

# Evaluate the model's performance
mse <- mean((predictions - testData$charges)^2)
rmse <- sqrt(mse)
cat("RMSE:", rmse, "\n")

# Use the trained model to predict the medical bills for new patients
newPatient <- data.frame(age = 30, sex = "female", bmi = 25, children = 0, smoker = "no", region = "northeast")
newPrediction <- predict(model, newPatient)
cat("Predicted medical bill for new patient:", newPrediction, "\n")
