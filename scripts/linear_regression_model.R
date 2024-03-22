# Install and load library to read excel file
# install.packages("readxl")
library(readxl)

# Read the dataset
data <- read_excel("data/data.xlsx")

# Select the relevant columns for independent variables and dependent variable
selected_data <- data[, c("birth_sex", "age", "1. martial", "num_siblings", "2. num_children", "6. close_friends", "suicide_lifetime")]

# Rename the columns for better readability
colnames(selected_data) <- c("sex", "age", "marital_status", "num_siblings", "num_children", "close_friends")

# Set the dependent variable from the dataset
dependent_variable <- selected_data$suicide_lifetime

# Build the linear regression model
model <- lm(dependent_variable ~ sex + age + marital_status + num_siblings + num_children + close_friends, data = selected_data)

# Bayesian Model
# TO DO

# Print the summary of the model
summary(model)

par(mfrow = c(2, 2))  

# ---------------------------------

# Box plot of suicide_lifetime vs age
boxplot(dependent_variable ~ age, data = selected_data, xlab="Age", ylab="Lifetime Suicide Ideation")

# Sunflower plot (experimentation)
library(beeswarm)
beeswarm(dependent_variable ~ age, data = selected_data, xlab="Age", ylab="Lifetime Suicide Ideation")

# ---------------------------------

# Individually analyze the independent variables to see how they impact our dependent_variable
plot(selected_data$age, dependent_variable,
     xlab = "Age",
     ylab = "Lifetime Suicide Ideation",
     )

plot(selected_data$age, dependent_variable,
     xlab = "Age",
     ylab = "Lifetime Suicide Ideation", 
     xlim = c(min(selected_data$age), max(selected_data$age)), 
     ylim = c(1, 6) 
) 

# ---------------------------------

boxplot(dependent_variable ~ num_siblings, data = selected_data, xlab="Number of Siblings", ylab="Lifetime Suicide Ideation")

# ---------------------------------

boxplot(dependent_variable ~ num_children, data = selected_data, xlab="Number of Children", ylab="Lifetime Suicide Ideation")
