install.packages(c("brms", "tidyverse", "ggplot2", "loo", "rstan", "performance")) 

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(brms)
library(tidyverse)
library(ggplot2)
library(loo)
library(rstan) 
library(performance)
library(readxl)
library(dplyr)


# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data <-  read_excel("/Users/nimishabhagat/Documents/Capstone/Code/Test/data/TUSK.xlsx", sheet = "column_data") 
data <- na.omit(data)  

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Response variable 1
data <- data %>%
  mutate(suicide_lifetime = ifelse(suicide_lifetime == 1, 0, 1))


# Response variable 2
data <- data %>%
  mutate(suicide_pastyear = ifelse(suicide_pastyear == 1, 0, 1))

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Rename the column names to make it readable
data <- data %>%
  rename(marital = `1. martial`)
data <- data %>%
  rename(employed = `9. employed`)

# Encode the marital column
data <- data %>%
  mutate(marital = ifelse(marital == 1, 0, 1))

# Encode the employed variable
data <- data %>%
  mutate(employed = case_when(
    employed == 1 ~ 0,
    employed %in% c(2, 3, 4) ~ 1
  ))

# Encode the demo_area variable
data <- data %>%
  mutate(demo_area = case_when(
    demo_area %in% c(3, 4, 5) ~ 0,
    demo_area %in% c(1, 2, 6) ~ 1
  ))

# Encode the demo_job_status variable
data <- data %>%
  mutate(demo_job_status = case_when(
    demo_job_status %in% c(5, 6, 7, 8) ~ 0,
    demo_job_status %in% c(1, 2, 3, 4, 9) ~ 1
  ))

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

summary(data) 
str(data)     
head(data)  

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Define the formulas for the models
formula1 <- suicide_lifetime ~ 1 + (1|country) 
formula2 <- suicide_lifetime ~ 1 + (1|country) + marital + employed + demo_area + demo_edu +  demo_job_status + demo_income_level
formula3 <- suicide_lifetime ~ 1 + (1|country) + marital + employed + demo_area + demo_edu +  demo_job_status + demo_income_level + belong_sum + burden_sum + communal_sum + horizontal_individualism + vertical_individualism + horizontal_collectivism + vertical_collectivism
formula4 <- suicide_pastyear ~ 1 + (1|country)
formula5 <- suicide_pastyear ~ 1 + (1|country) + marital + employed + demo_area + demo_edu +  demo_job_status + demo_income_level
formula6 <- suicide_pastyear ~ 1 + (1|country) + marital + employed + demo_area + demo_edu +  demo_job_status + demo_income_level + belong_sum + burden_sum + communal_sum + horizontal_individualism + vertical_individualism + horizontal_collectivism + vertical_collectivism

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Define thresholds
prior_thresholds <- c(
  prior(normal(0,0.5), class = Intercept),
  prior(normal(0,0.5), class = b),
  prior(student_t(3,0,1), class = sd)
)

prior_intercept <- c(prior(normal(0, 1), class = 'Intercept'))  
prior_country <- c(prior(normal(0, 0.5), class = 'sd', group = 'country')) 

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Build Models

# Model 1
Model1 <- brm(
  formula = formula1,
  data = data,
  family = bernoulli("logit"),
  # family = "gaussian",
  prior = c(prior_intercept, prior_country),
  warmup = 1000,
  iter = 3000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)
save(Model1,file="Model1.RData")

# Model 2
Model2 <- brm(
  formula = formula2,
  data = data,
  family = bernoulli("logit"),
  # family = "gaussian",
  prior = prior_thresholds,
  warmup = 1000,
  iter = 3000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)
save(Model2,file="Model2.RData")

# Model 3
Model3 <- brm(
  formula = formula3,
  data = data,
  family = bernoulli("logit"),
  # family = "gaussian",
  prior = c(prior_intercept, prior_country),
  warmup = 1000,
  iter = 3000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)
save(Model3,file="Model3.RData")


# Model 4
Model4 <- brm(
  formula = formula4,
  data = data,
  family = bernoulli("logit"),
  # family = "gaussian",
  prior = c(prior_intercept, prior_country),
  warmup = 1000,
  iter = 3000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)
save(Model4,file="Model4.RData")


# Model 5
Model5 <- brm(
  formula = formula5,
  data = data,
  family = bernoulli("logit"),
  # family = "gaussian",
  prior = prior_thresholds,
  warmup = 1000,
  iter = 3000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)
save(Model5,file="Model5.RData")

# Model 6
Model6 <- brm(
  formula = formula6,
  data = data,
  family = bernoulli("logit"),
  # family = "gaussian",
  prior = prior_thresholds,
  warmup = 1000,
  iter = 3000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)
save(Model6,file="Model6.RData")

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Build the covariance matrix
# Extract posterior samples from the models
# posterior_samples_Model1 <- posterior_samples(Model1)
posterior_samples_Model2 <- posterior_samples(Model2)
posterior_samples_Model3 <- posterior_samples(Model3)
# posterior_samples_Model4 <- posterior_samples(Model4)
posterior_samples_Model5 <- posterior_samples(Model5)
posterior_samples_Model6 <- posterior_samples(Model6)

# Combine all posterior samples into one dataframe
all_posterior_samples <- bind_rows(
                                   posterior_samples_Model2, 
                                   posterior_samples_Model3, 
                                   posterior_samples_Model5, 
                                   posterior_samples_Model6)
colnames(all_posterior_samples)

# Calculate covariance matrix
cov_matrix <- cov(all_posterior_samples[, c("b_marital", "b_employed", "b_demo_area", "b_demo_edu", 
                                            "b_demo_job_status", "b_demo_income_level", 
                                            "b_belong_sum", "b_burden_sum", "b_communal_sum", 
                                            "b_horizontal_individualism", "b_vertical_individualism", 
                                            "b_horizontal_collectivism", "b_vertical_collectivism")])

print(cov_matrix)
