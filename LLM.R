# Load required libraries
library(readxl)
library(brms)
library(loo)
library(bayesplot)
library(dplyr)

# Load the data set
data <- read_excel("/Users/nimishabhagat/Documents/Capstone/Code/Suicidal_Ideation/data/data.xlsx", sheet = "Sheet1")

# Rename column names
names(data) <- c("country", "birth_sex", "age", "marital_status", "num_siblings", "num_children", "suicide_lifetime")

# ###################Models###########################

# Define the formula for the model
formula <- suicide_lifetime ~ age + birth_sex + num_siblings + num_children + marital_status + (1 | country)
# Additional formulas worth experimenting with
formula_1 <- suicide_lifetime ~ age + marital_status + num_siblings + num_children + age*marital_status + (1 ∣ country)
formula_2 <- suicide_lifetime ~ age + age*num_children + age*marital_status + marital_status + num_siblings + num_children + (1 | country)

# Define prior thresholds
prior_thresholds <- c(
  prior(normal(0,0.5), class = Intercept),
  prior(normal(0,0.5), class = b),
  prior(student_t(3,0,1), class = sd)
)

# Fit the Bayesian model
bayesian_model_0 <- brm(
  formula = formula,
  data = data,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)

bayesian_model_1 <- brm(
  formula = formula_1,
  data = data,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)

bayesian_model_2 <- brm(
  formula = formula_2,
  data = data,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)

# Save the model
save(bayesian_model_0, file = "BayesianModel_0.RData")
save(bayesian_model_1, file = "BayesianModel_1.RData")
save(bayesian_model_2, file = "BayesianModel_2.RData")


# Compute LOO and WAIC
loo_results <- loo(bayesian_model)
waic_results <- waic(bayesian_model)

# Save LOO and WAIC results
save(loo_results, file = "LOO_results.RData")
save(waic_results, file = "WAIC_results.RData")

# Extract posterior samples for the parameter
parameter_samples <- posterior_samples(bayesian_model)

# Define parameter
parameter_df <- data_frame(Median = NA, CI = NA, Direction = NA, Significance = NA, Large = NA)

# Extract parameter names from the posterior samples
colPosteriorall <- colnames(parameter_samples)
colPosterior <- colPosteriorall[grep("^b_", colPosteriorall)]

for (i in colPosterior) {
  # Calculate median estimate
  median_estimate <- median(parameter_samples[[i]])
  
  # Calculate 95% Bayesian credible interval
  credible_interval <- quantile(parameter_samples[[i]], c(0.025, 0.975))
  
  # Calculate probability of direction
  probability_direction <- mean(parameter_samples[[i]] > 0)
  
  # Calculate probability of practical significance
  probability_practical_significance <- mean(parameter_samples[[i]] > 0.05)
  
  # Calculate probability of having a large effect
  probability_large_effect <- mean(parameter_samples[[i]] > 0.30)
  
  parameter_df <- rbind(parameter_df, c(median_estimate, credible_interval, probability_direction, probability_practical_significance, probability_large_effect))
}

parameter_df <- parameter_df[-1,]

# Print parameter results
print(parameter_df)

# TO DO - Plotting 

