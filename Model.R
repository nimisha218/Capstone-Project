install.packages(c("brms", "tidyverse", "ggplot2", "loo", "rstan", "performance")) 

library(brms)
library(tidyverse)
library(ggplot2)
library(loo)
library(rstan) 
library(performance)
library(readxl)

data <-  read_excel("/Users/nimishabhagat/Documents/Capstone/Code/Suicidal_Ideation/data/data.xlsx", sheet = "column_data") 
data <- na.omit(data)  

summary(data)  # Summary statistics
str(data)      # Data types
head(data)     # View the first few rows

formula1 <- suicide_lifetime ~ birth_sex  + age + marital_status + num_siblings + num_children + (1 | country) 
formula2 <- suicide_lifetime ~ birth_sex * (age + marital_status) + (1 | country) 

prior_thresholds <- c(
  prior(normal(0,0.5), class = Intercept),
  prior(normal(0,0.5), class = b),
  prior(student_t(3,0,1), class = sd)
)

Model1 <- brm(
  formula = formula1,
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

save(Model1,file="Model1.RData")

Model2 <- brm(
  formula = formula2,
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

save(Model2,file="Model2.RData")

loo_results <- list(
  Intersectional_Model1 = loo(Model1),
  Intersectional_Model2 = loo(Model2)
)

waic_results <- list( 
  Intersectional_Model1 = waic(Model1),
  Intersectional_Model2 = waic(Model2)
)

r2_results<-list( 
  Modelraw_Intersectional_Model1_r2=performance::r2(Model1),
  Modelraw_Intersectional_Model2_r2=performance::r2(Model2)
)

save(loo_results,file="LooModelReplication.RData")
save(waic_results,file="WAICModelReplication.RData")
save(r2_results,file="R2ModelReplication.RData")

loo_results_estimate <- list(
  Intersectional_Model1_estimate = loo_results$Intersectional_Model1$estimates,
  Intersectional_Model2_estimate = loo_results$Intersectional_Model2$estimates
)

waic_results_estimate <- list( 
  Intersectional_Model1_estimate = waic_results$Intersectional_Model1$estimates,
  Intersectional_Model2_estimate = waic_results$Intersectional_Model2$estimates
)

parameter_df<-tibble(Median=NA,CI=NA,Direction=NA,Significance=NA,Large=NA)

parameter_samples <- posterior_samples(Model1)
colPosteriorall<-colnames(parameter_samples)
colPosterior <- colPosteriorall[grep("^b_", colPosteriorall)]

for (i in colPosterior){
  print(i)
  # Calculate the median of the posterior distribution
  median_estimate <- median(parameter_samples[[i]])
  
  # Calculate the 95% Bayesian credible interval
  credible_interval <- quantile(parameter_samples[[i]], c(0.025, 0.975))
  
  # Calculate the probability of direction (96% chance of being positive)
  probability_direction <- mean(parameter_samples[[i]] > 0)
  
  # Calculate the probability of practical significance (95% chance of being > 0.05)
  probability_practical_significance <- mean(parameter_samples[[i]] > 0.05)
  
  # Calculate the probability of having a large effect (89% chance of being > 0.30)
  probability_large_effect <- mean(parameter_samples[[i]] > 0.30)
  
  parameter_df<-rbind(parameter_df,c(median_estimate,credible_interval,probability_direction,probability_practical_significance,probability_large_effect))
  
}

parameter_df<-parameter_df[-1,]

plottingBar <- function(gr1,l,k) { # create a function with the name my_function
  titlex=paste("Probability of Suicide Lifetime by birth_sex ",k)
  titlex=paste(titlex,l)
  
  m=ggplot(gr1, aes(x = birth_sex, y = estimate__*100, fill = !! sym(k), colour = !! sym(k))) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__*100, ymax = upper__*100), width = 0.11, position = position_dodge(width = 0.3)) +
    labs(
      title = titlex ,
      x = "Birth Sex",
      y = "Probability Of Suicide Lifetime"
    )+scale_y_continuous(
      limits =   c(0,100))+
    theme_minimal()
  return (m)
}

##

modelname="Model1 Intersection"
conditions <- expand.grid(marital_status=1, birth_sex =unique(data$birth_sex),age=27)
mod_plot <- conditional_effects(Model1,categorical = TRUE, effect ="birth_sex",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("birth_sex")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="birth_sex"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_birth_sex.jpeg", plot = plotb, width = 8, height = 6) 

####

# rater-gender,rater-race
conditions <- expand.grid(birth_sex =unique(data$birth_sex),marital_status=unique(data$marital_status))

mod_plot <- conditional_effects(Model1,categorical = TRUE, effect ="marital_status" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("marital_status")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="marital_status"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_birth_sex_and_marital_status.jpeg", plot = plotb, width = 16, height = 8) 

####

modelname="Model1 Intersection"
conditions <- expand.grid(birth_sex=1, marital_status =unique(data$marital_status),age=27)
mod_plot <- conditional_effects(Model1,categorical = TRUE, effect ="marital_status",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("marital_status")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="birth_sex"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_birth_sex.jpeg", plot = plotb, width = 8, height = 6) 
##

