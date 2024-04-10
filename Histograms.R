library(ggplot2)
library(dplyr) 

data <-  read_excel("data/data.xlsx", sheet = "column_data") 
data$country <- ifelse(data$country == 1, "Turkey",
                            ifelse(data$country == 2, "US", "Korea")) 

data %>%
  ggplot(aes(x = num_children)) +
  geom_histogram(binwidth = 1) + 
  facet_wrap(~ country) +
  labs(title = "Distribution of Number of Children by Country",
       x = "Number of Children",
       y = "Count") +
  theme_minimal() 

ggsave(filename = "num_children_histogram.png", plot = my_plot, 
       width = 8, height = 6, units = "in", dpi = 300) 



