#This R script analyzes population growth of Lemna minor

# Load necessary libraries
library(ggpubr)  # For ggarrange
library(tidyverse)

# Set your working directory
setwd("addyourworkingdirectoryhere") 

# Load the dataset
data <- read.csv("/duckweeddatasheet.csv")

# Compute log population size
data$log_PopulationSize <- log(data$PopulationSize)

# Filter the data for the low initial population (2 plants)
low_pop <- subset(data, Treatment == "LowInitialPopulationSize")

# Fit a linear model for log N vs time (Day) for low initial population
low_model <- lm(log_PopulationSize ~ Day, data = low_pop)

# Display the slope (r estimate) for low initial population
summary(low_model)

# Extract the intercept and slope
intercept <- coef(low_model)[1]
slope <- coef(low_model)[2]

# Derive the initial population size
initial_population_size <- exp(intercept)

# Output the results
cat("Slope (r):", slope, "\n")
cat("Initial population size (N(0)):", initial_population_size, "\n")

# Plot log N vs Day for low initial population
plot_low <- ggplot(low_pop, aes(x = Day, y = log_PopulationSize)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, col = "blue") +
  ggtitle("Low initial population size (2 plants)") +
  xlab("Time (Days)") +
  ylab("Log population size (N)") +
  theme_minimal()

# Repeat for high initial population (15 plants)
high_pop <- subset(data, Treatment == "HighInitialPopulationSize")

# Fit a linear model for log N vs time (Day) for high initial population
high_model <- lm(log_PopulationSize ~ Day, data = high_pop)

# Display the slope (r estimate) for high initial population
summary(high_model)

# Extract the intercept and slope
intercept <- coef(high_model)[1]
slope <- coef(high_model)[2]

# Derive the initial population size
initial_population_size <- exp(intercept)

# Output the results
cat("Slope (r):", slope, "\n")
cat("Initial population size (N(0)):", initial_population_size, "\n")

# Plot log N vs Day for high initial population
plot_high <- ggplot(high_pop, aes(x = Day, y = log_PopulationSize)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, col = "red") +
  ggtitle("High initial population size (15 plants)") +
  xlab("Time (Days)") +
  ylab("Log population size (N)") +
  theme_minimal()

# Combine the two plots using ggarrange
combined_plot <- ggarrange(plot_low, plot_high, 
                           ncol = 2, nrow = 1,  # Arrange side by side
                           labels = c("A", "B"))  # Add labels A and B

print(combined_plot)

# Geometric population growth-------------

# Function to calculate lambda over the entire time frame (from Day 0 to Day 14)
calculate_lambda <- function(initial_pop, final_pop, total_time) {
  (final_pop / initial_pop)^(1 / total_time)
}

# Filter the data for the low initial population (2 plants)
low_pop_data <- data %>% filter(Treatment == "LowInitialPopulationSize")
initial_low <- low_pop_data %>% filter(Day == 0) %>% pull(PopulationSize)
final_low <- low_pop_data %>% filter(Day == 14) %>% pull(PopulationSize)

# Calculate lambda for low initial population
lambda_low <- calculate_lambda(initial_low, final_low, total_time = 14)
cat("Lambda for LowInitialPopulationSize (2 plants):", lambda_low, "\n")

# Filter the data for the high initial population (15 plants)
high_pop_data <- data %>% filter(Treatment == "HighInitialPopulationSize")
initial_high <- high_pop_data %>% filter(Day == 0) %>% pull(PopulationSize)
final_high <- high_pop_data %>% filter(Day == 14) %>% pull(PopulationSize)

# Calculate lambda for high initial population
lambda_high <- calculate_lambda(initial_high, final_high, total_time = 14)
cat("Lambda for HighInitialPopulationSize (15 plants):", lambda_high, "\n")

# Carrying capacity--------------------------

# Calculate lambda for each time step (from Nt to Nt+1)
data <- data %>%
  group_by(Treatment) %>%
  arrange(Day) %>%
  mutate(lambda = lead(PopulationSize) / PopulationSize) %>%
  filter(!is.na(lambda))  # Remove NA lambda values

# Plot lambda vs. population size (Nt) and fit a linear model
ggplot(data, aes(x = PopulationSize, y = lambda)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  xlab("Population size (N_t)") +
  ylab("Geometric growth rate (λ)") +
  ggtitle("Geometric growth rate (λ) as a function of population size (N_t)") +
  theme_minimal()

# Fit a linear model for lambda vs. Nt
lambda_model <- lm(lambda ~ PopulationSize, data = data)

# Summary of the model
summary(lambda_model)

# Extract intercept and slope
intercept <- coef(lambda_model)[1]  # This approximates r when N = 0
slope <- coef(lambda_model)[2]      # The slope is used in determining K

# Calculate the carrying capacity K (where λ = 1)
carrying_capacity_K <- (1 - intercept) / slope

cat("Estimated r (y-intercept where N = 0):", intercept, "\n")
cat("Estimated carrying capacity K (where λ = 1):", carrying_capacity_K, "\n")

# Did your container reach estimated carrying capacity?
