# In this R script we will explore different form of statistical tests!

# Load libraries
library(tidyverse)

# 1. G-test (Categorical Response and Categorical Explanatory Variable)

# Create example data
data_g_test <- data.frame(
  TreeSpecies = factor(rep(c("Oak", "Pine", "Maple"), each = 20)),
  NestPresence = sample(c("Yes", "No"), 60, replace = TRUE)
)

# Run a G-test (Chi-squared test)
g_test_result <- chisq.test(table(data_g_test$TreeSpecies, data_g_test$NestPresence))
print(g_test_result)

# Create a bar plot
library(ggplot2)
ggplot(data_g_test, aes(x = TreeSpecies, fill = NestPresence)) +
  geom_bar(position = "dodge") +
  labs(title = "Bird Nest Preference by Tree Species", x = "Tree Species", y = "Count")

# t-test (Continuous Response and Categorical Explanatory Variable)

# Create example data
set.seed(123)
data_t_test <- data.frame(
  Species = factor(rep(c("A", "B"), each = 30)),
  Weight = c(rnorm(30, mean = 50, sd = 5), rnorm(30, mean = 55, sd = 5))
)

# Run t-test
t_test_result <- t.test(Weight ~ Species, data = data_t_test)
print(t_test_result)

# Create a boxplot
ggplot(data_t_test, aes(x = Species, y = Weight, fill = Species)) +
  geom_boxplot() +
  labs(title = "Frog Weight Comparison", x = "Species", y = "Weight (g)")

# ANOVA

# Create example data
set.seed(123)
data_anova <- data.frame(
  Habitat = factor(rep(c("Desert", "Forest", "Wetland"), each = 30)),
  PlantHeight = c(rnorm(30, mean = 25, sd = 3), rnorm(30, mean = 45, sd = 5), rnorm(30, mean = 35, sd = 4))
)

# Run ANOVA
anova_result <- aov(PlantHeight ~ Habitat, data = data_anova)
summary(anova_result)

# Create a boxplot
ggplot(data_anova, aes(x = Habitat, y = PlantHeight, fill = Habitat)) +
  geom_boxplot() +
  labs(title = "Plant Height by Habitat", x = "Habitat", y = "Plant Height (cm)")

# Linear regression

# Create example data
set.seed(123)
data_lm <- data.frame(
  WaterTemp = runif(50, 10, 30),
  FishCount = 10 + 3 * runif(50, 10, 30) + rnorm(50, 0, 5)
)

# Run linear regression
lm_result <- lm(FishCount ~ WaterTemp, data = data_lm)
summary(lm_result)

# Create a scatter plot with regression line
ggplot(data_lm, aes(x = WaterTemp, y = FishCount)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fish Count vs Water Temperature", x = "Water Temperature (°C)", y = "Fish Count")

# Logistic regression

# Create example data
set.seed(123)
data_logit <- data.frame(
  Elevation = runif(100, 100, 3000),
  BirdPresence = sample(c(0, 1), 100, replace = TRUE)
)

# Run logistic regression
logit_result <- glm(BirdPresence ~ Elevation, data = data_logit, family = binomial)
summary(logit_result)

# Create a scatter plot with a logistic curve
ggplot(data_logit, aes(x = Elevation, y = BirdPresence)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  labs(title = "Bird Presence vs Elevation", x = "Elevation (meters)", y = "Bird Presence (Yes/No)")


