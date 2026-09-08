# In this R script we will explore different form of statistical tests!

# Load libraries
library(tidyverse)

# -------------------------------
# 1. G-test (Categorical Response and Categorical Explanatory Variable)

# Example: Bird nest preferences
data_g_test <- data.frame(
  TreeSpecies = factor(rep(c("Oak", "Pine", "Maple"), each = 20)),
  NestPresence = sample(c("Yes", "No"), 60, replace = TRUE)
)

# Check out the data
data_g_test

# Run a G-test (Chi-squared test)
g_test_result <- chisq.test(table(data_g_test$TreeSpecies, data_g_test$NestPresence))
print(g_test_result)

# Create a bar plot
ggplot(data_g_test, aes(x = TreeSpecies, fill = NestPresence)) +
  geom_bar(position = "dodge") +
  labs(x = "Tree species", y = "Count")

# -------------------------------

# -------------------------------
# t-test (Continuous Response and Categorical Explanatory Variable)

# Two species of frogs example
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
# -------------------------------

# -------------------------------
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
# -------------------------------

# -------------------------------
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
# -------------------------------

# -------------------------------
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

# -----------------------------
# Assignment 
# -----------------------------

# Datasets for each scenario! Add and modify code below!

# Scenario 1: Comparing Two Groups
# A biologist measures the leaf nitrogen content (%) in plants from two different habitats: Sunny vs. Shady.

leaf_data <- data.frame(
  habitat = rep(c("Sunny", "Shady"), each = 8),
  nitrogen = c(2.3, 2.5, 2.7, 2.8, 2.9, 3.0, 2.6, 2.4,
               1.8, 2.0, 2.1, 2.3, 1.9, 2.2, 2.1, 2.0)
)

# Scenario 2: More Than Two Groups
# An ecologist measures the average number of pollinator visits to flowers of three species: Aster, Goldenrod, and Sunflower.

pollinator_data <- data.frame(
  species = rep(c("Aster", "Goldenrod", "Sunflower"), each = 6),
  visits = c(5, 6, 4, 7, 5, 6,
             8, 9, 10, 9, 11, 8,
             12, 11, 13, 12, 14, 13)
)

# Scenario 3: Relationship Between Two Variables
# A forester records tree diameter (cm) and age (years) for a sample of trees.

tree_data <- data.frame(
  diameter = c(10, 15, 20, 25, 30, 35, 40, 45),
  age = c(12, 18, 22, 30, 35, 40, 45, 50)
)

# Scenario 4: Categorical Data
# A conservationist records whether birds are Present or Absent at two sites: Restored and Unrestored.

bird_data <- matrix(
  c(25, 15,   # Restored: Present, Absent
    10, 20),  # Unrestored: Present, Absent
  nrow = 2, byrow = TRUE
)
rownames(bird_data) <- c("Restored", "Unrestored")
colnames(bird_data) <- c("Present", "Absent")