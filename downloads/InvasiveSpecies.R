# This R file tests the effect of disturbance and direction on invasive species density

# Load libraries
library(tidyverse)
library(ggpubr)
library(car)

# Set your working directory
setwd("addyourworkingdirectory") 

# Load the data
data <- read.csv("invasivespeciesdatasheet.csv")

# Conduct statistical analysis
model <- lm(NumberNonNativeSpecies ~ Transect, data = data)
Anova(model)

model <- lm(NumberNonNativeSpecies ~ TreeNumber, data = data)
Anova(model)

model <- lm(NumberNonNativeSpecies ~ DisturbancePresent, data = data)
Anova(model)

# Visualize results

# Plot for Transect
plot_transect <- ggplot(data, aes(x = Transect, y = NumberNonNativeSpecies)) +
  geom_boxplot(aes(fill = Transect)) +
  theme_minimal() +
  ylab(expression("Invasive species density (N/m"^2*")")) +
  xlab("Transect direction") +
  theme(legend.position = "none")

# Plot for TreeNumber
plot_treenumber <- ggplot(data, aes(x = TreeNumber, y = NumberNonNativeSpecies)) +
  geom_point(size = 3, aes(color = TreeNumber)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  ylab(expression("Invasive species density (N/m"^2*")")) +
  xlab(expression("Tree density (N/m"^2*")")) +
  theme(legend.position = "none")

# Plot for DisturbancePresent
plot_disturbance <- ggplot(data, aes(x = DisturbancePresent, y = NumberNonNativeSpecies)) +
  geom_boxplot(aes(fill = DisturbancePresent)) +
  theme_minimal() +
  ylab(expression("Invasive species density (N/m"^2*")")) +
  xlab("Disturbance (Yes / No)") +
  theme(legend.position = "none")

# Arrange the plots
combined_plot <- ggarrange(plot_transect, plot_treenumber, plot_disturbance, 
                           ncol = 3, nrow = 1)

# Print the combined plot
print(combined_plot)
