# This R file tests the effect of disturbance and direction on invasive species density

# Load libraries
library(tidyverse)
library(ggpubr)
library(car)

# Set your working directory
# Put this script and your datasheet in the SAME folder, then use
# Session > Set Working Directory > To Source File Location in RStudio.
# After that, the read.csv line below just works - no file paths to type.

# Load the data
data <- read.csv("invasivespeciesdatasheet.csv")

# Conduct statistical analysis
# The datasheet ships blank - fill in your counts before running the models.
if (all(is.na(data$NumberNonNativeSpecies))) {
  stop("No data to analyse. The NumberNonNativeSpecies column in ",
       "invasivespeciesdatasheet.csv is still empty - enter your field counts ",
       "before running this script.", call. = FALSE)
}

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
