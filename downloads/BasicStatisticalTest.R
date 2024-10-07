# Basic statistical test

# Load libraries
library (tidyverse)

# Example dataset: the number of times each dog chose the blue ball in 10 trials
blue_choices <- c(7, 6, 4, 8, 5, 9, 6, 7, 5, 10)

# Perform a one-tailed t-test to see if the mean number of blue ball choices is greater than 5 (50/50 neutral)
t_test_result <- t.test(blue_choices, mu = 5, alternative = "greater")

# Display the test results
t_test_result

# Convert the data to a data frame for plotting
data <- data.frame(blue_choices = blue_choices)

# Create a box plot
ggplot(data, aes(x = "", y = blue_choices)) +
  geom_boxplot(fill = "#18DDC2FF", color = "black") +
  labs(x = NULL, y = "Number of blue choices") +
  theme_minimal()

# Calculate how much more frequently the dogs chose blue (as a percentage)
mean_blue <- mean(blue_choices)
neutral <- 5  # Neutral choice (50/50)
percent_increase <- ((mean_blue - neutral) / neutral) * 100

# Output the percentage increase
cat("The dogs chose the blue ball", round(percent_increase, 2), "% more frequently than expected under neutral conditions.\n")
