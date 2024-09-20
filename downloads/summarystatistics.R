#Lesson 2: Descriptive statistics

# URL to the raw dataset on GitHub
url <- "https://raw.githubusercontent.com/sarasouther/ENV226LabRManual/refs/heads/main/downloads/%20lesson2data.csv"

# Import the CSV file into R
lesson2_data <- read.csv(url)

# View the first few rows of the dataset to verify
head(lesson2_data)

# We will work with the popular suite of packages, 'tidyverse', today
install.packages("tidyverse")
library(tidyverse)

# Function to calculate standard error - you don't need to create a function for the other descriptors that we talked about - just use the R functions
standard_error <- function(x) {
  sd(x) / sqrt(length(x))
}

# Summarize mean and standard error for each species
summary_stats <- lesson2_data %>%
  group_by(Species) %>% # Tell the program to group by
  summarise( # tell R to summarize among those groups
    mean_petal_length = mean(Petal.length, na.rm = TRUE), #creates an object called mea_petal_length that is the mean of the petal lengths for each species
    se_petal_length = standard_error(Petal.length)
  )

# View the summarized data
print(summary_stats)

# Now can you modify or generate your own code to derive the median petal length for the three species?

# Plot mean petal length with standard error bars
ggplot(summary_stats, aes(x = Species, y = mean_petal_length, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_petal_length - se_petal_length, 
                    ymax = mean_petal_length + se_petal_length), 
                width = 0.2, 
                position = position_dodge(0.7)) +
  labs(title = "Median Petal Length of Different Species",
       x = "Species",
       y = "Mean Petal Length (with SE)") +
  theme_minimal() +
  theme(legend.position = "none")

# Create a boxplot for petal length by species - an easy way to look at the median!
ggplot(lesson2_data, aes(x = Species, y = Petal.length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Petal Length Distribution by Species",
       x = "Species",
       y = "Petal Length") +
  theme_minimal() +
  theme(legend.position = "none")

# Google box plots and describe what each part of the box plot is

# Turn into your TA (put in a word or other document)
# 1. Summary statistics for your flower species (mean, median, and se)
# 2. The two plots you created
# 3. Description of what a box plot shows
