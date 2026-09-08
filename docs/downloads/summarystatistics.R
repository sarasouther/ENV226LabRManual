#Lesson 2: Descriptive statistics

# Code from the document------------

#create vector of heights (cm) of one population of A. subverticulata
sedonapopulation <- c(3, 3, 3, 3, 7, 8, 9)
#take the mean
mean(sedonapopulation)
#calculate variance
var(sedonapopulation)
#calculate standard deviation
sd(sedonapopulation)
#calculate standard error
#base r doesn't have this function
#so we have to write our own
std_error <- function(x) sd(x)/sqrt(length(x))
std_error(sedonapopulation)

sum = 3+3+3+3+7+8+9 #add all the numbers in the sample
n = length(sedonapopulation) #or you can just calculate the number of height measurements
mean = sum/n; mean #divide sum by number

#We determine how much each observation varies from the mean.
diffobs1 = mean - 3
diffobs2 = mean - 3
diffobs3 = mean - 3
diffobs4 = mean - 3
diffobs5 = mean - 7 
diffobs6 = mean - 8
diffobs7 = mean - 9 

#Then we square each of these. 
diffobj1_sq = diffobs1^2
diffobj2_sq = diffobs2^2
diffobj3_sq = diffobs3^2
diffobj4_sq = diffobs4^2
diffobj5_sq = diffobs5^2
diffobj6_sq = diffobs6^2
diffobj7_sq = diffobs7^2

#Then we add the differences up.
sumofsquares = sum(diffobj1_sq, diffobj2_sq, diffobj3_sq, diffobj4_sq, diffobj5_sq, diffobj6_sq, diffobj7_sq)
#Divide the sum of squares by n - 1.
variance = sumofsquares/(n-1); variance 

#an example of an unskewed population
sedona_unskewed <- c(1, 2, 3, 4, 5, 6, 7)
mean(sedona_unskewed)
median(sedona_unskewed)

#previous sedona population; skewed
sedonapopulation <- c(3, 3, 3, 3, 7, 8, 9)
mean(sedonapopulation)
median(sedonapopulation)

sedona_unskewed <- c(7, 2, 2, 3, 3, 3, 3, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 0.5)
mean(sedona_unskewed)
median(sedona_unskewed)
#I'm renaming sedonapopulation, sedona_skewed for this example
sedona_skewed <- c(3, 3, 3, 3, 7, 3, 4, 5, 6, 3, 3, 3, 4, 4, 6, 7, 8, 9, 3, 4, 5, 2)
mean(sedona_skewed)
median(sedona_skewed)

hist(sedona_unskewed, main = "Mostly Unskewed", xlab = "Plant height (cm)", breaks=5)

h <- hist(sedona_unskewed, main = "Mostly Unskewed", xlab = "Plant height (cm)", breaks=5)

xfit <- seq(min(sedona_unskewed), max(sedona_unskewed), length = 40) 
yfit <- dnorm(xfit, mean = mean(sedona_unskewed), sd = sd(sedona_unskewed)) 
yfit <- yfit * diff(h$mids[1:2]) * length(sedona_unskewed) 

lines(xfit, yfit, col = "black", lwd = 2)

hist(sedona_skewed, main = "Skewed", xlab = "Plant height (cm)", breaks = 5)

boxplot(sedona_skewed, main="Skewed", ylab="Plant height (cm)")

#Let's add a plant height of 20.
sedona_skewed <- c(3, 3, 3, 3, 7, 3, 4, 5, 6, 3, 3, 3, 4, 4, 6, 7, 8, 9, 3, 4, 5, 2, 20)
boxplot(sedona_skewed, main="Skewed", ylab="Plant height (cm)")

# Code for your analysis-------------

# Remember there are two ways to install packages, the first option is the packages tab on your screen, and the second is using code:
install.packages("tidyverse")

# # We will work with the popular suite of packages, 'tidyverse', today
library(tidyverse)

# Set your own working directory here 
# Put this script and your datasheet in the SAME folder, then use
# Session > Set Working Directory > To Source File Location in RStudio.
# After that, the read.csv line below just works - no file paths to type.

# Import the CSV file into R
lesson2_data <- read.csv("summarystatsdata.csv")

# View the first few rows of the dataset to verify
head(lesson2_data)

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
  labs(title = "Mean Petal Length of Different Species",
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
