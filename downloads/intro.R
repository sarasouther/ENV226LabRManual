# This document shows the basic elements for setting up R files

# Libraries to be loaded
library(tidyverse)

# Remember there are two ways to install packages, the first option is the packages tab on your screen, and the second is using code:
install.packages("tidyverse")

# Set your own working directory 

# Read in a file
data <- read.csv("WelcomeToENV226.csv")

# Look at your raw data
data

# Let's visualize your data
ggplot(data, aes(x = DistanceFrHome)) +
  geom_histogram(bins = 10)

ggplot(data, aes(x = NumberOfRoommates)) +
  geom_bar()

ggplot(data, aes(x = FavoritePet)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = DBH)) +
  geom_histogram(bins = 10) +
  labs(x = "Diameter at Breast Height (cm)",
       y = "Number of trees")
