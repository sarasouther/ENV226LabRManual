# This code allows us to look at the effects of natural selection due to predation

#Load libraries
library(tidyverse)
library(gridExtra) #REMEMBER TO INSTALL!!!

# Set your working directory
setwd("addyourworkingdirectoryhere") 

# Load the data
data <- read.csv("naturalselectiondatasheet.csv")

# Ensure the 'Order_LightesttoDarkest' column is numeric
data$Order_LightesttoDarkest <- as.numeric(data$Order_LightesttoDarkest)

# Define a named vector that matches coat color names to actual colors
color_mapping <- c("White" = "white", 
                   "Yellow" = "yellow", 
                   "Green" = "#00FF7F", 
                   "Light blue" = "lightblue", 
                   "Pink" = "#FFC0CB", 
                   "Hot pink" = "#FF1493", 
                   "Black" = "black")

# Create a subset for the first generation
first_gen <- data %>% dplyr::filter(Generation == "1stGen")

# Create a subset for the final generation
final_gen <- data %>% dplyr::filter(Generation == "4thGen")

# Plot for the first generation
p1 <- ggplot(first_gen, aes(x = reorder(MouseCoatColor, Order_LightesttoDarkest), y = Frequency, fill = MouseCoatColor)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_mapping) +
  labs(title = "1st generation",
       x = "Mouse coat color",
       y = "Frequency") +
  theme_minimal() 

# Plot for the final generation
p2 <- ggplot(final_gen, aes(x = reorder(MouseCoatColor, Order_LightesttoDarkest), y = Frequency, fill = MouseCoatColor)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_mapping) +
  labs(title = "4th generation",
       x = "Mouse coat color",
       y = "Frequency") +
  theme_minimal() 

grid.arrange(p1, p2, ncol = 2)

