library(tidyverse)
library(gridExtra) #REMEMBER TO INSTALL!!!

#import data

# URL to the raw dataset on GitHub
url <- "https://raw.githubusercontent.com/sarasouther/ENV226LabRManual/refs/heads/main/downloads/%20lesson2data.csv"

# Import the CSV file into R
lesson2_data <- read.csv(url)

# View the first few rows of the dataset to verify
head(lesson2_data)

#change below once finalized
setwd("/Users/sks379/Desktop/ENV226LabRManual/downloads") #laptop
mousetable <- read.csv("naturalselectiondatasheet.csv", header=TRUE) 

# Load the data
data <- read.csv("naturalselectiondatasheet.csv")

# Ensure the 'Order_LightesttoDarkest' column is numeric
data$Order_LightesttoDarkest <- as.numeric(data$Order_LightesttoDarkest)

# Define a named vector that matches coat color names to actual colors
color_mapping <- c("White" = "white", 
                   "Peach" = "peachpuff", 
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

