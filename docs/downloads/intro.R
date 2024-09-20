# This document shows the basic elements for setting up R files

# Libraries to be loaded
library(tidyverse)

# Remember there are two ways to install packages, the first option is the packages tab on your screen, and the second is using code:
install.packages("tidyverse")

# Set your own working directory here 
setwd("your_path_to_your_working_folder") 

# Read in a file
read.csv("your_data.csv")

# Write a file
write.csv(your_object_to_export, "your_file_name.csv") 