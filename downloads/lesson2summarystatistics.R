#set your working directory and import data

# Specify the path to the file in the 'downloads' directory
file_path <- "downloads/lesson2data.csv"

# Import the CSV file using read.csv
example_data <- read.csv(file_path)

# View the first few rows of the data
head(example_data)
