# This document shows the basic elements for setting up R files

# ---- Installing packages ---------------------------------------------------
# Remember there are two ways to install packages: the Packages tab on your
# screen, or code. You only need to install a package ONCE per computer.
# This line checks first, so it is safe to re-run.

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

# ---- Loading packages ------------------------------------------------------
# Install once, but LOAD every time you open R.

library(tidyverse)

# ---- Setting your working directory ----------------------------------------
# Put this script and your data file in the SAME folder (your EcologyLab
# folder), then use:
#     Session > Set Working Directory > To Source File Location
# After that, the read.csv line below just works - no file paths to type.

# ---- Read in a file --------------------------------------------------------
# Download the class data sheet from the manual and save it in this folder as
# WelcomeToENV226.csv
#
# Your browser may save it with a longer name, like
# "WelcomeToENV226 - Sheet1.csv" - just rename it to match the line below.
#
# An example file with the same columns ships with the manual, so you can run
# this script before the class data is ready.

if (!file.exists("WelcomeToENV226.csv")) {
  stop("Can't find WelcomeToENV226.csv in this folder.\n",
       "  1. Check Session > Set Working Directory > To Source File Location\n",
       "  2. Check the file is named exactly WelcomeToENV226.csv",
       call. = FALSE)
}

data <- read.csv("WelcomeToENV226.csv")

# ---- Look at your raw data -------------------------------------------------

data

# Two other ways to look at it, which are more useful once the data gets bigger:
head(data)     # just the first few rows
str(data)      # the columns, and what type each one is

# ---- Let's visualize your data ---------------------------------------------

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
