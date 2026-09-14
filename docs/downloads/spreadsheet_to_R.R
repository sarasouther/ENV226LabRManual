# ============================================================================
# ENV 226 Lab - Working with data in a spreadsheet
#
# Goal: reproduce, in R, the summary statistics you just calculated by hand
# in Google Sheets (or Excel). The two sets of numbers should match exactly.
#
# Before you run this:
#   1. Your exported CSV is in this same folder, named WelcomeToENV226.csv
#   2. Session > Set Working Directory > To Source File Location
# ============================================================================


# ---- Read in your data -----------------------------------------------------

if (!file.exists("WelcomeToENV226.csv")) {
  stop("Can't find WelcomeToENV226.csv in this folder.\n",
       "  1. Check Session > Set Working Directory > To Source File Location\n",
       "  2. Check the file is named exactly WelcomeToENV226.csv",
       call. = FALSE)
}

data <- read.csv("WelcomeToENV226.csv")


# ---- Look before you calculate ---------------------------------------------
# str() shows every column and what R thinks it holds.
#   num = a number, chr = text.
# If a column you expect to be numeric says chr, there is something
# non-numeric in it - a stray "cm", a note, a typed dash. Find it and fix it
# in the spreadsheet, then re-export. Do not fix it here.

str(data)

head(data)   # the first few rows, to confirm nothing shifted on export


# ---- The five statistics, for DBH ------------------------------------------
# data$DBH means "the column named DBH, from the object named data".
#
# Spreadsheet          R
# -----------          -
# =COUNT(D2:D25)       length(data$DBH)
# =AVERAGE(D2:D25)     mean(data$DBH)
# =MEDIAN(D2:D25)      median(data$DBH)
# =MIN(D2:D25)         min(data$DBH)
# =MAX(D2:D25)         max(data$DBH)
#
# Note what is NOT in the R versions: a row number. That is the whole point.

length(data$DBH)
mean(data$DBH)
median(data$DBH)
min(data$DBH)
max(data$DBH)


# ---- The same five, for DistanceFrHome -------------------------------------

length(data$DistanceFrHome)
mean(data$DistanceFrHome)
median(data$DistanceFrHome)
min(data$DistanceFrHome)
max(data$DistanceFrHome)

# Compare that mean and that median. They are not the same number.
# Which one better describes a typical ENV 226 student? (Assignment item 4.)


# ---- All five at once, if you want them together ---------------------------
# summary() gives you most of them in one line. It reports the 1st and 3rd
# quartiles too - we get to those next week.

summary(data$DBH)


# ---- Counting the categories -----------------------------------------------
# =COUNTIF(C2:C25,"Dog")   ->   table(data$FavoritePet)
#
# table() counts every category at once, INCLUDING ones you did not expect.
# If you see "Dog" and "dog" as separate entries, that is the quality-control
# problem from Step 6, and it is in your data, not in your code.

table(data$FavoritePet)

# Does it add up to your n?
sum(table(data$FavoritePet))


# ---- Group means -----------------------------------------------------------
# =AVERAGEIF(C2:C25,"Dog",D2:D25)  ->  one line, all groups:
#
# Read the formula "DBH ~ FavoritePet" as: DBH as a function of FavoritePet.
# The ~ shows up constantly from here on.

aggregate(DBH ~ FavoritePet, data = data, FUN = mean)

# Swap mean for anything else and it still works:
aggregate(DBH ~ FavoritePet, data = data, FUN = length)   # the group counts


# ---- A tidyverse version of the same thing ---------------------------------
# You do not need this today. It is here because this is the style you will
# see in most modern R code, and because it reads almost like a sentence.
# Uncomment to run (tidyverse must be installed - see Chapter 2, Step 10).

# library(tidyverse)
#
# data |>
#   group_by(FavoritePet) |>
#   summarise(
#     n         = n(),
#     mean_DBH  = mean(DBH),
#     median_DBH = median(DBH)
#   )


# ---- Step 14: add a row and re-run -----------------------------------------
# Add one new student to the bottom of your spreadsheet, export again, then
# run these two lines. The mean changes.
#
# In the spreadsheet, your =AVERAGE(D2:D25) did NOT change, because the new
# row is row 26 and the formula never heard of it.

# data <- read.csv("WelcomeToENV226.csv")
# mean(data$DBH)


# ---- Writing results back out ----------------------------------------------
# If you want your group means as a file you can drop into your write-up:

group_means <- aggregate(DBH ~ FavoritePet, data = data, FUN = mean)
group_means

# write.csv(group_means, "group_means.csv", row.names = FALSE)
#
# row.names = FALSE stops R from adding a pointless column of row numbers,
# which is a small thing that will annoy you for years if nobody mentions it.
