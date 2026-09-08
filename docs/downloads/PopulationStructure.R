# ============================================================================
# ENV 226 Lab - Population structure and demography
#
# Reconstructs a population's history from a single visit.
#
# Your datasheet needs five columns:
#     Site           where you worked (text)
#     Group          what you are comparing - plot, stand, time period (text)
#     IndividualID   one number per individual (number)
#     Value          the measurement: DBH in cm, or age at death in years
#     ValueType      "DBH_cm" or "AgeAtDeath_yr"
#
# ============================================================================

library(tidyverse)

# ---- 1. SET THIS BEFORE YOU RUN ANYTHING -----------------------------------
#
# This is the most important line in the script, and the easiest to get wrong.
#
# "standing" = you measured individuals that are ALIVE right now.
#              (Track A and B - every tree in your plot is a survivor.)
#
# "death"    = you recorded the age at which individuals DIED.
#              (Track C - every row in a cemetery record is a death.)
#
# Survivors and deaths are not the same data, and they do not become a life
# table the same way. If you set this wrong your survivorship curve will be
# backwards and it will still look plausible, which is why you must think
# about it rather than guess.

record_type <- "standing"     # "standing" or "death"

class_width <- 5              # cm for DBH, years for age at death

# ---- 2. Read your data -----------------------------------------------------
# Put this script and your datasheet in the same folder, then use
# Session > Set Working Directory > To Source File Location.

data <- read.csv("population_structure_datasheet.csv")
data <- data %>% filter(!is.na(Value))

if (nrow(data) == 0) {
  stop("No data to analyse. The Value column in your datasheet is still empty - ",
       "fill in your measurements before running this script.", call. = FALSE)
}

head(data)
cat("Individuals recorded:", nrow(data), "\n")
table(data$Group)

# ---- 3. Bin individuals into classes ---------------------------------------

data <- data %>%
  mutate(ClassStart = floor(Value / class_width) * class_width)

class_counts <- data %>%
  count(Group, ClassStart, name = "Count") %>%
  arrange(Group, ClassStart)

print(class_counts)

# ---- 4. Look at the shape FIRST --------------------------------------------
# The shape of this distribution is your result. Everything after this is
# putting numbers on something you can already see.

ggplot(class_counts, aes(x = ClassStart, y = Count)) +
  geom_col() +
  facet_wrap(~ Group) +
  xlab(paste0("Class (width = ", class_width, ")")) +
  ylab("Number of individuals") +
  theme_minimal()

# Reverse-J  -> steady recruitment
# Gap in the small classes -> recruitment stopped at some point
# One tall bar -> a single establishment pulse

# ---- 5. Build the life table -----------------------------------------------

life_table <- function(df) {
  df <- arrange(df, ClassStart)

  if (record_type == "standing") {
    # Each count is individuals that SURVIVED to this class.
    Sx <- df$Count
  } else {
    # Each count is individuals that DIED in this class, so survivors at the
    # start of a class is everyone who had not died yet.
    total <- sum(df$Count)
    Sx <- total - c(0, head(cumsum(df$Count), -1))
  }

  lx <- Sx / Sx[1]                        # proportion surviving to class x
  dx <- lx - c(tail(lx, -1), 0)           # proportion dying during class x
  qx <- ifelse(lx > 0, dx / lx, NA)       # per-class mortality rate

  tibble(
    ClassStart = df$ClassStart,
    Sx = Sx,
    lx = round(lx, 4),
    dx = round(dx, 4),
    qx = round(qx, 4)
  )
}

tables <- class_counts %>%
  group_by(Group) %>%
  group_modify(~ life_table(.x)) %>%
  ungroup()

print(as.data.frame(tables))

if (any(tables$lx > 1, na.rm = TRUE)) {
  cat("\nNOTE: some lx values are greater than 1.\n")
  cat("That is not an error. It means a larger class holds MORE individuals than\n")
  cat("the smallest class - so this population cannot have a stable stage\n")
  cat("distribution, and the static life table's central assumption is violated.\n")
  cat("The negative dx and qx values in the table above are the same symptom.\n")
  cat("Say so in your assumption paragraph, and say which conclusions survive.\n")
}

# ---- 6. Survivorship curve -------------------------------------------------
# Plotted on a log scale, because that is what makes the three types
# distinguishable by eye:
#   Type I   - flat, then falls off a cliff late
#   Type II  - a straight line (constant mortality at every age)
#   Type III - drops steeply early, then flattens

ggplot(filter(tables, lx > 0), aes(x = ClassStart, y = lx, colour = Group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_log10() +
  xlab("Class") +
  ylab("Proportion surviving (log scale)") +
  theme_minimal()

# ---- 7. Measurement error check (Track A and B) ----------------------------
# Enter your paired re-measurements here, then run these three lines.

observer_1 <- c()   # e.g. c(12.4, 30.1, 8.8, 45.0, 22.3)
observer_2 <- c()   # the second person's numbers, same five trees, same order

if (length(observer_1) > 0 && length(observer_1) == length(observer_2)) {
  differences <- abs(observer_1 - observer_2)
  cat("Mean absolute difference between observers:",
      round(mean(differences), 2), "\n")
  cat("Largest single disagreement:", round(max(differences), 2), "\n")
  cat("Is that big enough to move a tree into a different class?\n")
}
