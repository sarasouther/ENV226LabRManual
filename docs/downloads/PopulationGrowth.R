# ============================================================================
# ENV 226 Lab - Population growth
#
# This script estimates three things from counts of a population over time:
#     r       the intrinsic rate of increase
#     lambda  the finite rate of increase
#     K       the carrying capacity
#
# It does not care what organism you counted. Duckweed, Daphnia, yeast -
# if your data has the three columns below, this script works.
#
#     Treatment       a label for each experimental group (text)
#     Day             days since the start of the experiment (number)
#     PopulationSize  how many individuals you counted (number)
#
# ============================================================================

library(tidyverse)

# ---- 1. Read your data -----------------------------------------------------
# Put this script and your datasheet in the SAME folder, then use
# Session > Set Working Directory > To Source File Location in RStudio.
# After that, the line below just works - no file paths to type.

data <- read.csv("population_growth_datasheet.csv")

# Drop any rows you haven't filled in yet
data <- data %>% filter(!is.na(PopulationSize))

if (nrow(data) == 0) {
  stop("No data to analyse. The PopulationSize column in your datasheet is ",
       "still empty - fill in your counts before running this script.",
       call. = FALSE)
}

# Always look at your data before you analyse it
head(data)
table(data$Treatment)

# ---- 2. Estimate r ---------------------------------------------------------
# Population growth is exponential, so log(N) plotted against time is a
# straight line, and the SLOPE of that line is r.

data$log_PopulationSize <- log(data$PopulationSize)

treatments <- unique(data$Treatment)   # whatever you named your groups

for (t in treatments) {
  group <- filter(data, Treatment == t)
  model <- lm(log_PopulationSize ~ Day, data = group)

  cat("\n---", t, "---\n")
  cat("r (slope):           ", coef(model)[2], "\n")
  cat("N at day 0 (intercept):", exp(coef(model)[1]), "\n")
}

# Plot log(N) against time, one panel per treatment.
# The fitted line is straight on purpose - a straight line has one slope,
# and that slope is your estimate of r.

ggplot(data, aes(x = Day, y = log_PopulationSize)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~ Treatment) +
  xlab("Time (days)") +
  ylab("log population size") +
  theme_minimal()

# ---- 3. Estimate lambda ----------------------------------------------------
# lambda is the factor a population multiplies by per unit time.
# lambda > 1 means growing, lambda < 1 means shrinking, lambda = 1 means stable.

calculate_lambda <- function(initial_pop, final_pop, total_time) {
  (final_pop / initial_pop)^(1 / total_time)
}

for (t in treatments) {
  group <- filter(data, Treatment == t) %>% arrange(Day)

  first_day <- first(group$Day)
  last_day  <- last(group$Day)

  lambda <- calculate_lambda(
    initial_pop = first(group$PopulationSize),
    final_pop   = last(group$PopulationSize),
    total_time  = last_day - first_day
  )

  cat("lambda for", t, ":", lambda, "\n")
}

# ---- 4. Estimate K ---------------------------------------------------------
# As a population fills its container, growth slows. So lambda should DROP as
# N rises. Plot lambda against N, fit a line, and read K off where lambda = 1
# (the population is no longer growing - that is carrying capacity).

lambda_by_step <- data %>%
  group_by(Treatment) %>%
  arrange(Day, .by_group = TRUE) %>%
  mutate(lambda = lead(PopulationSize) / PopulationSize) %>%
  filter(!is.na(lambda)) %>%
  ungroup()

ggplot(lambda_by_step, aes(x = PopulationSize, y = lambda)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("Population size (N)") +
  ylab("Geometric growth rate (lambda)") +
  theme_minimal()

lambda_model <- lm(lambda ~ PopulationSize, data = lambda_by_step)
summary(lambda_model)

intercept <- coef(lambda_model)[1]
slope     <- coef(lambda_model)[2]

cat("\nEstimated K (where lambda = 1):", (1 - intercept) / slope, "\n")

# How many points went into that estimate?
cat("K was estimated from", nrow(lambda_by_step), "points.\n")
cat("Fewer than about 6 and you should treat K as a rough guess.\n")

# Did your container reach its estimated carrying capacity?
