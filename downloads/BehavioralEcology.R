# ============================================================================
# ENV 226 Lab - Behavioral ecology
#
# Turns interval-by-interval behavior codes into one number per animal, then
# tests whether that number depends on distance to cover or group size.
#
#     Observer        initials of the person who recorded this row
#     Site            where you watched
#     Species         what you watched
#     FocalID         one label per animal watched (A1, A2, ...)
#     GroupSize       individuals of the same species within 10 m, incl. focal
#     DistToCover_m   meters to the nearest thing it could hide in or under
#     Interval        1 to 30 - which beep this row is
#     Behavior        one ethogram code: FOR VIG LOC REST SOC OTH
#
# Expected output for the example datasheet shipped with the manual
# (14 focal Abert's squirrels, one of them double-observed):
#     slope of proportion vigilant on distance = 0.036 per meter
#     p = 0.0025, R-squared = 0.55
#     observer agreement on focal A5 = 83.3%
# ============================================================================

library(tidyverse)

ETHOGRAM <- c("FOR", "VIG", "LOC", "REST", "SOC", "OTH")

# ---- 1. Read your data -----------------------------------------------------
# Session > Set Working Directory > To Source File Location

if (!file.exists("behavior_datasheet.csv")) {
  stop("Can't find behavior_datasheet.csv in this folder.\n",
       "  1. Session > Set Working Directory > To Source File Location\n",
       "  2. Check the filename matches exactly",
       call. = FALSE)
}

data <- read.csv("behavior_datasheet.csv", stringsAsFactors = FALSE)
str(data)


# ---- 2. Check every code against the ethogram ------------------------------
# A stray "vig", or "FOR " with a trailing space, is a different string to R
# and would quietly become its own behavior category. Catch it here.

data$Behavior <- trimws(data$Behavior)
unknown <- setdiff(unique(data$Behavior), ETHOGRAM)

if (length(unknown) > 0) {
  cat("Codes in your data that are NOT in the ethogram:\n")
  print(table(data$Behavior[data$Behavior %in% unknown]))
  stop("Fix these in the spreadsheet and re-export. Do not fix them here - ",
       "the spreadsheet is your record of what you saw.", call. = FALSE)
}

cat("\nAll codes valid. Overall tally:\n")
print(table(data$Behavior))


# ---- 3. One number per animal ----------------------------------------------
# IMPORTANT: your sample size is the number of ANIMALS, not the number of
# intervals. Thirty intervals from one squirrel are thirty looks at the same
# squirrel, not thirty independent data points. Collapsing to one proportion
# per focal is what keeps this honest. The mistake has a name:
# pseudoreplication.
#
# We use the FIRST observer per focal so that a double-observed animal is not
# counted twice.

primary <- data %>%
  group_by(FocalID) %>%
  filter(Observer == first(Observer)) %>%
  ungroup()

per_animal <- primary %>%
  group_by(FocalID, Species, GroupSize, DistToCover_m) %>%
  summarise(
    n_intervals = n(),
    prop_vigilant = mean(Behavior == "VIG"),
    prop_foraging = mean(Behavior == "FOR"),
    prop_outofsight = mean(Behavior == "OTH"),
    .groups = "drop"
  )

print(as.data.frame(per_animal))

cat("\nSample size for the analysis:", nrow(per_animal), "animals\n")
if (nrow(per_animal) < 8) {
  message("With fewer than ~8 animals you are unlikely to detect anything. ",
          "Pool with the rest of your section.")
}

# A focal where a lot of time was OTH is a weak focal - flag it, don't hide it.
weak <- per_animal %>% filter(prop_outofsight > 0.25)
if (nrow(weak) > 0) {
  cat("\nFocals with >25% of intervals out of sight (interpret with care):\n")
  print(as.data.frame(weak[, c("FocalID", "prop_outofsight")]))
}


# ---- 4. Question A: does vigilance increase with distance to cover? --------

ggplot(per_animal, aes(x = DistToCover_m, y = prop_vigilant)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  xlab("Distance to nearest cover (m)") +
  ylab("Proportion of intervals vigilant") +
  theme_minimal()

# No title on the figure - the title belongs in your legend.

cat("\n--- Vigilance vs distance to cover ---\n")
print(summary(lm(prop_vigilant ~ DistToCover_m, data = per_animal)))


# ---- 5. Question B: does vigilance fall as group size rises? ---------------
# Only meaningful if your animals actually varied in group size.

if (length(unique(per_animal$GroupSize)) > 2) {
  ggplot(per_animal, aes(x = GroupSize, y = prop_vigilant)) +
    geom_point(size = 2.5) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
    xlab("Group size (individuals within 10 m)") +
    ylab("Proportion of intervals vigilant") +
    theme_minimal()

  cat("\n--- Vigilance vs group size ---\n")
  print(summary(lm(prop_vigilant ~ GroupSize, data = per_animal)))
} else {
  cat("\nGroup size barely varied in your data, so there is nothing to regress\n",
      "against. That is a sampling-design result, not a null result.\n", sep = "")
}


# ---- 6. How much did two observers disagree? -------------------------------
# For any focal watched by two people, what fraction of intervals match?

doubled <- data %>%
  group_by(FocalID) %>%
  filter(n_distinct(Observer) == 2) %>%
  ungroup()

if (nrow(doubled) == 0) {
  cat("\nNo focal was recorded by two observers, so agreement cannot be\n",
      "calculated. Do this next time - it is the only check you have on\n",
      "whether your ethogram means the same thing to two people.\n", sep = "")
} else {
  for (fid in unique(doubled$FocalID)) {
    pair <- doubled %>%
      filter(FocalID == fid) %>%
      select(Observer, Interval, Behavior) %>%
      pivot_wider(names_from = Observer, values_from = Behavior)

    obs <- setdiff(names(pair), "Interval")
    a <- pair[[obs[1]]]
    b <- pair[[obs[2]]]
    ok <- !is.na(a) & !is.na(b)

    agreement <- mean(a[ok] == b[ok]) * 100
    cat(sprintf("\n--- Focal %s: %s vs %s ---\n", fid, obs[1], obs[2]))
    cat(sprintf("Agreement: %.1f%% of %d intervals\n", agreement, sum(ok)))

    if (agreement < 80) {
      cat("Below 80%. That is a signal your ethogram needs tightening,\n",
          "not that somebody was careless.\n", sep = "")
    }

    mismatch <- pair[ok, ][a[ok] != b[ok], ]
    if (nrow(mismatch) > 0) {
      cat("Where you disagreed:\n")
      print(table(paste(a[ok][a[ok] != b[ok]], "vs", b[ok][a[ok] != b[ok]])))
      cat("The pair that shows up most is the definition to sharpen.\n")
    }
  }
}
