# ============================================================================
# ENV 226 Lab - Mark-recapture
#
# Estimates population size from marked and recaptured counts.
#
#     Trial      a number or label for each round
#     Treatment  Clean, UnequalCatch, Closure, MarkLoss, or Field
#     n1         how many you marked and released
#     n2         how many you caught the second time
#     m2         how many of the second catch were already marked
#     TrueN      the real population size - only for the bag. Leave BLANK
#                for field data. Nobody knows the true N of a real population,
#                which is the entire reason this method exists.
#
# Expected output for the example datasheet shipped with the manual
# (true N = 400 for the Clean trials):
#     Clean Chapman estimates run 360 - 434, mean 394, and all five
#     confidence intervals contain 400.
#     UnequalCatch  374  (true 500) - too LOW, as predicted
#     Closure       541  (true 400) - too HIGH, as predicted
#     MarkLoss      471  (true 400) - too HIGH, as predicted
# ============================================================================

library(tidyverse)

# ---- 1. Read your data -----------------------------------------------------
# Session > Set Working Directory > To Source File Location

if (!file.exists("mark_recapture_datasheet.csv")) {
  stop("Can't find mark_recapture_datasheet.csv in this folder.\n",
       "  1. Session > Set Working Directory > To Source File Location\n",
       "  2. Check the filename matches exactly",
       call. = FALSE)
}

data <- read.csv("mark_recapture_datasheet.csv")
data <- data %>% filter(!is.na(m2))

if (nrow(data) == 0) {
  stop("No data to analyse - the m2 column is empty. Fill in your counts first.",
       call. = FALSE)
}

str(data)

# ---- 2. A sanity check you should never skip -------------------------------
# You cannot recapture more marked animals than you marked, and you cannot
# recapture more than you caught. If this stops you, it is a recording error,
# not a biological result.

bad <- data %>% filter(m2 > n1 | m2 > n2)
if (nrow(bad) > 0) {
  print(bad)
  stop("The rows above are impossible: m2 cannot exceed n1 or n2.", call. = FALSE)
}

if (any(data$m2 == 0)) {
  message("NOTE: at least one trial has m2 = 0. Lincoln-Petersen will return Inf ",
          "for those rows. That is the method telling you a sample with no ",
          "recaptures carries no information about population size.")
}

# ---- 3. The two estimators -------------------------------------------------
#   Lincoln-Petersen   N = (n1 * n2) / m2
#   Chapman            N = ((n1+1)(n2+1) / (m2+1)) - 1     <- report this one
#
# Chapman is less biased with small samples and does not blow up when m2 = 0.

data <- data %>%
  mutate(
    N_petersen = n1 * n2 / m2,
    N_chapman  = ((n1 + 1) * (n2 + 1) / (m2 + 1)) - 1,

    # Seber's variance for the Chapman estimator.
    # Notice m2 appears in every term - small m2, huge interval.
    var_chapman = ((n1 + 1) * (n2 + 1) * (n1 - m2) * (n2 - m2)) /
                  (((m2 + 1)^2) * (m2 + 2)),
    se_chapman  = sqrt(var_chapman),
    lower_95    = N_chapman - 1.96 * se_chapman,
    upper_95    = N_chapman + 1.96 * se_chapman
  )

data %>%
  select(Trial, Treatment, n1, n2, m2, N_petersen, N_chapman, lower_95, upper_95) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  print()


# ---- 4. How much did five identical trials disagree? -----------------------
# Same bag, same method, same true N. This spread is what a SINGLE estimate
# is hiding from you.

clean <- data %>% filter(Treatment == "Clean")

if (nrow(clean) > 0) {
  cat("\n--- Clean trials ---\n")
  cat("n trials:      ", nrow(clean), "\n")
  cat("mean estimate: ", round(mean(clean$N_chapman), 1), "\n")
  cat("range:         ", round(min(clean$N_chapman), 1), "to",
                         round(max(clean$N_chapman), 1), "\n")
  if (!all(is.na(clean$TrueN))) {
    truth <- unique(clean$TrueN[!is.na(clean$TrueN)])[1]
    cat("true N:        ", truth, "\n")
    covered <- sum(clean$lower_95 <= truth & clean$upper_95 >= truth)
    cat("CIs containing the true value:", covered, "of", nrow(clean), "\n")
  }
}


# ---- 5. Plot the estimates against the truth -------------------------------

truth <- if (all(is.na(data$TrueN))) NA else unique(data$TrueN[!is.na(data$TrueN)])[1]

p <- ggplot(data, aes(x = factor(Trial), y = N_chapman, colour = Treatment)) +
  geom_pointrange(aes(ymin = lower_95, ymax = upper_95)) +
  xlab("Trial") +
  ylab("Estimated population size (Chapman, 95% CI)") +
  theme_minimal()

if (!is.na(truth)) {
  p <- p + geom_hline(yintercept = truth, linetype = "dashed")
}
print(p)

# No title on the plot - the title belongs in your figure legend.


# ---- 6. Did the broken assumptions move the estimate as predicted? ---------
# Compare each broken trial against the SPREAD of the clean trials, not
# against a single clean trial.

if (nrow(clean) > 0) {
  cat("\n--- Broken-assumption trials vs the clean range ---\n")
  cat("Clean range:", round(min(clean$N_chapman)), "to",
                      round(max(clean$N_chapman)), "\n\n")
  broken <- data %>% filter(Treatment != "Clean")
  for (i in seq_len(nrow(broken))) {
    r <- broken[i, ]
    where <- if (r$N_chapman > max(clean$N_chapman)) "ABOVE the clean range"
             else if (r$N_chapman < min(clean$N_chapman)) "BELOW the clean range"
             else "inside the clean range"
    cat(sprintf("%-14s estimate %6.1f  (true %s) - %s\n",
                r$Treatment, r$N_chapman,
                ifelse(is.na(r$TrueN), "unknown", r$TrueN), where))
  }
  cat("\nA trial landing INSIDE the clean range does not mean the assumption\n",
      "did not matter. Bias and precision are different things: one noisy\n",
      "trial often cannot detect a bias that would still be there every time\n",
      "you ran the study. That is why you ran five clean trials.\n", sep = "")
}
