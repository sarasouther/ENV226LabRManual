# ============================================================================
# ENV 226 Lab - Litter decomposition
#
# Turns before-and-after litterbag masses into a decomposition rate
# constant k, and compares k between the levels of your chosen factor.
#
#     BagID          one label per bag
#     Factor         LitterType, Microhabitat, or MeshSize
#     Level          the specific level of that factor for this bag
#     Site           where it was deployed
#     Replicate      1-6 within a level
#     InitialMass_g  oven-dry-corrected starting mass
#     FinalMass_g    oven-dry mass on retrieval
#     Days           days in the field
#
# Expected output for the example datasheet shipped with the manual
# (24 bags, 70 days, north campus):
#     PonderosaNeedle  96.1% remaining   k = 0.00058 /day  (0.21 /yr)
#     GambelOakLeaf    92.4% remaining   k = 0.00113 /day  (0.41 /yr)
#     TeaRooibos       87.6% remaining   k = 0.00189 /day
#     TeaGreen         50.1% remaining   k = 0.00996 /day
#     Ponderosa vs oak: p = 0.0005
# ============================================================================

library(tidyverse)

# ---- 1. Read your data -----------------------------------------------------
# Session > Set Working Directory > To Source File Location

if (!file.exists("litter_decomposition_datasheet.csv")) {
  stop("Can't find litter_decomposition_datasheet.csv in this folder.\n",
       "  1. Session > Set Working Directory > To Source File Location\n",
       "  2. Check the filename matches exactly",
       call. = FALSE)
}

data <- read.csv("litter_decomposition_datasheet.csv", stringsAsFactors = FALSE)
data <- data %>% filter(!is.na(FinalMass_g))

if (nrow(data) == 0) {
  stop("No data to analyse - FinalMass_g is empty. Weigh your bags first.",
       call. = FALSE)
}

str(data)


# ---- 2. Checks worth running before you calculate anything -----------------

gained <- data %>% filter(FinalMass_g > InitialMass_g)
if (nrow(gained) > 0) {
  cat("\nThese bags came back HEAVIER than they went out:\n")
  print(gained[, c("BagID", "Level", "InitialMass_g", "FinalMass_g")])
  cat("Litter does not gain mass. This is soil and roots that did not get\n",
      "cleaned off, or a missing air-dry correction. k cannot be calculated\n",
      "for these rows - they are dropped below, and you should say so in\n",
      "your methods rather than quietly ignoring them.\n", sep = "")
  data <- data %>% filter(FinalMass_g <= InitialMass_g)
}

cat("\nBags per level:\n")
print(table(data$Level))
if (any(table(data$Level) < 4)) {
  message("A level with fewer than 4 surviving bags will not support a ",
          "comparison. Report how many you lost and why.")
}


# ---- 3. Percent remaining and the decay constant ---------------------------
#   Mt/M0 = exp(-k*t)    so    k = -ln(Mt/M0) / t
# Reported per day here; multiply by 365 for the per-year figure the
# literature uses.

data <- data %>%
  mutate(
    prop_remaining = FinalMass_g / InitialMass_g,
    pct_remaining  = 100 * prop_remaining,
    pct_lost       = 100 - pct_remaining,
    k_day          = -log(prop_remaining) / Days,
    k_year         = k_day * 365
  )

summary_table <- data %>%
  group_by(Level) %>%
  summarise(
    n             = n(),
    # NOTE: each of these gets a NEW name. If you write
    #     pct_lost = mean(pct_lost),  sd_pct_lost = sd(pct_lost)
    # the second line sees the mean you just created, not the original
    # column, and silently returns NA. dplyr evaluates these in order.
    mean_pct_remaining = mean(pct_remaining),
    mean_pct_lost      = mean(pct_lost),
    sd_pct_lost        = sd(pct_lost),
    mean_k_day         = mean(k_day),
    mean_k_year        = mean(k_year),
    .groups = "drop"
  )

print(as.data.frame(summary_table %>%
  mutate(across(where(is.numeric), ~ signif(.x, 3)))))


# ---- 4. Did the tea actually decompose? ------------------------------------
# The positive control. If green tea barely lost mass, decomposition did not
# happen much at your site over your interval, and a null result for your
# litter tells you nothing about the litter.

tea <- data %>% filter(grepl("^Tea", Level))
if (nrow(tea) > 0) {
  green <- tea %>% filter(grepl("Green", Level))
  if (nrow(green) > 0) {
    gl <- mean(green$pct_lost)
    cat(sprintf("\n--- Positive control ---\nGreen tea lost %.1f%% of its mass in %d days.\n",
                gl, round(mean(green$Days))))
    if (gl < 20) {
      cat("That is LOW. Decomposition barely happened at your site over this\n",
          "interval. A null result for your litter is therefore uninformative\n",
          "about the litter - report it as a result about the site.\n", sep = "")
    } else {
      cat("Decomposition clearly happened at this site. So if your litter did\n",
          "not lose much mass, that is a real property of the litter.\n", sep = "")
    }
  }
} else {
  cat("\nNo tea bags in your data, so you have no positive control. With a\n",
      "small expected effect size that makes a null result hard to interpret.\n", sep = "")
}


# ---- 5. Plot -----------------------------------------------------------------
# Individual bags as points so the within-level spread is visible. If that
# spread is as wide as the gap between levels, you have not shown a difference.

ggplot(data, aes(x = Level, y = k_day)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, size = 2, alpha = 0.7) +
  xlab("") +
  ylab(expression(paste("Decomposition rate constant ", italic(k), " (per day)"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# No title on the figure - the title belongs in your legend.


# ---- 6. Is the difference between your levels real? ------------------------
# Your own factor only - the tea is a control, not a treatment.

own <- data %>% filter(!grepl("^Tea", Level))
lv  <- unique(own$Level)

if (length(lv) == 2) {
  cat("\n--- Comparing", lv[1], "and", lv[2], "---\n")
  print(t.test(k_day ~ Level, data = own))
} else if (length(lv) > 2) {
  cat("\n--- Comparing", length(lv), "levels ---\n")
  print(anova(lm(k_day ~ Level, data = own)))
} else {
  cat("\nOnly one non-tea level in your data, so there is nothing to compare.\n")
}
