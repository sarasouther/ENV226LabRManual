# ---------------------------------------------------------------------------
# ENV 226 Lab -- Urban ecology: the crosstown walk
#
# This script is deliberately GENERIC. Different groups measured different
# things, so the datasheet is in long format: one row per measurement, with
# a Variable column saying what was measured.
#
# That means this script works whether you counted birds, measured trees,
# classified land cover, or ran a line intercept down a sidewalk.
#
# HOW TO USE THIS SCRIPT
#   - Put urban_walk_datasheet.csv in the same folder as this script
#   - In RStudio: Session > Set Working Directory > To Source File Location
#   - Run one line at a time (Ctrl/Cmd + Enter)
# ---------------------------------------------------------------------------

library(tidyverse)

# --- 1. Read the data ------------------------------------------------------

walk <- read.csv("urban_walk_datasheet.csv", stringsAsFactors = FALSE)

head(walk)

# What did the class actually measure?
walk |> count(Variable, Unit, name = "n_records")

# What segments did we cover?
walk |> count(Segment, name = "n_records")

# Put the segments in WALKING ORDER, not alphabetical order.
# CHANGE THIS to match your route.
segment_order <- c("Southside", "Downtown", "Campus Edge")

walk <- walk |>
  mutate(Segment = factor(Segment, levels = segment_order))

# --- 2. Pick your variable -------------------------------------------------
# Change this to whatever your group measured.

my_variable <- "CanopyCover"

my_data <- walk |> filter(Variable == my_variable)

nrow(my_data)

if (nrow(my_data) == 0) {
  stop("No rows for '", my_variable, "'. Check the spelling against the ",
       "Variable column above.")
}

# --- 3. Summarise by segment ----------------------------------------------

seg_summary <- my_data |>
  group_by(Segment) |>
  summarise(
    n      = n(),
    mean   = mean(Value, na.rm = TRUE),
    sd     = sd(Value, na.rm = TRUE),
    se     = sd / sqrt(n),
    .groups = "drop"
  )

seg_summary

# NOTE ON n: with only two or three blocks per segment, your standard error
# is barely meaningful. Say so in your write-up rather than reporting it as
# if it were solid. Small n is a real limitation of a one-afternoon study,
# and naming it is better science than hiding it.

# --- 4. Figures ------------------------------------------------------------
# (No titles on the figures: the title belongs in your caption.)

unit_label <- unique(my_data$Unit)[1]
y_label <- paste0(my_variable, " (", unit_label, ")")

# Figure 1 -- mean by segment, with error bars
fig1 <- ggplot(seg_summary, aes(x = Segment, y = mean)) +
  geom_col(fill = "grey35", width = 0.6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.15) +
  labs(x = NULL, y = y_label) +
  theme_minimal(base_size = 13)

fig1

# Figure 2 -- block by block along the transect.
# This is usually the more honest figure: it shows the gradient rather than
# collapsing it into three bars, and it shows how variable blocks are within
# a segment.
fig2 <- ggplot(my_data, aes(x = Block, y = Value, colour = Segment)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1), colour = "grey60", linewidth = 0.4) +
  labs(x = "Block number along transect", y = y_label, colour = NULL) +
  theme_minimal(base_size = 13)

fig2

# ggsave("urban_transect.png", fig2, width = 6, height = 4, dpi = 300)

# --- 5. Is the difference real? -------------------------------------------
# With three or more segments and a continuous response, a one-way ANOVA is
# the natural test -- the same logic as the "selecting statistical tests"
# chapter. With only two segments, use a t-test.

if (nlevels(droplevels(my_data$Segment)) >= 3) {
  fit <- aov(Value ~ Segment, data = my_data)
  summary(fit)
  # TukeyHSD(fit)     # which segments differ from which
} else {
  t.test(Value ~ Segment, data = my_data)
}

# BEFORE you report a p-value, ask whether your sampling design supports it.
# Blocks within a segment are not independent of each other in the way the
# test assumes -- adjacent blocks share the same street, the same planting
# history, the same storm drain. That does not make the comparison useless,
# but it does mean the p-value is optimistic. Say so.

# --- 6. Compare across groups ---------------------------------------------
# Did groups using DIFFERENT methods find the same pattern?
# Convergence from independent methods is much stronger evidence than any
# single method on its own.

class_wide <- walk |>
  group_by(Segment, Variable) |>
  summarise(mean = mean(Value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Variable, values_from = mean)

class_wide

# Figure 3 -- every variable, standardised so they can share an axis
fig3 <- walk |>
  group_by(Variable) |>
  mutate(z = as.numeric(scale(Value))) |>
  ungroup() |>
  group_by(Segment, Variable) |>
  summarise(mean_z = mean(z, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = Segment, y = mean_z, group = Variable, colour = Variable)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  labs(x = NULL, y = "Standardised value (z-score)", colour = NULL) +
  theme_minimal(base_size = 13)

fig3

# Lines that move together are variables telling the same story.
# Lines that cross are worth a paragraph.
