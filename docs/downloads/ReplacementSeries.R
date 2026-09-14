# ============================================================================
# ENV 226 Lab - Population ecology, Track C
# Two species in one cup: a de Wit replacement series
#
# Reads the SAME three-column datasheet as PopulationGrowth.R. Species and
# treatment are encoded together in the Treatment column, like this:
#
#     Lemna_Mono     species alone, full density
#     Lemna_Mix      species in the 50/50 mixture
#     Azolla_Mix     the other species in the same mixture
#     Azolla_Mono    the other species alone, full density
#
# The rule is <Species>_Mono and <Species>_Mix. Anything else is ignored.
#
# Expected output for the example datasheet shipped with the manual:
#     RY Lemna  = 0.36     RY Azolla = 0.64     RYT = 1.00
#     -> the two species are competing for the same limiting resource,
#        and Azolla is the stronger competitor for it.
# ============================================================================

library(tidyverse)

if (!file.exists("replacement_series_datasheet.csv")) {
  stop("Can't find replacement_series_datasheet.csv in this folder.\n",
       "  1. Session > Set Working Directory > To Source File Location\n",
       "  2. Check the filename matches exactly",
       call. = FALSE)
}

data <- read.csv("replacement_series_datasheet.csv", stringsAsFactors = FALSE)
data <- data %>% filter(!is.na(PopulationSize))

if (nrow(data) == 0) {
  stop("No data yet - PopulationSize is empty.", call. = FALSE)
}

# ---- 1. Split the Treatment label into species and stand -------------------

data <- data %>%
  separate(Treatment, into = c("Species", "Stand"), sep = "_",
           remove = FALSE, fill = "right")

bad <- data %>% filter(is.na(Stand) | !Stand %in% c("Mono", "Mix"))
if (nrow(bad) > 0) {
  print(unique(bad$Treatment))
  stop("The Treatment labels above are not in <Species>_Mono / <Species>_Mix ",
       "form. Fix them in the spreadsheet.", call. = FALSE)
}

cat("Species found:", paste(unique(data$Species), collapse = ", "), "\n")

# ---- 2. Growth curves, both species, both stands ---------------------------

ggplot(data, aes(x = Day, y = PopulationSize,
                 colour = Species, linetype = Stand)) +
  geom_line() +
  geom_point(size = 2) +
  xlab("Time (days)") +
  ylab("Cover (grid points occupied)") +
  theme_minimal()

# No title on the figure - the title belongs in your legend.

# ---- 3. Relative yield ------------------------------------------------------
# RY = final cover in the mixture / final cover in monoculture.
# You planted each species at HALF its monoculture density, so RY = 0.5 is
# the no-interaction expectation. Above 0.5 it beat its share; below, it lost.

final <- data %>%
  group_by(Species, Stand) %>%
  filter(Day == max(Day)) %>%
  summarise(final_cover = mean(PopulationSize), .groups = "drop") %>%
  pivot_wider(names_from = Stand, values_from = final_cover)

if (!all(c("Mono", "Mix") %in% names(final)) || any(is.na(final$Mono))) {
  stop("Every species needs BOTH a _Mono and a _Mix row. Without the ",
       "monoculture there is nothing to compare the mixture against.",
       call. = FALSE)
}

final <- final %>% mutate(RY = Mix / Mono)

cat("\n--- Relative yields ---\n")
print(as.data.frame(final %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))

RYT <- sum(final$RY)
cat(sprintf("\nRYT (relative yield total) = %.2f\n", RYT))

if (RYT > 1.15) {
  cat("RYT well above 1: the species are using partly DIFFERENT resources,\n",
      "or one is facilitating the other. They are not purely in each other's way.\n", sep = "")
} else if (RYT < 0.85) {
  cat("RYT well below 1: mutual interference. Each does worse together than\n",
      "simple resource accounting predicts.\n", sep = "")
} else {
  cat("RYT near 1: the species are competing for the SAME limiting resource.\n",
      "One's gain is the other's loss.\n", sep = "")
}

winner <- final$Species[which.max(final$RY)]
loser  <- final$Species[which.min(final$RY)]
if (max(final$RY) - min(final$RY) > 0.1) {
  cat(sprintf("\n%s outcompeted %s (RY %.2f vs %.2f).\n",
              winner, loser, max(final$RY), min(final$RY)))
  cat("Before you explain WHY, check whether the two species differ in\n",
      "something other than competitive ability - Azolla fixes its own\n",
      "nitrogen, which is a different resource, not a better grip on the\n",
      "same one.\n", sep = "")
} else {
  cat("\nNeither species clearly outcompeted the other.\n")
}
