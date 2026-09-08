# ---------------------------------------------------------------------------
# ENV 226 Lab -- Identifying trees: dichotomous keys and the campus tree walk
#
# What this script does:
#   1. Reads your tree walk datasheet
#   2. Summarises how many trees you found of each species
#   3. Compares the native and planted tree communities on your route
#   4. Checks how your stated confidence lined up with being right
#
# HOW TO USE THIS SCRIPT
#   - Put tree_walk_datasheet.csv in the same folder as this script
#   - In RStudio: Session > Set Working Directory > To Source File Location
#   - Then run the script one line at a time (Ctrl/Cmd + Enter)
# ---------------------------------------------------------------------------

# install.packages("tidyverse")   # run once, only if you don't have it
library(tidyverse)

# --- 1. Read your data -----------------------------------------------------

trees <- read.csv("tree_walk_datasheet.csv", stringsAsFactors = FALSE)

# ALWAYS look at your data before you analyse it.
head(trees)
str(trees)

# How many trees did you record?
nrow(trees)

# --- 2. What did you find? -------------------------------------------------

species_counts <- trees |>
  count(Species, Origin, name = "n") |>
  arrange(desc(n))

species_counts

# Richness: how many DIFFERENT species (unknowns count as their own)
n_distinct(trees$Species)

# --- 3. Native vs planted --------------------------------------------------

origin_summary <- trees |>
  group_by(Origin) |>
  summarise(
    n_trees    = n(),
    n_species  = n_distinct(Species),
    mean_dbh   = mean(DBH_cm, na.rm = TRUE),
    sd_dbh     = sd(DBH_cm, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  mutate(pct_of_trees = 100 * n_trees / sum(n_trees))

origin_summary

# Figure 1 -- how many trees of each origin
# (No title on the figure itself: the title belongs in your caption.)
fig1 <- ggplot(origin_summary, aes(x = Origin, y = n_trees, fill = Origin)) +
  geom_col(width = 0.6) +
  labs(x = NULL, y = "Number of trees recorded") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

fig1

# Figure 2 -- size distribution, native vs planted
fig2 <- ggplot(trees, aes(x = DBH_cm, fill = Origin)) +
  geom_histogram(binwidth = 10, colour = "white", position = "identity",
                 alpha = 0.65) +
  labs(x = "Diameter at breast height (cm)", y = "Number of trees",
       fill = NULL) +
  theme_minimal(base_size = 13)

fig2

# Save a figure for your write-up
# ggsave("tree_walk_origin.png", fig1, width = 5, height = 4, dpi = 300)

# --- 4. How good were your identifications? --------------------------------
#
# Add a column called Correct to your datasheet after your TA checks you,
# with values "Yes" or "No". Then run the block below.

if ("Correct" %in% names(trees)) {

  accuracy <- trees |>
    filter(!is.na(Correct), Correct != "") |>
    group_by(Confidence) |>
    summarise(
      n         = n(),
      n_correct = sum(Correct == "Yes"),
      pct_correct = 100 * n_correct / n,
      .groups   = "drop"
    )

  print(accuracy)

  # The interesting question is not whether you were right.
  # It is whether you KNEW when you were right.
  # A well-calibrated observer is much more correct on their "High" calls
  # than on their "Low" ones. If your High and Low rows look the same,
  # your confidence is not carrying information -- and that is worth
  # a sentence in your write-up.

} else {
  message("No 'Correct' column yet -- add it after your TA checks your IDs.")
}

# --- 5. Unknowns -----------------------------------------------------------

unknowns <- trees |> filter(grepl("^UNK", Species))
nrow(unknowns)
unknowns

# An unknown that you recorded consistently is data.
# An unknown you guessed at is not. Keep your UNK codes.

# --- 6. Is your unknown actually resolvable? -------------------------------
#
# The test that matters: if you handed your log to a botanist you have never
# met, could they identify this plant? This block checks whether the fields
# that make that possible are filled in.
#
# It cannot tell you whether your notes are GOOD. It can only tell you
# whether they EXIST. That is still worth knowing before you walk away from
# the tree.

log_path <- "unknown_plant_log.csv"

if (file.exists(log_path)) {

  unk <- read.csv(log_path, stringsAsFactors = FALSE)

  # Fields a stranger needs. Determiner/DeterminedDate are deliberately
  # excluded -- those get filled in later, by whoever identifies it.
  required <- c("UnknownCode", "Date", "Collector", "LocationNotes",
                "GrowthHabit", "LeafArrangement", "LeafType",
                "BarkNotes", "OtherCharacters", "PhotoNumbers")

  missing_cols <- setdiff(required, names(unk))
  if (length(missing_cols) > 0) {
    warning("Your log is missing these columns entirely: ",
            paste(missing_cols, collapse = ", "))
    required <- intersect(required, names(unk))
  }

  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

  completeness <- unk |>
    rowwise() |>
    mutate(
      n_missing = sum(blank(c_across(all_of(required)))),
      missing_fields = paste(required[blank(c_across(all_of(required)))],
                             collapse = ", ")
    ) |>
    ungroup() |>
    select(UnknownCode, n_missing, missing_fields)

  print(completeness, n = Inf)

  cat("\n")
  n_complete <- sum(completeness$n_missing == 0)
  cat(n_complete, "of", nrow(completeness),
      "unknowns have every field filled in.\n\n")

  if (n_complete < nrow(completeness)) {
    cat("The rows above with n_missing > 0 are the ones a botanist could not\n")
    cat("work from. Go back and fill them in NOW, while you can still find\n")
    cat("the tree -- not next week.\n\n")
  }

  # Did anyone collect a specimen or post to iNaturalist?
  if ("SpecimenCollected" %in% names(unk)) {
    cat("Specimens collected:",
        sum(grepl("^Y", unk$SpecimenCollected, ignore.case = TRUE)),
        "of", nrow(unk), "\n")
  }
  if ("iNatURL" %in% names(unk)) {
    cat("Posted to iNaturalist:", sum(!blank(unk$iNatURL)),
        "of", nrow(unk), "\n")
  }

} else {
  message("No unknown_plant_log.csv found. If every tree keyed out, say so ",
          "in your write-up -- but check the Wommack route again first.")
}
