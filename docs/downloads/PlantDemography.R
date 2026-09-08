# ---------------------------------------------------------------------------
# ENV 226 Lab -- Plant population biology: matrix models and viability
#
# What this script does:
#   1. Reads your census data and shows the size distribution
#   2. Assigns plants to stage classes
#   3. Builds a stage transition matrix from two years of data
#   4. Calculates lambda, the stable stage distribution, and elasticity
#   5. Runs a stochastic projection and estimates quasi-extinction risk
#   6. Compares management scenarios
#
# BUILD THE MATRIX BY HAND FIRST. Then run this and check that they agree.
# If they don't, find out why before you go on -- it is almost always the
# deaths row or the fecundity row.
#
# HOW TO USE THIS SCRIPT
#   - Put plant_demography_datasheet.csv in the same folder as this script
#   - In RStudio: Session > Set Working Directory > To Source File Location
#   - Run one line at a time (Ctrl/Cmd + Enter)
# ---------------------------------------------------------------------------

# install.packages(c("popbio", "gplots", "tidyverse"))   # run once
# ---------------------------------------------------------------------------
# MISSING PACKAGE CHECK - stops with a useful message instead of a bare error
for (.p in c("popbio", "gplots")) {
  if (!requireNamespace(.p, quietly = TRUE)) {
    stop("The package '", .p, "' is not installed.\n",
         "  Run this once in the Console, then re-run this script:\n",
         "      install.packages(c('popbio', 'gplots'))", call. = FALSE)
  }
}
# ---------------------------------------------------------------------------

library(popbio)
library(tidyverse)

# --- 1. Read your census ---------------------------------------------------

census <- read.csv("plant_demography_datasheet.csv", stringsAsFactors = FALSE)

head(census)
nrow(census)

# Figure 1 -- size distribution.
# LOOK AT THIS BEFORE YOU SET STAGE BOUNDARIES. The shape of the histogram
# is what tells you where the natural breaks are.
# (No title on the figure: put it in the caption.)
fig1 <- ggplot(census, aes(x = Size)) +
  geom_histogram(binwidth = 2, fill = "grey35", colour = "white") +
  labs(x = "Plant size", y = "Number of individuals") +
  theme_minimal(base_size = 13)

fig1

# --- 2. Assign stage classes ----------------------------------------------
# CHANGE THESE CUTOFFS. They are a real methodological choice, not a given.
# Set them from your histogram, then come back later and move them to see
# how much your answer depends on where you drew the lines.

recruit_max  <- 3      # below this = recruit
juvenile_max <- 8      # below this = juvenile

assign_stage <- function(size, reproductive) {
  dplyr::case_when(
    is.na(size)                 ~ NA_character_,
    size <  recruit_max         ~ "recruit",
    size <  juvenile_max        ~ "juvenile",
    reproductive == "Yes"       ~ "flowering",
    TRUE                        ~ "nonflowering"
  )
}

stage_order <- c("recruit", "juvenile", "nonflowering", "flowering")

census <- census |>
  mutate(Stage = factor(assign_stage(Size, Reproductive), levels = stage_order))

census |> count(Year, Stage)

# --- 3. Build the transition matrix ---------------------------------------
#
# You need TWO years. If you have only one, your instructor will give you a
# simulated year 2 (or you are the first year of a class time series and
# next year's students will build the matrix from your tags).
#
# Reshape to one row per plant with its stage in each of two years:

years <- sort(unique(census$Year))

if (length(years) >= 2) {

  y1 <- years[1]; y2 <- years[2]

  paired <- census |>
    filter(Year %in% c(y1, y2)) |>
    select(PlantID, Year, Stage) |>
    pivot_wider(names_from = Year, values_from = Stage,
                names_prefix = "stage_")

  names(paired)[2:3] <- c("stage_t", "stage_t1")

  # Survival transitions: FROM stage_t (column) TO stage_t1 (row)
  trans <- paired |>
    filter(!is.na(stage_t)) |>
    count(stage_t, stage_t1, .drop = FALSE)

  # Column totals INCLUDE the plants that died (stage_t1 is NA)
  col_totals <- paired |>
    filter(!is.na(stage_t)) |>
    count(stage_t, name = "n_start", .drop = FALSE)

  A <- matrix(0, nrow = 4, ncol = 4,
              dimnames = list(stage_order, stage_order))

  for (i in seq_len(nrow(trans))) {
    from <- as.character(trans$stage_t[i])
    to   <- as.character(trans$stage_t1[i])
    if (is.na(to) || to == "NA") next          # this plant died
    denom <- col_totals$n_start[col_totals$stage_t == from]
    if (length(denom) == 1 && denom > 0) A[to, from] <- trans$n[i] / denom
  }

  # --- Fecundity (top row) -------------------------------------------------
  # New recruits in year 2 divided by flowering adults in year 1.
  # This is a COUNT per individual, not a probability. It can exceed 1.

  n_recruits_t1  <- sum(census$Year == y2 & census$Stage == "recruit",
                        na.rm = TRUE)
  n_flowering_t  <- sum(census$Year == y1 & census$Stage == "flowering",
                        na.rm = TRUE)

  if (n_flowering_t > 0) {
    A["recruit", "flowering"] <- n_recruits_t1 / n_flowering_t
  }

  round(A, 3)

  # SANITY CHECK: every column of survival transitions (rows 1-4, EXCLUDING
  # the fecundity contribution) must sum to at most 1. If one exceeds 1,
  # you divided by the wrong denominator.
  colSums(A)

} else {

  message("Only one year of data found. Using a worked example matrix so you ",
          "can run the rest of the script.")

  # A plausible declining perennial, for practice only.
  # lambda = 0.94 -- a slowly declining population, which is the case worth
  # practising on. Elasticity is highest on adult stasis (0.20) rather than
  # fecundity (0.11), which is the result the chapter asks you to explain.
  A <- matrix(c(
    0.20, 0.00, 0.00, 2.15,   # -> recruit  (top row: fecundity in last column)
    0.32, 0.40, 0.05, 0.00,   # -> juvenile
    0.00, 0.27, 0.50, 0.22,   # -> nonflowering
    0.00, 0.02, 0.22, 0.54    # -> flowering
  ), nrow = 4, byrow = TRUE,
  dimnames = list(stage_order, stage_order))
}

A

# --- 4. Lambda, stable stage, elasticity ----------------------------------

lam <- lambda(A)
lam
# lambda > 1 : growing. lambda < 1 : declining. lambda = 1 : holding steady.

stable.stage(A)
# The proportions the population converges to if this matrix stays fixed.
# This is the "stable stage distribution" assumption you were asked to
# defend in the population structure lab -- here is where it comes from.

elas <- elasticity(A)
round(elas, 3)

# Which single transition matters most?
which(elas == max(elas), arr.ind = TRUE)

# Figure 2 -- elasticity, as a heat map
elas_df <- as.data.frame(as.table(elas))
names(elas_df) <- c("To", "From", "Elasticity")

fig2 <- ggplot(elas_df, aes(x = From, y = To, fill = Elasticity)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(Elasticity, 2)), size = 3.4) +
  scale_fill_gradient(low = "white", high = "steelblue4") +
  labs(x = "From stage", y = "To stage") +
  theme_minimal(base_size = 13)

fig2

# --- 5. Project the population --------------------------------------------

n0 <- c(recruit = 30, juvenile = 25, nonflowering = 20, flowering = 15)

proj <- pop.projection(A, n0, iterations = 50)
proj$lambda
proj$stage.vectors[, 1:5]

proj_df <- tibble(Year = 0:49, N = proj$pop.sizes)

fig3 <- ggplot(proj_df, aes(x = Year, y = N)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Years from now", y = "Total population size") +
  theme_minimal(base_size = 13)

fig3

# --- 6. Stochastic projection and quasi-extinction ------------------------
#
# One matrix is one year. Real vital rates vary. Here each year's matrix is
# drawn with noise, and we run the projection many times.
#
# KEY POINT: a population with mean lambda ABOVE 1 can still go extinct.
# Variability alone can carry a small population to zero.

set.seed(226)                       # so your results are reproducible

quasi_threshold <- 10               # "functionally gone" -- set by your class
n_years <- 50
n_runs  <- 500
cv      <- 0.25                     # how variable the vital rates are

simulate_one <- function(A, n0, n_years, cv, threshold) {
  n <- n0
  sizes <- numeric(n_years)
  for (t in seq_len(n_years)) {
    A_t <- A * matrix(rlnorm(length(A), meanlog = 0, sdlog = cv),
                      nrow = nrow(A))
    # survival transitions can't exceed 1
    A_t[-1, ] <- pmin(A_t[-1, ], 1)
    n <- as.vector(A_t %*% n)
    sizes[t] <- sum(n)
  }
  list(sizes = sizes, quasi_extinct = any(sizes < threshold))
}

runs <- replicate(n_runs,
                  simulate_one(A, n0, n_years, cv, quasi_threshold),
                  simplify = FALSE)

p_quasi <- mean(vapply(runs, function(x) x$quasi_extinct, logical(1)))

cat("Probability of falling below", quasi_threshold, "individuals within",
    n_years, "years:", round(100 * p_quasi, 1), "%\n")

# Figure 4 -- the spray of trajectories.
# The average is the least interesting thing here. Look at the spread.
traj <- map_dfr(seq_along(runs), function(i) {
  tibble(run = i, Year = 1:n_years, N = runs[[i]]$sizes)
})

fig4 <- ggplot(traj |> filter(run <= 100),
               aes(x = Year, y = N, group = run)) +
  geom_line(alpha = 0.12, linewidth = 0.4) +
  geom_hline(yintercept = quasi_threshold, linetype = "dashed",
             colour = "firebrick") +
  scale_y_log10() +
  labs(x = "Years from now", y = "Total population size (log scale)") +
  theme_minimal(base_size = 13)

fig4

# --- 7. Management scenarios ----------------------------------------------
# Each scenario is just a modified matrix. Compare lambda AND extinction
# risk -- they do not always rank scenarios the same way.

## Scenario 1: harvest 20% of flowering adults every year
A_harvest <- A
A_harvest[, "flowering"] <- A_harvest[, "flowering"] * 0.80

## Scenario 2: 10% improvement in adult survival (e.g. fencing)
A_protect <- A
A_protect["flowering", "flowering"]       <- min(A["flowering", "flowering"] * 1.10, 1)
A_protect["nonflowering", "nonflowering"] <- min(A["nonflowering", "nonflowering"] * 1.10, 1)

## Scenario 3: 10% more seed production
A_seed <- A
A_seed["recruit", "flowering"] <- A["recruit", "flowering"] * 1.10

scenarios <- tibble(
  Scenario = c("Baseline", "Harvest 20% of adults",
               "Adult survival +10%", "Seed production +10%"),
  Lambda   = c(lambda(A), lambda(A_harvest),
               lambda(A_protect), lambda(A_seed))
) |>
  mutate(Change_vs_baseline = round(Lambda - lambda(A), 4),
         Lambda = round(Lambda, 4))

scenarios

# Compare the ranking here to your elasticity matrix. The scenario that
# helps most SHOULD be the one acting on the highest-elasticity transition.
# When it isn't, that is worth a sentence in your write-up.
