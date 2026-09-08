# ---------------------------------------------------------------------------
# ENV 226 Lab -- Field methods: running a monitoring plot
#
# Line-point intercept (LPI) analysis, following the AIM Monitoring Manual
# (Herrick et al., USDA-ARS Jornada Experimental Range).
#
# What this script does:
#   1. Reads your LPI datasheet
#   2. Calculates foliar cover, bare ground, basal cover, species composition
#   3. Reads your observer calibration sheet and checks the 10-point standard
#
# HOW TO USE THIS SCRIPT
#   - Put lpi_datasheet.csv and calibration_datasheet.csv in the same folder
#   - In RStudio: Session > Set Working Directory > To Source File Location
#   - Run one line at a time (Ctrl/Cmd + Enter)
# ---------------------------------------------------------------------------

library(tidyverse)

# --- 0. The non-plant codes ------------------------------------------------
# These are the codes that are NOT species. Anything not in this list, and
# not blank, is treated as a plant species code.

nonplant_codes <- c("N",                                   # nothing hit
                    "HL", "WL", "NL", "VL", "DS",          # litter, etc.
                    "S", "LC", "CY", "M", "D",             # soil surface
                    "R", "GR", "CB", "ST", "BY", "BR", "W")

# --- 1. Read your data -----------------------------------------------------

lpi <- read.csv("lpi_datasheet.csv", stringsAsFactors = FALSE)

head(lpi)
str(lpi)

# How many pin drops did you record?
n_points <- nrow(lpi)
n_points

# Sanity check: you should have 50 points per transect.
lpi |> count(Transect, name = "points")

# --- 2. Tidy it ------------------------------------------------------------
# Reshape to one row per (point, layer) so every hit is its own row.
# This is what makes all four calculations below simple.

hits <- lpi |>
  pivot_longer(
    cols      = c(TopLayer, Lower1, Lower2, SoilSurface),
    names_to  = "Layer",
    values_to = "Code"
  ) |>
  mutate(Code = na_if(trimws(Code), "")) |>
  filter(!is.na(Code)) |>
  mutate(is_plant = !(Code %in% nonplant_codes))

head(hits)

# --- 3. Cover calculations -------------------------------------------------

## % foliar cover -- points where the TOP layer is a plant
foliar_points <- lpi |>
  filter(!(trimws(TopLayer) %in% nonplant_codes), trimws(TopLayer) != "") |>
  nrow()

pct_foliar <- 100 * foliar_points / n_points

## % bare ground -- top layer N, nothing in the lower layers, soil surface S
bare_points <- lpi |>
  filter(
    trimws(TopLayer) == "N",
    trimws(Lower1) == "" | is.na(Lower1),
    trimws(Lower2) == "" | is.na(Lower2),
    trimws(SoilSurface) == "S"
  ) |>
  nrow()

pct_bare <- 100 * bare_points / n_points

## % basal cover -- points with a SPECIES code in the soil surface column
basal_points <- lpi |>
  filter(!(trimws(SoilSurface) %in% nonplant_codes),
         trimws(SoilSurface) != "") |>
  nrow()

pct_basal <- 100 * basal_points / n_points

cover_summary <- tibble(
  Indicator = c("Foliar cover", "Bare ground", "Basal cover"),
  Percent   = round(c(pct_foliar, pct_bare, pct_basal), 1)
)

cover_summary

# NOTE: these three do NOT add to 100. They count different events over the
# same set of points. If yours sum to 130, nothing is wrong.

## Species composition -- of the points with rooted vegetation, what
## proportion does each species occur at (in ANY layer)?

points_with_veg <- hits |>
  filter(is_plant) |>
  distinct(PointID) |>
  nrow()

composition <- hits |>
  filter(is_plant) |>
  distinct(PointID, Code) |>          # each species once per point
  count(Code, name = "n_points") |>
  mutate(pct_composition = round(100 * n_points / points_with_veg, 1)) |>
  arrange(desc(pct_composition))

composition

# Figure 1 -- species composition
# (No title on the figure: put it in the caption.)
fig1 <- composition |>
  slice_max(pct_composition, n = 10) |>
  ggplot(aes(x = reorder(Code, pct_composition), y = pct_composition)) +
  geom_col(fill = "grey35", width = 0.7) +
  coord_flip() +
  labs(x = "Species code", y = "Percent composition") +
  theme_minimal(base_size = 13)

fig1

# --- 4. Observer calibration -----------------------------------------------
#
# The AIM standard for LPI: all observers must be within 10 PERCENTAGE POINTS
# (absolute) of one another, for each indicator.

cal <- read.csv("calibration_datasheet.csv", stringsAsFactors = FALSE)

cal_check <- cal |>
  group_by(Indicator) |>
  summarise(
    n_observers = n(),
    min_pct     = min(Value_pct),
    max_pct     = max(Value_pct),
    mean_pct    = round(mean(Value_pct), 1),
    spread      = max(Value_pct) - min(Value_pct),
    .groups     = "drop"
  ) |>
  mutate(passes_10pt = ifelse(spread <= 10, "PASS", "FAIL"))

cal_check

# Figure 2 -- who said what
fig2 <- ggplot(cal, aes(x = Indicator, y = Value_pct)) +
  geom_point(aes(colour = Observer), size = 3, alpha = 0.8,
             position = position_jitter(width = 0.08, height = 0)) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4,
               colour = "grey30", linewidth = 0.4) +
  labs(x = NULL, y = "Percent cover", colour = "Observer") +
  theme_minimal(base_size = 13)

fig2

# If an indicator FAILS, the useful question is WHICH observer is the
# outlier and WHY. Look at the point that is furthest from the group mean:

cal |>
  group_by(Indicator) |>
  mutate(deviation = Value_pct - mean(Value_pct)) |>
  arrange(desc(abs(deviation))) |>
  ungroup() |>
  head(5)

# Remember: the fix for observer error is a clearer protocol, not more effort.
