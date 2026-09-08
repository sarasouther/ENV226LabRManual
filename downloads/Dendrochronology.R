# ---------------------------------------------------------------------------
# ENV 226 Lab -- Dendrochronology: reading time in wood
#
# TWO TRACKS. Run the one your instructor assigned.
#   Track A -- your own ring width measurements from a physical cross-section
#   Track B -- a published Arizona ponderosa chronology from the ITRDB
#
# HOW TO USE THIS SCRIPT
#   - Put ring_width_datasheet.csv in the same folder as this script
#   - In RStudio: Session > Set Working Directory > To Source File Location
#   - Run one line at a time (Ctrl/Cmd + Enter)
# ---------------------------------------------------------------------------

# install.packages(c("dplR", "tidyverse"))   # run once
# ---------------------------------------------------------------------------
# MISSING PACKAGE CHECK - stops with a useful message instead of a bare error
for (.p in c("dplR")) {
  if (!requireNamespace(.p, quietly = TRUE)) {
    stop("The package '", .p, "' is not installed.\n",
         "  Run this once in the Console, then re-run this script:\n",
         "      install.packages('dplR')", call. = FALSE)
  }
}
# ---------------------------------------------------------------------------

library(dplR)
library(tidyverse)

# ===========================================================================
# TRACK A -- your own measurements
# ===========================================================================

rings <- read.csv("ring_width_datasheet.csv", stringsAsFactors = FALSE)

head(rings)

# Figure 1 -- your raw ring width series
# (No title on the figure: put it in the caption.)
figA1 <- ggplot(rings, aes(x = Year, y = RingWidth_mm, colour = SampleID)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  labs(x = "Year", y = "Ring width (mm)", colour = "Sample") +
  theme_minimal(base_size = 13)

figA1

# Do your samples go up and down TOGETHER?
# That shared year-to-year pattern is what crossdating uses. If two samples
# from the same stand move in opposite directions, at least one is misdated.

rings_wide <- rings |>
  pivot_wider(names_from = SampleID, values_from = RingWidth_mm) |>
  arrange(Year)

rings_wide

# Correlation between your samples (drop the Year column first)
cor(rings_wide[, -1], use = "pairwise.complete.obs")

# A strong positive correlation means the samples are telling the same story.
# A weak or negative one is a signal to go back to the wood.

# --- Which years were unusually narrow? ------------------------------------
# Express each ring as a proportion of that sample's own mean, so that a big
# fast-growing tree and a small slow one can be compared on the same scale.

rings_rel <- rings |>
  group_by(SampleID) |>
  mutate(rel_width = RingWidth_mm / mean(RingWidth_mm, na.rm = TRUE)) |>
  ungroup()

narrow_years <- rings_rel |>
  group_by(Year) |>
  summarise(mean_rel = mean(rel_width), n = n(), .groups = "drop") |>
  arrange(mean_rel)

narrow_years

# The years at the top of that list are your candidate marker years.


# ===========================================================================
# TRACK B -- an ITRDB chronology
# ===========================================================================
#
# AZ617 is the G.A. Pearson Natural Area, Fort Valley -- about 10 km from
# campus, ponderosa pine, spanning 1767-2016.
#
# Download the .rwl file to this folder from:
#   https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/northamerica/usa/az617.rwl
#
# Then uncomment the line below.

# az617 <- read.rwl("az617.rwl")

# If you don't have the download yet, dplR ships several ITRDB collections
# so you can run everything now. co021 is Mesa Verde, Colorado.
data(co021)
rwl <- co021          # <- swap this for az617 once you have the file

# --- 1. What have we got? --------------------------------------------------

rwl.report(rwl)                       # number of series, span, missing rings
plot(rwl, plot.type = "spag")         # every series, stacked

# --- 2. Check the dating ---------------------------------------------------
# Each series is correlated against a master built from ALL THE OTHERS,
# in overlapping windows.
#   BLUE  = correlates above threshold, dating looks fine
#   RED   = fails the threshold, possible dating error
#   GREEN = not enough overlap to judge

crossdate <- corr.rwl.seg(rwl, seg.length = 50, pcrit = 0.05)

# Overall agreement among series:
interseries.cor(rwl)

# --- 3. Detrend ------------------------------------------------------------
# Remove the age-related decline in ring width so what's left is the
# year-to-year variation. Each series is DIVIDED by a fitted curve, giving a
# dimensionless index centred on 1.

rwi <- detrend(rwl, method = "Spline")

# --- 4. Build the chronology -----------------------------------------------

crn <- chron(rwi)

plot(crn, add.spline = TRUE, nyrs = 20)

# --- 5. Find your pointer years --------------------------------------------
# A pointer year: at least `nseries.thresh` percent of series show more than
# `rgv.thresh` percent relative growth change, in the SAME direction.

pt <- pointer(rwi)                       # defaults: 75% of series, 10% change
head(pt)

# Negative pointer years = years nearly every tree grew poorly.
neg_pointers <- pt |>
  as.data.frame() |>
  filter(Nature == -1) |>
  arrange(Year)

head(neg_pointers, 20)

# Now try tightening and loosening the thresholds. The years that survive
# EVERY setting are your robust marker years -- those are the ones to compare
# against documented Southwest droughts.

pt_strict <- pointer(rwi, rgv.thresh = 20, nseries.thresh = 85)
pt_loose  <- pointer(rwi, rgv.thresh = 5,  nseries.thresh = 60)

# Which years appear under all three?
strict_yrs <- pt_strict$Year[pt_strict$Nature == -1]
loose_yrs  <- pt_loose$Year[pt_loose$Nature  == -1]
default_yrs <- pt$Year[pt$Nature == -1]

robust <- Reduce(intersect, list(default_yrs, strict_yrs, loose_yrs))
robust

# --- 6. Compare to documented drought --------------------------------------
# Fill in your own comparison. Some periods to check against:
#   1950s  -- the mid-century Southwest drought
#   2002   -- regional die-off drought (Breshears et al. 2005)
#   2000-2004 -- lowest 5-year Colorado River flow since 1906
#   late 1500s -- the driest reconstructed period in the last 1200 years
#
# Where your pointer years match, you have independent confirmation.
# Where they don't, you have something to explain -- and a local cause
# (insects, fire, thinning) is as real an answer as regional climate.
