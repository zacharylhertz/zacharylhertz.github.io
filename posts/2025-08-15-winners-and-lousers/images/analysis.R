# ==============================================================================
# Analysis of 2022-2024 election results and demographics in CA-11
# Author: Zachary Lorico Hertz
# Last updated: 1 August 2025
# ==============================================================================

# Load Required Libraries --------------------------------------------------
library(sf)           # Spatial data handling
library(tidyverse)    # Data manipulation and visualization
library(data.table)   # Fast data reading
library(tigris)       # Census geography data
library(tidycensus)   # Census data access
library(leaflet)      # Interactive maps
library(leafpop)      # Leaflet popups
library(htmltools)    # HTML tools for leaflet
library(broom)        # Broom to handle NA value in model fitting

# Data Loading and Processing ===============================================

# Load Census Data ----------------------------------------------------------
census <- fread("data/census2020_blck.csv") |>
  # Filter for San Francisco County, California
  filter(STUSAB == "CA" & COUNTY == "San Francisco County") |>
  # Calculate demographic percentages and diversity index
  mutate(
    white.pct = U7L003/U7L001,
    black.pct = U7L004/U7L001,
    asian.pct = U7L006/U7L001,
    hisp.pct = U7L010/U7L001,
    other.pct = (U7L005 + U7L007 + U7L009)/U7L001,
    # Blau Index: measure of racial diversity 
    #(0 = homogeneous, 1 = maximally diverse)
    blau_index = ifelse(U7L001 > 0,
                        1 - (white.pct^2 + black.pct^2 +
                               asian.pct^2 + hisp.pct^2 + other.pct^2),
                        NA),
    # Create tract-block identifier for merging
    TRACTBLOCK = paste0(TRACTA, BLOCKA)
  )

# Load Precinct Shapefiles --------------------------------------------------
precincts <- st_read("shapefiles/mprec_075_g22_v01.shp", quiet = TRUE) |>
  # Rename for easier merging
  mutate(srprec = PRECINCT)

# Load 2022 Election Data ---------------------------------------------------
results_data <- read.csv("data/SF_2022_srprec.csv") |>
  mutate(
    # Calculate total congressional votes (all candidates)
    total_cng_votes_22 = CNGDEM01 + CNGDEM02 + CNGGRN01 + CNGREP01,
    # Calculate two-way vote totals (Democrat + Republican only)
    two_way_votes_22 = CNGDEM01 + CNGREP01,
    # Calculate Pelosi vote share
    pelosi.twv_22 = CNGDEM01/two_way_votes_22,        # Two-way vote share
    pelosi.pct_22 = CNGDEM01/total_cng_votes_22,      # Raw percentage
    # Calculate Republican vote share
    republican.twv_22 = CNGREP01/two_way_votes_22,
    republican.pct_22 = CNGREP01/total_cng_votes_22
  )

# Load 2024 Election Data ---------------------------------------------------
results_data24 <- read.csv("data/SF_2024_srprec.csv") |>
  # Convert vote columns to numeric (handle any formatting issues)
  mutate(
    CNGDEM01 = as.numeric(CNGDEM01),
    CNGDEM02 = as.numeric(CNGDEM02),
    CNGIND01 = as.numeric(CNGIND01),
    CNGREP01 = as.numeric(CNGREP01),
    CNGREP02 = as.numeric(CNGREP02),
    PRSDEM01 = as.numeric(PRSDEM01),
    PRSGRN01 = as.numeric(PRSGRN01),
    PRSLIB01 = as.numeric(PRSLIB01),
    PRSPAF01 = as.numeric(PRSPAF01),
    PRSREP01 = as.numeric(PRSREP01),
    
    # Congressional race calculations
    total_cng_votes_24 = CNGDEM01 + CNGDEM02 + CNGIND01 + CNGREP01 + CNGREP02,
    two_way_votes_24 = CNGDEM01 + CNGREP01,
    pelosi.twv_24 = CNGDEM01/two_way_votes_24,
    pelosi.pct_24 = CNGDEM01/total_cng_votes_24,
    bruce.twv = CNGREP01/two_way_votes_24,
    bruce.pct_24 = CNGREP01/total_cng_votes_24,
    republican.twv_24 = CNGREP01/two_way_votes_24,
    republican.pct_24 = CNGREP01/total_cng_votes_24,
    
    # Bruce Lou performance metrics
    bruce_net = CNGREP01 - CNGDEM01,
    bruce_netpct = bruce.pct_24 - 0.18965,  # Compared to citywide result
    
    # Presidential race calculations
    total_pres_votes_24 = PRSDEM01 + PRSGRN01 + PRSLIB01 + PRSPAF01 + PRSREP01,
    two_way_pres_votes_24 = PRSDEM01 + PRSREP01,
    harris.twv_24 = PRSDEM01/two_way_pres_votes_24,
    harris.pct_24 = PRSDEM01/total_pres_votes_24,
    trump.twv_24 = PRSREP01/two_way_pres_votes_24,
    trump.pct_24 = PRSREP01/total_pres_votes_24,
    
    # Compare Bruce Lou vs Trump performance
    bruce_trump_pct_diff = bruce.pct_24 - trump.pct_24,
    bruce_trump_twv_diff = bruce.twv - trump.twv_24,
    bruce_trump_raw_diff = CNGREP01 - PRSREP01
  )

# Load Block-to-Precinct Crosswalk ------------------------------------------
crosswalk <- read.csv("data/c075_g22_sr_blk_map.csv") |>
  mutate(TRACTBLOCK = paste0(tract, block))

# Geographic Data Processing ===============================================

# Combine Election Results with Precinct Geography -------------------------
precincts_with_data <- precincts |>
  # Join 2022 results
  left_join(results_data, by = "srprec", suffix = c("", "_2022")) |>
  # Join 2024 results
  left_join(results_data24, by = "srprec", suffix = c("", "_2024")) |>
  # Calculate shifts between elections  
  mutate(
    pelosi.twv_shift = pelosi.twv_24 - pelosi.twv_22,
    pelosi.pct_shift = pelosi.pct_24 - pelosi.pct_22,
    r.pct_shift = republican.pct_24 - republican.pct_22,
    r.twv_shift = republican.twv_24 - republican.twv_22
  )

# Filter to CA-11 District Boundaries ---------------------------------------
# Remove water areas and crop to San Francisco proper
sf_only <- precincts_with_data |>
  filter(srprec != "water") |>
  st_crop(xmin = -122.55, xmax = -122.35,  # SF longitude bounds
          ymin = 37.70, ymax = 37.85)       # SF latitude bounds

# Define precincts outside CD-11 boundaries
# Here, I just manually looked at the map provided by SF Board of Elections
# and deleted the ones that the map indicated are outside CD-11.
# Note that for some reason the map codes are not the same as the 
# codes used for precincts in the data, so this required a fair bit of trial
# and error. 
outside_precincts <- c(
  9731, 1106, 1105, 1104, 1109, 1111, 1107, 1112, 1113, 1114,
  1115, 1117, 1118, 1119, 1121, 1145, 1122, 1124, 1146, 1144,
  1123, 1125, 1126, 1128, 1127, 1141, 1147, 1148, 1142, 1129,
  1136, 1137, 1133, 1131, 1129, 1132, 1134, 1138, 1143, 1149,
  1135, 1139, 7942, 7943, 7944, 7945, 9902, 7947, 7948, 7949,
  7946, 7039, 7041, 7042, 7043, 7045, 7046, 7044, 7049, 7048,
  7047, 7052)

# Filter to CD-11 precincts only
sf_only_ca_11 <- sf_only |>
  filter(!srprec %in% outside_precincts)

crosswalk_ca_11 <- crosswalk |>
  filter(!srprec %in% outside_precincts)

# Demographic Data Processing ===============================================

# Distribute Census Block Data to Precincts --------------------------------
# I broke this into steps to make it easier for others (and future me) to follow.

# Step 1: Merge census data with block-to-precinct conversion
census_with_conversion <- merge(census, crosswalk_ca_11,
                                by.x = "TRACTBLOCK", by.y = "TRACTBLOCK")

# Step 2: Distribute census counts proportionally using pctblk field
census_distributed <- census_with_conversion |>
  mutate(
    # Distribute population counts based on block percentage in each precinct
    U7L001_distributed = U7L001 * pctblk,  # Total population
    U7L003_distributed = U7L003 * pctblk,  # White
    U7L004_distributed = U7L004 * pctblk,  # Black
    U7L005_distributed = U7L005 * pctblk,  # American Indian/Alaska Native
    U7L006_distributed = U7L006 * pctblk,  # Asian
    U7L007_distributed = U7L007 * pctblk,  # Native Hawaiian/Pacific Islander
    U7L008_distributed = U7L008 * pctblk,  # Other race
    U7L009_distributed = U7L009 * pctblk,  # Two or more races
    U7L010_distributed = U7L010 * pctblk   # Hispanic/Latino
  )

# Step 3: Aggregate to precinct level
census_by_srprec <- census_distributed |>
  group_by(srprec) |>
  summarise(
    total_pop = sum(U7L001_distributed, na.rm = TRUE),
    white_pop = sum(U7L003_distributed, na.rm = TRUE),
    black_pop = sum(U7L004_distributed, na.rm = TRUE),
    aian_pop = sum(U7L005_distributed, na.rm = TRUE),
    asian_pop = sum(U7L006_distributed, na.rm = TRUE),
    nhpi_pop = sum(U7L007_distributed, na.rm = TRUE),
    other_pop = sum(U7L008_distributed, na.rm = TRUE),
    multirace_pop = sum(U7L009_distributed, na.rm = TRUE),
    hisp_pop = sum(U7L010_distributed, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  # Recalculate percentages at precinct level
  mutate(
    white_pct = white_pop / total_pop,
    black_pct = black_pop / total_pop,
    asian_pct = asian_pop / total_pop,
    hisp_pct = hisp_pop / total_pop,
    other_pct = (other_pop + multirace_pop + nhpi_pop + aian_pop) / total_pop,
    # Recalculate Blau diversity index
    blau_index = ifelse(total_pop > 0,
                        1 - (white_pct^2 + black_pct^2 +
                               asian_pct^2 + hisp_pct^2 + other_pct^2),
                        NA)
  )

# Create Final Integrated Dataset -------------------------------------------
final_data <- merge(sf_only_ca_11, census_by_srprec,
                    by.x = "srprec", by.y = "srprec")

# Quick residual analysis
m <- lm(bruce.pct_24 ~ trump.pct_24 + pelosi.pct_22 + asian_pct, data=final_data)
m2 <- lm(bruce.pct_24 ~ asian_pct, data=final_data)
m3 <- lm(bruce.pct_24 ~ pelosi.pct_22, data=final_data)
m4 <- lm(bruce.pct_24 ~ trump.pct_24, data=final_data)
m5 <- lm(bruce.pct_24 ~ asian_pct + pelosi.pct_22, data=final_data)

final_data <- final_data |> 
  mutate(pred.full = predict(m, final_data),
         resid.full = bruce.pct_24 - pred.full,
         pred.asian = predict(m2, final_data),
         resid.asian = bruce.pct_24 - pred.asian,
         pred.hist = predict(m3, final_data),
         resid.hist = bruce.pct_24 - pred.hist,
         pred.trump = predict(m4, final_data),
         resid.trump = bruce.pct_24 - pred.trump)

# Visualization and Analysis ===============================================

# Calculate Precinct Centroids for Labeling --------------------------------
precinct_centers <- st_centroid(sf_only_ca_11, of_largest_polygon = TRUE) |>
  st_crop(xmin = -122.55, xmax = -122.35,
          ymin = 37.70, ymax = 37.85)

# Electoral Performance Maps ------------------------------------------------

# Plot 1: 2022 Pelosi Performance
p1 <- ggplot(final_data) +
  geom_sf(aes(fill = pelosi.twv_22)) +
  scale_fill_gradient2(
    low = "#C20114",      # Red for low Democratic performance
    mid = "#EAF2EF",      # Light gray for middle
    high = "#3266E9",     # Blue for high Democratic performance
    midpoint = 0.8395,    # 2022 average
    name = "Pelosi 2022\n(Two-Way Vote Share)"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Pelosi Performance 2022")

# Plot 2: 2024 Pelosi Performance
p2 <- ggplot(final_data) +
  geom_sf(aes(fill = pelosi.twv_24)) +
  scale_fill_gradient2(
    low = "#C20114",
    mid = "#EAF2EF",
    high = "#3266E9",
    midpoint = .8103,     # 2024 average
    name = "Pelosi 2024\n(Two-Way Vote Share)"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "2024 General Election in CA-11\nNancy Pelosi (D) 81.0% - 19.0% Bruce Lou (R)",
    caption = "Map: Zachary L. Hertz\nData: Statewide Database.\nResults normalized relative to citywide average."
  )

# Demographic Maps ----------------------------------------------------------

# Plot 3: Asian American Population Distribution
p3 <- ggplot(final_data) +
  geom_sf(aes(fill = asian_pct)) +
  scale_fill_gradient(
    low = "white",
    high = "#5941A9",
    name = "Asian\nPercentage",
    labels = scales::percent
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Asian American Population by Precinct")

# Plot 4: White Population Distribution
p4 <- ggplot(final_data) +
  geom_sf(aes(fill = white_pct)) +
  scale_fill_gradient(
    low = "black",
    high = "white",
    name = "White\nPercentage",
    labels = scales::percent
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "White Population by Precinct")

# Plot 5: Racial Diversity Index
p5 <- ggplot(final_data) +
  geom_sf(aes(fill = blau_index)) +
  scale_fill_gradient2(
    low = "darkblue",    # Low diversity
    mid = "white",        # Medium diversity
    high = "orange",       # High diversity
    midpoint = .6143,     # Median diversity
    name = "Diversity\nIndex"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Racial Diversity by Precinct (Blau Index)")

# Plot 6: Total Population Density
p6 <- ggplot(final_data) +
  geom_sf(aes(fill = total_pop)) +
  scale_fill_gradient(
    low = "white",
    high = "black",
    name = "Total\nPopulation"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Total Population by Precinct")

# Plot 7: Bruce Lou vs Trump Performance Comparison
p7 <- ggplot(final_data) +
  geom_sf(aes(fill = bruce_trump_raw_diff)) +
  scale_fill_gradient2(
    low = "#340068",      # Purple for Trump outperformance
    mid = "white",        # White for equal performance
    high = "#FF9F1C",     # Orange for Lou outperformance
    midpoint = 0,
    name = "Lou Performance\nOver Trump"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Lou % - Trump %")

# Scatter Plot Analysis -----------------------------------------------------

# Plot 8: Relationship between Asian population and Republican overperformance
p8 <- ggplot(final_data, aes(x = asian_pct, y = bruce_trump_pct_diff)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = lm, se = TRUE) +
  labs(
    x = "Asian American Population (%)",
    y = "Lou % - Trump %",
    title = "Relationship between Asian Population and Lou Overperformance"
  ) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal()

# Plot 9: Residuals analysis with 2022 vote share
p9 <- ggplot(final_data, aes(x = pelosi.twv_22, y = resid.asian)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  theme_minimal()

# Plot 10: Lou performance relative to citywide and Asian population
p10 <- ggplot(final_data, aes(x = asian_pct, y = bruce_netpct)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(
    x = "Asian American Population (%)",
    y = "Precinct-level Lou Vote Share - Citywide Lou Vote Share",
    caption = "Plot: Zachary L. Hertz\nData: Statewide Database, IPUMS",
    title = "Lou performance relative to citywide results and Asian American population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Plot 11: Normalized shift map
p11 <- ggplot(final_data) +
  geom_sf(aes(fill = pelosi.twv_shift)) +
  scale_fill_gradient2(
    low = "#C20114",      # Red if shift from Pelosi > citywide
    mid = "#EAF2EF",      # Light gray for middle
    high = "#3266E9",     # Blue if shift from Pelosi < citywide
    midpoint = -0.0292,    # Citywide shift
    name = "Normalized \n2022-2024 shift"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "CA-11 Shift 2022-2024 normalized to citywide shift \nCitywide 2022-2024 shift: -2.9%")

# Plot 12: 2022-2024 shift and Asian population
p12 <- ggplot(final_data, aes(x = asian_pct, y = pelosi.twv_shift)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(
    x = "Asian American Population (%)",
    y = "Precinct-level 2022-2024 Shift",
    caption = "Plot: Zachary L. Hertz\nData: Statewide Database, IPUMS",
    title = "CA-11 precinct-level 2022-2024 shift relative to citywide results and Asian American population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Plot 13: Headline 2022-2024 shift
p13 <- ggplot(final_data) +
  geom_sf(aes(fill = pelosi.twv_shift)) +
  scale_fill_gradient2(
    low = "#C20114",      # Red if shift to Republicans
    mid = "#EAF2EF",      # Light gray for middle
    high = "#3266E9",     # Blue if shift to Pelosi
    midpoint = 0,    # Set middle to 0
    name = "2022-2024 shift\n(Two-Way Vote)"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "2024 to 2022 shift in CA-11\n2024 results: Pelosi 81.0%, 2022 results: Pelosi 84.0%")

# Plot 14: Full model residuals
p14 <- ggplot(final_data) +
  geom_sf(aes(fill = resid.full)) +
  scale_fill_gradient2(
    high = "#C20114",      # Red if shift to Republicans
    mid = "#EAF2EF",      # Light gray for middle
    low = "#3266E9",     # Blue if shift to Pelosi
    midpoint = 0,    # Set middle to 0
    name = "Residuals"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Plot 15: Residuals from Asian model vs 2022 results
p15 <- ggplot(final_data, aes(x = pelosi.twv_22, y = resid.asian)) +
  geom_point() +
  geom_smooth(method=lm, se = FALSE) +
  labs(
    x = "Pelosi 2022 Vote Share",
    y = "Lou Overperformance Relative to Asian Population",
    caption = "Plot: Zachary L. Hertz\nData: Statewide Database, IPUMS",
    title = "Prediction error from a model using only Asian population is correlated with 2022 results"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Plot 16: Residuals from historical model vs Asian population
p16 <- ggplot(final_data, aes(x = asian_pct, y = resid.hist)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(
    x = "Population Percent Asian American",
    y = "Lou Overperformance Relative to 2022 CA-11 results",
    caption = "Plot: Zachary L. Hertz\nData: Statewide Database, IPUMS",
    title = "Prediction error from a model using only 2022 results is not strongly correlated with Asian population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Plot 17: Residuals from Trump model vs Asian population
p17 <- ggplot(final_data, aes(x = asian_pct, y = resid.trump)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(
    x = "Population Percent Asian American",
    y = "Lou Overperformance Relative to Trump Vote Share",
    caption = "Plot: Zachary L. Hertz\nData: Statewide Database, IPUMS",
    title = "Prediction error from a model using only 2024 presidential \nresults is not correlated with Asian population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Plot 18: Residuals from Trump model vs diversity index
p18 <- ggplot(final_data, aes(x = blau_index, y = resid.trump)) +
  geom_point() +
  geom_smooth(method=loess, se=FALSE) +
  labs(
    x = "Blau Index (Diversity)",
    y = "Lou Overperformance Relative to Trump Vote Share",
    caption = "Plot: Zachary L. Hertz\nData: Statewide Database, IPUMS",
    title = "Prediction error from a model using only 2024 presidential \nresults is associated with precinct-level diversity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Interactive Map Preparation -----------------------------------------------
# Color palettes for interactive maps
pal <- colorNumeric(
  palette = c("#3266E9", "#FFFFFF", "#C20114"),
  domain = final_data$bruce.pct_24)

# Symmetric color palette for performance differences
data_range_trumpdiff <- max(abs(final_data$bruce_trump_pct_diff), na.rm = TRUE)
pal2 <- colorNumeric(
  palette = c("#DA611B", "#FFFFFF", "#AB2189"),
  domain = c(-data_range_trumpdiff, data_range_trumpdiff))

pal3 <- colorNumeric(
  palette = c("#FFFFFF", "#340068"),
  domain = final_data$asian_pct)

data_range_asianresids <- max(abs(final_data$resid.asian), na.rm = TRUE)
pal4 <- colorNumeric(
  palette = c("#EE6C4D", "#FFFFFF", "#340068"),
  domain = c(-data_range_asianresids, data_range_asianresids))

data_range_histresids <- max(abs(final_data$resid.hist), na.rm = TRUE)
pal5 <- colorNumeric(
  palette = c("#3266E9", "#FFFFFF", "#C20114"),
  domain = c(-data_range_histresids, data_range_histresids))

# Save plots -----------------------------------------------
ggsave("results_2024_map.png", p2)
ggsave("aa_pop_map.png", p3)
ggsave("scatter_lou_aa.png", p10)
ggsave("normalized_22_24_shift.png", p11)
ggsave("scatter_22_24_shift.png", p12)
ggsave("headline_22_24_shift.png", p13)
ggsave("residuals_22vote.png", p15)
ggsave("residuals_asianpop.png", p16)
ggsave("residuals_asianpoppres.png", p17)
ggsave("blau_map.png", p5)
ggsave("residuals_blau.png", p18)

# Objects available for further analysis:
#   - final_data: Complete dataset with all variables
#   - p1-p18: Various plots and visualizations
#   - pal, pal2-pal5: Color palettes for interactive maps