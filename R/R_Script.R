#//////////////////////////////////////////////////////////////////////////////#
# 
# TITLE: Opportunity Zones in New York City, Before and After 2018 (Script_v03)
# SUBTITLE: Top 10 vs Bottom 10 OZ tracts vs OZ-eligible, not designated tracts;
#           HPD completions (2014–2023) and ACS outcomes (2013–2017 vs 2019–2023)
# BY:    Paul Lee & Charles Nkurunziza
# DATE:  Dec 8, 2025
#
# MAIN GROUPS:
#   1) Top 10 OZ tracts (highest total housing stock, ACS 2019–23)
#   2) Bottom 10 OZ tracts (lowest total housing stock among “non-tiny” OZs)
#   3) Eligible but not designated tracts (CDFI “eligible” list minus OZs)
#
# MAIN FIGURES:
#   L1. Line: cumulative HPD “affordable” completions (2014–2023), 3 groups
#   B1. Bars: median gross rent (2013–17 vs 2019–23), 3 groups
#   B2. Bars: rent-burden share (≥30% of income; 2013–17 vs 2019–23), 3 groups
#   B3. Bars: median household income (2013–17 vs 2019–23), 3 groups
#   B4. Bars: white share of population (2013–17 vs 2019–23), 3 groups
#
# MAIN TABLES:
#   - hpd_cum_group.csv      (HPD completions by group & year, cumulative)
#   - acs_group_summary.csv  (ACS outcomes by group & period)
#
# INPUTS:
#   - designated-qozs.12.14.18.csv     (designated OZ tracts)
#   - eligible-qozs.2.27.18.csv        (all OZ-eligible tracts, CDFI)
#   - Affordable_Housing_Production_by_Building.csv  (HPD)
#   - nyct2020.csv                     (list of valid NYC census tract GEOIDs)
#
# NOTES:
#   - For ACS outcomes (rent, rent burden, income, race), we use 5-year ACS:
#       * 2017 (2013–2017 window) = “pre”
#       * 2023 (2019–2023 window) = “post”
#
#//////////////////////////////////////////////////////////////////////////////#

#------------------------------- PRE-ANALYSIS ---------------------------------#
library(tidyverse)
library(janitor)
library(scales)
library(tidycensus)
library(lubridate)
library(stringr)
library(forcats)

#------------------------------- COLOR PALETTE --------------------------------#
deep_blue     <- "#0B3C6D"
mid_blue      <- "#4379C3"
light_blue    <- "#6FA3E6"
grey_fill     <- "#BFBFBF"
grey_line     <- "grey60"
grey_text     <- "grey40"
accent_red    <- "#D84A4A"
label_white   <- "white"

fig_w <- 11; fig_h <- 6.5; fig_dpi <- 300

# Threshold / filter
hu_floor_bottom <- 500    # to avoid tiny/special tracts for bottom 10

#------------------------------ CENSUS API KEY --------------------------------#
# Use existing key
census_api_key("6a00d29884accc02aadc0242607ee52f88d6e504", install = FALSE)

#------------------------------ LOAD OZ LIST ----------------------------------#
oz_raw <- read_csv("designated-qozs.12.14.18.csv", show_col_types = FALSE) %>%
  clean_names()

oz_nyc <- oz_raw %>%
  transmute(
    geoid = as.character(census_tract_number),
    is_oz = 1L
  )

#------------------------- LOAD OZ-ELIGIBLE TRACTS (CDFI) ---------------------#
# CDFI file: all tracts that were eligible to be designated as OZs.
# We'll use this to define “eligible but not designated” comparison tracts.

eligible_raw <- read_csv("eligible-qozs.2.27.18.csv", show_col_types = FALSE) %>%
  clean_names()

eligible_nyc <- eligible_raw %>%
  mutate(
    # after clean_names(), the key tract field is low_income_communities_li_cs
    tract11     = str_pad(low_income_communities_li_cs, width = 11, side = "left", pad = "0"),
    state_fips  = substr(tract11, 1, 2),
    county_fips = substr(tract11, 3, 5)
  ) %>%
  # Keep only NYC: NY state (36) + the 5 NYC counties
  filter(
    state_fips == "36",
    county_fips %in% c("005", "047", "061", "081", "085")
  ) %>%
  transmute(
    geoid       = tract11,
    is_eligible = 1L
  ) %>%
  distinct()

#------------------------------ LOAD HPD CSV ----------------------------------#
hpd <- read_csv(
  "Affordable_Housing_Production_by_Building.csv",
  col_types = cols(
    `Project Start Date`       = col_date("%m/%d/%Y"),
    `Project Completion Date`  = col_date("%m/%d/%Y"),
    `Building Completion Date` = col_date("%m/%d/%Y")
  ),
  show_col_types = FALSE
) %>%
  clean_names()

#-------------------------- BOROUGH - COUNTY FIPS -----------------------------#
county_fips <- c(
  "Bronx"         = "005",
  "Brooklyn"      = "047",  # Kings
  "Manhattan"     = "061",  # New York
  "Queens"        = "081",
  "Staten Island" = "085"   # Richmond
)

#--------------------------- MAKE 11-DIGIT GEOID (HPD) ------------------------#
hpd <- hpd %>%
  mutate(
    county = county_fips[as.character(borough)],
    ct4    = str_pad(str_extract(as.character(census_tract), "\\d+"), 4, "left", "0"),
    tract6 = if_else(is.na(ct4), NA_character_, paste0(ct4, "00")),
    geoid  = if_else(
      !is.na(county) & !is.na(tract6),
      paste0("36", county, tract6),
      NA_character_
    )
  ) %>%
  filter(!is.na(geoid))

#------------------------ HPD TRACT TOTALS (2018–2023 WINDOW) -----------------#
# We keep this post window for the basic “aff_units_post” field.

hpd_tract_post <- hpd %>%
  mutate(
    year_c      = year(building_completion_date),
    total_units = replace_na(total_units, 0)
  ) %>%
  filter(!is.na(year_c), year_c >= 2018, year_c <= 2023) %>%
  group_by(geoid) %>%
  summarise(
    aff_units_post = sum(total_units, na.rm = TRUE),
    .groups = "drop"
  )

#------------------------ HPD TRACT TOTALS BY YEAR (2014–2023) ----------------#
# For the cumulative HPD line, we need annual counts by tract.

hpd_year_tract <- hpd %>%
  mutate(
    year_c      = year(building_completion_date),
    total_units = replace_na(total_units, 0)
  ) %>%
  filter(!is.na(year_c), year_c >= 2014, year_c <= 2023) %>%
  group_by(geoid, year_c) %>%
  summarise(
    aff_units_year = sum(total_units, na.rm = TRUE),
    .groups = "drop"
  )

#------------------------------- ACS: TOTAL HOUSING ---------------------------#
# Total housing units (B25001_001), 2019–23 5-year ACS

acs_post <- get_acs(
  geography   = "tract",
  state       = "NY",
  county      = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
  year        = 2023,
  survey      = "acs5",
  variables   = c(hu = "B25001_001"),
  cache_table = TRUE
) %>%
  clean_names() %>%
  transmute(geoid, hu_post = estimate) %>%
  filter(!is.na(hu_post))

# Filtering out any census tracts not in the CT shapefile
nyct2020 <- read_csv("nyct2020.csv",
                     col_types = cols(GEOID = col_character())) %>%
  clean_names()

acs_post_filtered <- acs_post %>%
  filter(geoid %in% nyct2020$geoid)

#----------------------------------- JOIN (BASE DF) ---------------------------#
# Build main tract-level dataframe: ACS + HPD (post window) + OZ flags + eligibility

df <- acs_post_filtered %>%
  left_join(hpd_tract_post, by = "geoid") %>%
  left_join(oz_nyc,         by = "geoid") %>%
  left_join(eligible_nyc,   by = "geoid") %>%
  mutate(
    is_oz            = replace_na(is_oz, 0L),
    is_eligible      = replace_na(is_eligible, 0L),
    aff_units_post   = replace_na(aff_units_post, 0),
    other_units_post = pmax(hu_post - aff_units_post, 0),
    aff_share_post   = if_else(hu_post > 0, aff_units_post / hu_post, NA_real_)
  )

# At this point:
#   - df has one row per tract in NYC (limited to nyct2020)
#   - hu_post        = ACS total housing 2019–23
#   - aff_units_post = HPD affordable units 2018–23
#   - aff_share_post = aff_units_post / hu_post
#   - is_oz          = 1 for OZ tracts, 0 otherwise
#   - is_eligible    = 1 for OZ-eligible tracts (CDFI), 0 otherwise

#-------------------------- DEFINE 3 COMPARISON GROUPS ------------------------#
# Groups are defined exactly as:
#   1) Top 10 OZ tracts by total housing units (hu_post)
#   2) Bottom 10 OZ tracts by hu_post, among OZ tracts with hu_post >= hu_floor_bottom
#   3) Eligible but not designated (is_oz == 0 & is_eligible == 1)

oz_df <- df %>%
  filter(is_oz == 1L)

top10_oz <- oz_df %>%
  arrange(desc(hu_post)) %>%
  slice_head(n = 10) %>%
  mutate(group3 = "Top 10 OZ tracts")

bottom10_oz <- oz_df %>%
  filter(hu_post >= hu_floor_bottom) %>%    # avoid tiny/special tracts
  arrange(hu_post) %>%
  slice_head(n = 10) %>%
  mutate(group3 = "Bottom 10 OZ tracts")

top_ids    <- top10_oz$geoid
bottom_ids <- bottom10_oz$geoid

df <- df %>%
  mutate(
    group3 = case_when(
      geoid %in% top_ids                       ~ "Top 10 OZ tracts",
      geoid %in% bottom_ids                    ~ "Bottom 10 OZ tracts",
      is_oz == 0L & is_eligible == 1L          ~ "Eligible, not designated",
      TRUE                                     ~ NA_character_
    ),
    # lock in a consistent ordering for charts
    group3 = factor(group3,
                    levels = c("Top 10 OZ tracts",
                               "Bottom 10 OZ tracts",
                               "Eligible, not designated"))
  )

# Small lookup table to attach group labels elsewhere
group_lookup <- df %>%
  select(geoid, group3, hu_post) %>%
  filter(!is.na(group3))

#------------------------------ HPD CUMULATIVE LINE ---------------------------#
# Aggregate annual HPD completions to the 3 groups and compute cumulative totals.
# Then normalize by each group's total housing stock (ACS 2019–23) so that
# lines are comparable across groups of different size (Furman-style share).

# 1) Group-level housing stock (denominator: total units, ACS 2019–23)
group_hu_totals <- group_lookup %>%
  group_by(group3) %>%
  summarise(
    group_hu_post = sum(hu_post, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Annual HPD completions by group, with cumulative sum
hpd_year_group <- hpd_year_tract %>%
  left_join(group_lookup %>% select(geoid, group3), by = "geoid") %>%
  filter(!is.na(group3)) %>%
  group_by(group3, year_c) %>%
  summarise(
    aff_units_year = sum(aff_units_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(group3) %>%
  tidyr::complete(year_c = 2014:2023, fill = list(aff_units_year = 0)) %>%
  arrange(year_c) %>%
  mutate(
    cum_aff_units = cumsum(aff_units_year)
  ) %>%
  ungroup() %>%
  # 3) Attach denominators and compute cumulative share
  left_join(group_hu_totals, by = "group3") %>%
  mutate(
    share_cum_aff = if_else(
      group_hu_post > 0,
      cum_aff_units / group_hu_post,
      NA_real_
    )
  )

# Save the HPD cumulative table (now includes both counts and shares)
write_csv(hpd_year_group, "hpd_cum_group.csv")

# Cumulative HPD units as share of housing stock, dashed line at 2018
fig_hpd_cum <- ggplot(hpd_year_group,
                      aes(x = year_c, y = share_cum_aff, color = group3)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = grey_line) +
  # label for OZ designation year
  annotate("text",
           x = 2018,
           y = 0.072,
           label = "OZ designation (2018)",
           vjust = -0.5,
           size = 3,
           color = grey_text) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = 2014:2023) +
  labs(
    title    = "Where NYC’s Subsidized “Affordable” Housing Was Built (2014–2023)",
    subtitle = "Cumulative share of housing built with HPD financing in large OZ tracts, small OZ tracts, and OZ-eligible tracts that weren’t designated.",
    x = "Year affordable buildings were completed",
    y = "Cumulative affordable units as a share of all housing units",
    color = "Tract group:",
    caption = "Sources: HPD Affordable Housing Production by Building; ACS 2019–2023 (B25001)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(
      hjust = 0, face = "bold", color = deep_blue,
      size = 18, margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      hjust = 0, size = 11, margin = margin(b = 10)
    ),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

ggsave("fig_L1_hpd_cumulative_share.png",
       fig_hpd_cum, width = fig_w, height = fig_h, dpi = fig_dpi, bg = "white")



#------------------------- ACS OUTCOMES (PRE VS POST) -------------------------#
# Compare 2013–2017 vs 2019–2023 using 5-year ACS:
#   - year = 2017 (2013–2017)
#   - year = 2023 (2019–2023)

# Function to get ACS outcomes for one year (rent, rent burden, income, white pop share)

get_acs_outcomes <- function(year_label) {
  # Median gross rent (B25064_001)
  rent <- get_acs(
    geography   = "tract",
    state       = "NY",
    county      = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
    year        = year_label,
    survey      = "acs5",
    variables   = c(med_rent = "B25064_001"),
    cache_table = TRUE
  ) %>%
    clean_names() %>%
    transmute(
      geoid,
      med_rent = estimate
    )
  
  # Rent burden (≥30% of income) from B25070:
  # sum of categories 30–34.9, 35–39.9, 40–49.9, ≥50%
  rentburden_raw <- get_acs(
    geography   = "tract",
    state       = "NY",
    county      = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
    year        = year_label,
    survey      = "acs5",
    variables   = c(
      total   = "B25070_001",
      ge30_1  = "B25070_007",
      ge30_2  = "B25070_008",
      ge30_3  = "B25070_009",
      ge30_4  = "B25070_010"
    ),
    cache_table = TRUE
  ) %>%
    clean_names() %>%
    select(geoid, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate)
  
  rentburden <- rentburden_raw %>%
    transmute(
      geoid,
      rb_share = if_else(
        total > 0,
        (ge30_1 + ge30_2 + ge30_3 + ge30_4) / total,
        NA_real_
      )
    )
  
  # Median household income (B19013_001)
  inc <- get_acs(
    geography   = "tract",
    state       = "NY",
    county      = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
    year        = year_label,
    survey      = "acs5",
    variables   = c(med_inc = "B19013_001"),
    cache_table = TRUE
  ) %>%
    clean_names() %>%
    transmute(
      geoid,
      med_inc = estimate
    )
  
  # White share from B02001 (white alone / total population)
  race_raw <- get_acs(
    geography   = "tract",
    state       = "NY",
    county      = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
    year        = year_label,
    survey      = "acs5",
    variables   = c(
      total_pop = "B02001_001",
      white_pop = "B02001_002"
    ),
    cache_table = TRUE
  ) %>%
    clean_names() %>%
    select(geoid, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate)
  
  race <- race_raw %>%
    transmute(
      geoid,
      white_share = if_else(
        total_pop > 0,
        white_pop / total_pop,
        NA_real_
      )
    )
  
  # Merge all ACS outcomes for this year
  out <- rent %>%
    left_join(rentburden, by = "geoid") %>%
    left_join(inc,        by = "geoid") %>%
    left_join(race,       by = "geoid")
  
  out
}

# Get ACS outcomes for pre and post windows
acs_2017 <- get_acs_outcomes(2017) %>%
  mutate(period = "2013–2017 (ACS 2017)")

acs_2023 <- get_acs_outcomes(2023) %>%
  mutate(period = "2019–2023 (ACS 2023)")

acs_all <- bind_rows(acs_2017, acs_2023)

# Join group labels, keep only the 3 main groups
acs_grouped <- acs_all %>%
  left_join(group_lookup %>% select(geoid, group3), by = "geoid") %>%
  filter(!is.na(group3))

# Group-level summaries by period
#   - median for rent
#   - median for rent burden
#   - median for income
#   - median for white share

acs_group_summary <- acs_grouped %>%
  group_by(group3, period) %>%
  summarise(
    median_rent        = median(med_rent,     na.rm = TRUE),
    median_rent_burden = median(rb_share,     na.rm = TRUE),
    median_income      = median(med_inc,      na.rm = TRUE),
    median_white_share = median(white_share,  na.rm = TRUE),
    .groups = "drop"
  )


# Save the ACS summary table
write_csv(acs_group_summary, "acs_group_summary.csv")

#---------------------------- CHANGE SUMMARY TABLE ----------------------------#
# Summarizes pre vs post medians and changes by group.

acs_change_table <- acs_group_summary %>%
  # Simple pre / post flag for cleaner column names
  mutate(
    period_short = dplyr::case_when(
      stringr::str_detect(period, "2013") ~ "pre",
      stringr::str_detect(period, "2019") ~ "post",
      TRUE                                ~ NA_character_
    )
  ) %>%
  select(-period) %>%
  tidyr::pivot_wider(
    names_from  = period_short,
    values_from = c(median_rent,
                    median_rent_burden,
                    median_income,
                    median_white_share)
  ) %>%
  mutate(
    # Absolute & % change in median rent
    rent_change_abs = median_rent_post - median_rent_pre,
    rent_change_pct = rent_change_abs / median_rent_pre,
    
    # Rent burden and white share are proportions (0–1); convert change to p.p.
    rb_change_pp    = (median_rent_burden_post - median_rent_burden_pre) * 100,
    
    # Absolute & % change in income
    inc_change_abs  = median_income_post - median_income_pre,
    inc_change_pct  = inc_change_abs / median_income_pre,
    
    white_change_pp = (median_white_share_post - median_white_share_pre) * 100
  ) %>%
  # Keep both medians and changes (all numeric)
  select(
    group3,
    median_rent_pre,        median_rent_post,
    rent_change_abs,        rent_change_pct,
    median_rent_burden_pre, median_rent_burden_post,
    rb_change_pp,
    median_income_pre,      median_income_post,
    inc_change_abs,         inc_change_pct,
    median_white_share_pre, median_white_share_post,
    white_change_pp
  )

# Save for use in the blog / Datawrapper
write_csv(acs_change_table, "acs_change_summary.csv")


#----------------------- EXPORT GROUP TOTALS FOR DATAWRAPPER ------------------#
group_totals_export <- df %>%
  filter(!is.na(group3)) %>%
  group_by(group3) %>%
  summarise(
    total_housing_units = sum(hu_post, na.rm = TRUE),
    affordable_units    = sum(aff_units_post, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    other_units       = pmax(total_housing_units - affordable_units, 0),
    affordable_share  = if_else(total_housing_units > 0,
                                affordable_units / total_housing_units, NA_real_)
  )

# Save for Datawrapper
write_csv(group_totals_export, "group_totals_for_datawrapper.csv")

#----------------------------- BAR CHARTS (ACS) -------------------------------#
# One bar chart per outcome, 3 groups × 2 periods.

# Relabel the period variable for cleaner legends
acs_group_summary <- acs_group_summary %>%
  mutate(period = recode(period,
                         "2013–2017 (ACS 2017)" = "2013–2017",
                         "2019–2023 (ACS 2023)" = "2019–2023"))

#---------------------- FIGURE 1: Median Gross Rent ---------------------------#
fig_rent <- ggplot(acs_group_summary,
                   aes(x = group3, y = median_rent, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title    = "Typical Monthly Rent by Tract Group, Before and After 2018",
    subtitle = "Median gross rent by census tract group, comparing 2013–2017 and 2019–2023.",
    x = "Census tract group",
    y = "Median monthly gross rent",
    fill = "Time Period:",
    caption = "Source: ACS 5-year tables (B25064), 2013–2017 and 2019–2023."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0, face = "bold", color = deep_blue,
                                 size = 18, margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0, size = 11, margin = margin(b = 10)),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

ggsave("fig_B1_median_rent.png", fig_rent,
       width = fig_w, height = fig_h, dpi = fig_dpi, bg = "white")


#---------------------- FIGURE 2: Rent Burden ---------------------------------#
fig_rentburden <- ggplot(acs_group_summary,
                         aes(x = group3, y = median_rent_burden, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Share of Rent-Burdened Households by Tract Group",
    subtitle = "Median share of renter households paying 30% or more of income on rent, 2013–2017 vs 2019–2023.",
    x = "Census tract group",
    y = "Median share of rent-burdened households",
    fill = "Time Period:",
    caption = "Source: ACS 5-year tables (B25070), 2013–2017 and 2019–2023."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0, face = "bold", color = deep_blue,
                                 size = 18, margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0, size = 11, margin = margin(b = 10)),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

ggsave("fig_B2_rent_burden.png", fig_rentburden,
       width = fig_w, height = fig_h, dpi = fig_dpi, bg = "white")


#---------------------- FIGURE 3: Median Household Income ---------------------#
fig_income <- ggplot(acs_group_summary,
                     aes(x = group3, y = median_income, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title    = "Household Incomes by Tract Group, Before and After 2018",
    subtitle = "Median household income by census tract group, comparing 2013–2017 and 2019–2023.",
    x = "Census tract group",
    y = "Median household income (annual)",
    fill = "Time Period:",
    caption = "Source: ACS 5-year tables (B19013), 2013–2017 and 2019–2023."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0, face = "bold", color = deep_blue,
                                 size = 18, margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0, size = 11, margin = margin(b = 10)),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

ggsave("fig_B3_median_income.png", fig_income,
       width = fig_w, height = fig_h, dpi = fig_dpi, bg = "white")


#---------------------- FIGURE 4: White Share of Population -------------------#
fig_white <- ggplot(acs_group_summary,
                    aes(x = group3, y = median_white_share, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "White Share of Population by Tract Group, Before and After 2018",
    subtitle = "Median share of residents identifying as white alone, 2013–2017 vs 2019–2023.",
    x = "Census tract group",
    y = "Median share of white residents",
    fill = "Time Period:",
    caption = "Source: ACS 5-year tables (B02001), 2013–2017 and 2019–2023."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0, face = "bold", color = deep_blue,
                                 size = 18, margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0, size = 11, margin = margin(b = 10)),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

ggsave("fig_B4_white_share.png", fig_white,
       width = fig_w, height = fig_h, dpi = fig_dpi, bg = "white")

#------------------------------- END OF FILE ----------------------------------#
