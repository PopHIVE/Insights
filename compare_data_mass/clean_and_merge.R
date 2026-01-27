# Clean and Merge Diabetes Data with All.csv
# This script cleans the poorly formatted diabetes file, standardizes labels,
# and merges with all.csv based on matching age, sex, and race/ethnicity categories.

library(tidyverse)

# ==============================================================================
# 1. READ AND CLEAN THE DIABETES FILE (with ethnicity)
# ==============================================================================

# Read diabetes file, skipping the metadata header (first 10 rows)
# Row 11-12 contain the column headers
diabetes_raw <- read_csv(
  "2026_01_26_C02_DIABETES_ICD_A1C_MA_AGE_SEX_RACE_ETH_YEAR_2018-2025.csv",
  skip = 12,  # Skip to first data row (rows 1-10 are metadata, 11-12 are headers)
  col_names = c("year", "age_group", "sex", "ethnicity", "race",
                "pct_diabetes_icd", "pct_a1c_65_plus", "n_patients"),
  show_col_types = FALSE
)

# Debug: show first few rows and check for NA pattern
cat("Raw data - first 5 rows:\n")
print(head(diabetes_raw, 5))
cat("\nNA counts per column:\n")
print(colSums(is.na(diabetes_raw)))

# Fill down the hierarchical structure (blank cells inherit from above)
diabetes_clean <- diabetes_raw %>%
  fill(year, .direction = "down") %>%
  fill(age_group, .direction = "down") %>%
  fill(sex, .direction = "down") %>%
  fill(ethnicity, .direction = "down")

# Remove Total/summary rows (contain "Total:" in any field)
diabetes_clean <- diabetes_clean %>%
  filter(
    !str_detect(age_group, "^Total:", ignore_case = TRUE),
    !str_detect(sex, "^Total:", ignore_case = TRUE),
    !str_detect(ethnicity, "^Total", ignore_case = TRUE),
    !str_detect(race, "^Total:", ignore_case = TRUE)
  )

# Clean percentage columns - remove "%" and convert to numeric
diabetes_clean <- diabetes_clean %>%
  mutate(
    pct_diabetes_icd = str_replace(pct_diabetes_icd, "%", ""),
    pct_a1c_65_plus = str_replace(pct_a1c_65_plus, "%", ""),
    # Handle suppressed values ("-")
    pct_diabetes_icd = na_if(pct_diabetes_icd, "-"),
    pct_a1c_65_plus = na_if(pct_a1c_65_plus, "-"),
    pct_diabetes_icd = as.numeric(pct_diabetes_icd),
    pct_a1c_65_plus = as.numeric(pct_a1c_65_plus),
    n_patients = as.numeric(n_patients)
  )

# Filter to only 2025 data
diabetes_clean <- diabetes_clean %>%
  filter(str_detect(year, "2025"))


# ==============================================================================
# 2. CREATE COMBINED RACE/ETHNICITY CATEGORY
# ==============================================================================

# Create race_ethnicity: Hispanic ethnicity takes priority over race
# If Hispanic -> "HISPANIC"
# If Not Hispanic -> use race category
# Exclude "None of the above" ethnicity (unknown ethnicity)

diabetes_clean <- diabetes_clean %>%
  filter(tolower(ethnicity) != "none of the above") %>%
  mutate(
    race_ethnicity = case_when(
      tolower(ethnicity) == "hispanic or latino" ~ "HISPANIC",
      tolower(ethnicity) == "not hispanic or latino" ~ toupper(race),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(ethnicity != "Total")

cat(sprintf("After creating race/ethnicity: %d rows\n", nrow(diabetes_clean)))

# ==============================================================================
# 3. STANDARDIZE CATEGORY LABELS (case insensitive)
# ==============================================================================

# Age group mapping (diabetes labels -> standardized labels)
age_mapping <- c(
  "less than 18 years" = NA_character_,  # No match in all.csv
  "≥ 18 and < 35 years" = "18-34",
  ">= 18 and < 35 years" = "18-34",
  "≥ 35 and < 45 years" = "35-44",
  ">= 35 and < 45 years" = "35-44",
  "≥ 45 and < 55 years" = "45-54",
  ">= 45 and < 55 years" = "45-54",
  "≥ 55 and < 65 years" = "55-64",
  ">= 55 and < 65 years" = "55-64",
  "≥ 65 and < 75 years" = "65-74",
  ">= 65 and < 75 years" = "65-74",
  "75 years or more" = "75+",
  "no value" = NA_character_  # Missing data - no match
)

# Sex mapping (diabetes labels -> standardized labels)
sex_mapping <- c(
  "male" = "M",
  "female" = "F"
)

# Race/ethnicity mapping (standardize race names)
race_eth_mapping <- c(
  "AMERICAN INDIAN OR ALASKA NATIVE" = "AMERICAN INDIAN/ALASKAN NATIVE",
  "ASIAN" = "ASIAN",
  "BLACK OR AFRICAN AMERICAN" = "BLACK",
  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" = "PACIFIC ISLANDER/HAWAIIAN",
  "WHITE" = "WHITE",
  "OTHER RACE" = "OTHER",
  "NONE OF THE ABOVE" = "Unknown",
  "HISPANIC" = "HISPANIC"
)

# Apply standardization using lowercase matching
diabetes_standardized <- diabetes_clean %>%
  mutate(
    age_group_std = age_mapping[tolower(age_group)],
    sex_std = sex_mapping[tolower(sex)],
    race_eth_std = race_eth_mapping[toupper(race_ethnicity)]
  )

# Track unmatched records before filtering
unmatched_diabetes <- diabetes_standardized %>%
  filter(is.na(age_group_std) | is.na(sex_std) | is.na(race_eth_std)) %>%
  distinct(age_group, sex, race_ethnicity, age_group_std, sex_std, race_eth_std)

# ==============================================================================
# 4. AGGREGATE BY YEAR, AGE, SEX, RACE/ETHNICITY
# ==============================================================================

# Since we have multiple race rows per Hispanic category, we need to aggregate
# For Hispanic: sum n_patients across all races, weighted average for percentages
# For Non-Hispanic: each race is already separate

# Custom weighted mean that handles NA in both x and w
safe_weighted_mean <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  if (sum(valid) == 0) return(NA_real_)
  weighted.mean(x[valid], w[valid])
}

# First filter, then aggregate
# Need to store n_patients in a temp variable to avoid reference issues in summarise
diabetes_aggregated <- diabetes_standardized %>%
  filter(!is.na(age_group_std) & !is.na(sex_std) & !is.na(race_eth_std)) %>%
  mutate(weight = n_patients) %>%  # Create a copy for weighting
  group_by(year, age_group_std, sex_std, race_eth_std) %>%
  summarise(
    # Weighted average for percentages (weight by original n_patients)
    pct_diabetes_icd = safe_weighted_mean(pct_diabetes_icd, weight),
    pct_a1c_65_plus = safe_weighted_mean(pct_a1c_65_plus, weight),
    # Sum n_patients after weighted means are calculated
    n_patients = sum(n_patients, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    age_group = age_group_std,
    sex = sex_std,
    race = race_eth_std
  )

cat(sprintf("After aggregation: %d rows\n", nrow(diabetes_aggregated)))
cat(sprintf("Unique race/ethnicity categories: %s\n",
            paste(unique(diabetes_aggregated$race), collapse = ", ")))

# ==============================================================================
# 5. READ AND PREPARE ALL.CSV
# ==============================================================================

all_data <- read_csv("all.csv", show_col_types = FALSE)

# Standardize all.csv labels to uppercase for consistent matching
all_data <- all_data %>%
  mutate(
    race = toupper(race),
    sex = toupper(sex)
  )

# Track races in all.csv that won't merge
all_races <- unique(toupper(all_data$race))
diabetes_races <- unique(diabetes_aggregated$race)

unmatched_all_races <- setdiff(all_races, diabetes_races)
unmatched_diabetes_races <- setdiff(diabetes_races, all_races)

# ==============================================================================
# 6. MERGE DATASETS
# ==============================================================================

# Merge on age_group, sex, and race
merged_data <- diabetes_aggregated %>%
  inner_join(
    all_data,
    by = c("age_group", "sex", "race"),
    suffix = c("_diabetes", "_all")
  )

# ==============================================================================
# 7. COMPARE DIABETES VALUES BETWEEN DATASETS
# ==============================================================================

# Calculate percentage from all.csv (dm / Den * 100)
merged_data <- merged_data %>%
  mutate(
    pct_Mass_DPH_dm = (dm / Den) * 100
  )

# ==============================================================================
# 8. GENERATE DATA CLEANING SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("DATA CLEANING AND MERGE SUMMARY\n")
cat("========================================\n\n")

cat("DIABETES FILE CLEANING:\n")
cat(sprintf("  - Original rows (after header removal): %d\n", nrow(diabetes_raw)))
cat(sprintf("  - After filtering and creating race/ethnicity: %d\n", nrow(diabetes_clean)))
cat(sprintf("  - After aggregation: %d\n", nrow(diabetes_aggregated)))

cat("\nRACE/ETHNICITY CATEGORIES IN DIABETES FILE:\n")
for (r in sort(unique(diabetes_aggregated$race))) {
  n <- sum(diabetes_aggregated$race == r)
  cat(sprintf("  - %s: %d rows\n", r, n))
}

cat("\nALL.CSV:\n")
cat(sprintf("  - Total rows: %d\n", nrow(all_data)))
cat(sprintf("  - Unique age-sex-race combinations: %d\n",
            nrow(distinct(all_data, age_group, sex, race))))

cat("\nMERGED RESULT:\n")
cat(sprintf("  - Merged rows: %d\n", nrow(merged_data)))

cat("\n----------------------------------------\n")
cat("UNMATCHED GROUPS - CANNOT BE MERGED\n")
cat("----------------------------------------\n\n")

cat("Race/ethnicity in DIABETES but not in all.csv:\n")
if (length(unmatched_diabetes_races) > 0) {
  cat(sprintf("  %s\n", paste(unmatched_diabetes_races, collapse = ", ")))
} else {
  cat("  None\n")
}

cat("\nRace in ALL.CSV but not in diabetes file:\n")
if (length(unmatched_all_races) > 0) {
  cat(sprintf("  %s\n", paste(unmatched_all_races, collapse = ", ")))
} else {
  cat("  None\n")
}

# ==============================================================================
# 9. SAVE OUTPUTS
# ==============================================================================

# Save merged data
write_csv(merged_data, "merged_data.csv")
cat("\nSaved: merged_data.csv\n")

# Save aggregated diabetes data (before merge)
write_csv(diabetes_aggregated, "diabetes_aggregated.csv")
cat("Saved: diabetes_aggregated.csv\n")

cat("\n========================================\n")
cat("PROCESSING COMPLETE\n")
cat("========================================\n")

