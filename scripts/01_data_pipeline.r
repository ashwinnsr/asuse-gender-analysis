# ==============================================================================
# 01_data_pipeline.r
# ASUSE 2023-24 — Data Cleaning & Analysis-Ready Dataset
#
# PURPOSE : Load raw SPSS blocks, clean, merge, and produce
#           asuse_final_gendered_analysis.rds for downstream analysis.
# OUTPUTS : <asuse_dir>/Analysis_Ready_Data/asuse_final_gendered_analysis.rds
#           <asuse_dir>/Analysis_Ready_Data/asuse_final_gendered_analysis.csv
# NEXT    : Run 02_analysis_main.r  →  03_charts_advanced.r
# ==============================================================================

# Load libraries
library(haven) # Read SPSS files
library(tidyverse) # Data manipulation
library(labelled) # Handle SPSS labels
library(janitor) # Clean names
library(skimr) # Data summary
library(naniar) # Missing data analysis
library(assertr) # Data validation
library(lubridate)
library(srvyr) # Survey-weighted analysis

# Set options for rigorous survey analysis (handle lonely PSUs)
options(survey.lonely.psu = "adjust")

# ============================================
# STEP 1: DEFINE PATHS AND FILES
# ============================================

# Define the directory where your ASUSE files are stored
asuse_dir <- "C:\\Users\\ashwin\\Documents\\IndiaSpend Article\\ASUSE202324sav"

# Create output directory
output_dir <- file.path(asuse_dir, "Analysis_Ready_Data")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ============================================
# STEP 2: LOAD ALL BLOCKS WITH IMPROVED STRUCTURE
# ============================================


# Define all files with their purposes
all_files <- c(
  "LEVEL - 01(Block 1 & item 1503, 1508 of Block 15).sav" = "Weights & Sampling",
  "LEVEL - 02(Block 2).sav" = "Establishment Characteristics & NIC",
  # "LEVEL - 03 (Block 2.1, 2.2 & 2.3).sav" = "Enterprise Details", # UNUSED - Not required for current analysis
  # "LEVEL - 04 (Block 3 - section 3.1 to 3.14).sav" = "Operations", # UNUSED - Not required
  # "LEVEL - 05 (Block 4).sav" = "Assets", # UNUSED - Not required
  # "LEVEL - 06 (Block 5 - section 5.1 to 5.14).sav" = "Expenses", # UNUSED - Not required
  "LEVEL - 07 (Block 6).sav" = "Loans",
  "LEVEL - 08 (Block 7.1 & 7.2).sav" = "Income/GVA",
  "LEVEL - 09 (Block 8).sav" = "Employment",
  "LEVEL - 10 (Block 9).sav" = "Wages",
  "LEVEL - 11 (Block 10).sav" = "Fixed Assets"
  # "LEVEL - 12 (Block 11).sav" = "Social Security", # UNUSED - Not used in final dataset
  # "LEVEL - 13 (Block 11.1).sav" = "Digital Access", # UNUSED - Taken from Block 02
  # "LEVEL - 14 (Block 12).sav" = "Problems", # UNUSED - Taken from Block 02
  # "LEVEL - 15 (Block 13).sav" = "Training", # UNUSED
  # "LEVEL - 16 (Block 14).sav" = "Income Validation" # UNUSED
)


loaded_blocks <- list()

for (file_name in names(all_files)) {
  file_path <- file.path(asuse_dir, file_name)

  if (file.exists(file_path)) {
    # Load the data
    data <- read_sav(file_path)

    # Clean column names
    data <- data %>% janitor::clean_names()

    # Transforms existing block names into simple Descriptive Block Names like block01, block02, etc.
    block_name <- gsub("LEVEL - (\\d+).*", "block\\1", file_name)
    block_name <- tolower(gsub("\\s+|\\(|\\)", "_", block_name))
    block_name <- gsub("_+", "_", block_name) # Remove multiple underscores

    # Store in list
    loaded_blocks[[block_name]] <- data
    rm(data)
    gc()
  } else {
    cat("  File not found\n")
  }
}


# Make all blocks available as individual objects
list2env(loaded_blocks, envir = .GlobalEnv)
rm(loaded_blocks)   # single rm() — list2env() already moved its contents out
gc()

# ============================================
# STEP 3: CREATE BASE COMPREHENSIVE DATASET
# ============================================

# Start with Block02 (Core Establishment Data)
# Create base from block02 with ALL key variables
analysis_base <- block02 %>%
  # Create Unique_ID
  mutate(
    Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum_no, sample_est_no, sep = "_"),

    # Basic identifiers
    Sector_Tag = case_when(
      sector == "1" ~ "Rural",
      sector == "2" ~ "Urban",
      TRUE ~ "Unknown"
    ),

    # Gender/Ownership
    Gender = case_when(
      ownership_type == "1" ~ "Male",
      ownership_type == "2" ~ "Female",
      ownership_type == "3" ~ "Transgender",
      ownership_type %in% c("4", "5") ~ "Partnership",
      ownership_type %in% c("6", "7", "8") ~ "Institutional",
      TRUE ~ "Other/Unknown"
    ),
    Is_Female_Owned = ifelse(ownership_type == "2", 1, 0),

    # Establishment Type
    Establishment_Type = case_when(
      est_type == "1" ~ "HWE",
      est_type == "2" ~ "OAE",
      TRUE ~ "Other"
    ),

    # Geographic variables
    Region_Code = nss_region,
    District_Code = district,

    # NIC CODES FOR PINK GHETTO ANALYSIS
    NIC_2digit = major_nic_2dig,
    NIC_5digit = major_nic_5dig,

    # Create 3-digit NIC for industry drill-down
    NIC_3digit = substr(as.character(NIC_5digit), 1, 3),

    # Digital Access from Block02
    Internet_Use = case_when(
      used_internet == "1" ~ "Yes",
      used_internet == "2" ~ "No",
      TRUE ~ "Not reported"
    ),
    Computer_Use = case_when(
      used_computer == "1" ~ "Yes",
      used_computer == "2" ~ "No",
      TRUE ~ "Not reported"
    )
  )

# Filter for proprietary establishments only (for gendered analysis)
analysis_base <- analysis_base %>%
  filter(Gender %in% c("Male", "Female", "Transgender"))


# ============================================
# STEP 4: ADD WEIGHTS FROM BLOCK01
# ============================================


# Create Unique_ID in block01
block01_design <- block01 %>%
  mutate(
    Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum_no, sample_establishment_no, sep = "_"),
  ) %>%
  select(Unique_ID, Weight = mlt, PSU = fsu_serial_no, Strata = second_stage_stratum_no) %>%
  distinct(Unique_ID, .keep_all = TRUE)

analysis_base <- analysis_base %>%
  left_join(block01_design, by = "Unique_ID", relationship = "many-to-one")

# DIAGNOSTIC: Check weight join success
weight_na_rate <- mean(is.na(analysis_base$Weight))
cat(sprintf("  Weight join NA rate: %.1f%% (should be near 0)\n", weight_na_rate * 100))
if (weight_na_rate > 0.05) warning("More than 5% of rows have missing weights — check Unique_ID construction in block01.")


# ============================================
# STEP 5: ADD INCOME/GVA (FINAL CORRECTED VERSION)
# ============================================


# 1. Load the Data
# Ensure you refer to the correct file object from your loader
# usually 'block08' corresponds to "LEVEL - 08 (Block 7.1 & 7.2).sav"
income_source <- block08 %>%
  mutate(
    # --- FIX 1: USE THE 4-PART UNIQUE ID ---
    # NOTE: block08 uses 'second_stage_stratum' (no _no suffix)
    Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum, sample_est_no, sep = "_")
  )

# 2. Extract GVA (Market) and NVA (Non-Market)
# item_no 769 = GVA at market prices; item_no 779 = NVA (non-market / home production)
# Per ASUSE survey manual, an enterprise reports EITHER 769 or 779, not both.
# Summing is therefore safe; the rare case of both rows appearing is treated as
# additive (e.g., mixed-activity units) which is conservative.
income_summary <- income_source %>%
  filter(item_no %in% c("769", "779")) %>%
  group_by(Unique_ID) %>%
  summarise(
    # Verify: check for double-reporters if needed
    # n_rows = n(),  # uncomment to diagnose multi-row IDs
    Monthly_Value = sum(as.numeric(value_rs), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Reference period for Block 7 is "Last 30 Days" → annualise
    Income = Monthly_Value * 12
  ) %>%
  select(Unique_ID, Income)

# 3. Merge into Main Dataset
analysis_base <- analysis_base %>%
  left_join(income_summary, by = "Unique_ID")

# DIAGNOSTIC: Check income join
# NOTE: block08 uses 'second_stage_stratum' (no _no suffix) vs block02's 'second_stage_stratum_no'
# If these differ in your data, Unique_IDs will not match and income will be largely NA.
# Verify with: names(block08)[grepl("stratum", names(block08))]
income_na_rate <- mean(is.na(analysis_base$Income))
cat(sprintf("  Income join NA rate: %.1f%% (enterprises with no income record)\n", income_na_rate * 100))
if (income_na_rate > 0.80) warning("Very high income NA rate — Unique_ID mismatch likely between block02 and block08. Check stratum column name: block08 may use 'second_stage_stratum', not 'second_stage_stratum_no'.")


# ============================================
# STEP 6: ADD EMPLOYMENT DATA FROM BLOCK09
# ============================================
block09_base <- block09 %>%
  mutate(
    Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum, sample_est_no, sep = "_")
  )

# Get total workers
emp_summary <- block09_base %>%
  group_by(Unique_ID) %>%
  summarise(
    Total_Workers = sum(as.numeric(total_workers), na.rm = TRUE),
    .groups = "drop"
  )

analysis_base <- analysis_base %>%
  left_join(emp_summary, by = "Unique_ID")

# DIAGNOSTIC: Check employment join
# block09 also uses 'second_stage_stratum' — same potential mismatch as block08
emp_na_rate <- mean(is.na(analysis_base$Total_Workers))
cat(sprintf("  Employment join NA rate: %.1f%% (establishments with no worker record)\n", emp_na_rate * 100))
if (emp_na_rate > 0.80) warning("Very high employment NA — check Unique_ID mismatch between block02 and block09.")


# ============================================
# STEP 7: ADD LOANS DATA FROM BLOCK07
# ============================================
block07_ids <- block07 %>%
  mutate(
    Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum, sample_est_no, sep = "_")
  )

# Convert value to numeric
loan_summary <- block07_ids %>%
  mutate(value_rs = as.numeric(value_rs)) %>%
  group_by(Unique_ID) %>%
  summarise(
    Total_Loans = sum(value_rs, na.rm = TRUE),
    .groups = "drop"
  )

analysis_base <- analysis_base %>%
  left_join(loan_summary, by = "Unique_ID")

# DIAGNOSTIC: Loan join (block07 also uses 'second_stage_stratum')
loan_na_rate <- mean(is.na(analysis_base$Total_Loans))
cat(sprintf("  Loan join NA rate: %.1f%% (establishments with no loan record is expected)\n", loan_na_rate * 100))


# ============================================
# STEP 8: ADD ASSETS DATA FROM BLOCK11
# ============================================
# Create Unique_ID for block11
block11_ids <- block11 %>%
  mutate(
    Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum_no, sample_estab_no, sep = "_")
  )

# Sum numeric columns as assets
numeric_cols <- c("mv_assets_owned", "mv_assets_hired", "net_additions_owned", "rent_hired_assets")

# Convert to numeric and sum
for (col in numeric_cols) {
  block11_ids[[col]] <- as.numeric(block11_ids[[col]])
}

asset_summary <- block11_ids %>%
  group_by(Unique_ID) %>%
  summarise(
    Total_Assets = sum(!!!syms(numeric_cols), na.rm = TRUE),
    .groups = "drop"
  )

analysis_base <- analysis_base %>%
  left_join(asset_summary, by = "Unique_ID")

# DIAGNOSTIC: Asset join (block11 uses 'second_stage_stratum_no' — should match block02)
asset_na_rate <- mean(is.na(analysis_base$Total_Assets))
cat(sprintf("  Asset join NA rate: %.1f%% (establishments with no asset record)\n", asset_na_rate * 100))


# ============================================
# STEP 9: PATCH - ADD CRITICAL "MUSCLE" VARIABLES
# ============================================


# ----------------------------------------------------------------
# 9.1 EXTRACT "INVISIBLE WORKPLACE" VARIABLES FROM BLOCK02
# ----------------------------------------------------------------
block2_muscle <- block02 %>%
  mutate(
    Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum_no, sample_est_no, sep = "_"),

    # THE CRITICAL MISSING VARIABLES
    # Location (Item 214) - Home vs Office
    Location_Code = location,

    # Registration (Item 223) - Formal vs Informal
    Registered_Code = registered,

    # Nature of Operation (Item 216) - Perennial/Seasonal
    Nature_Operation = nature_of_operation,

    # Most Severe Problem (Item 1202)
    Most_Severe_Problem = most_severe_problem,

    # Social Group (for intersectional analysis)
    Social_Group = social_group,

    # Education Level of Owner
    Owner_Education = education_level,

    # Years of Operation
    Years_Operation = years_of_operation
  ) %>%
  select(
    Unique_ID,
    Location_Code, Registered_Code, Nature_Operation, Most_Severe_Problem,
    Social_Group, Owner_Education, Years_Operation
  ) %>%
  distinct(Unique_ID, .keep_all = TRUE)

# ----------------------------------------------------------------
# 9.2 EXTRACT "SISTERHOOD" VARIABLES FROM BLOCK09 (FIXED)
# ----------------------------------------------------------------

# Based on the column names we saw earlier
employment_enhance <- block09 %>%
  mutate(
    Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum, sample_est_no, sep = "_"),

    # Convert to numeric - handle missing values
    full_time_male = as.numeric(full_time_male),
    full_time_female = as.numeric(full_time_female),
    full_time_trans = as.numeric(full_time_trans),
    part_time_male = as.numeric(part_time_male),
    part_time_female = as.numeric(part_time_female),
    part_time_trans = as.numeric(part_time_trans)
  ) %>%
  group_by(Unique_ID) %>%
  summarise(
    # Sum across worker categories (no casual columns in this dataset)
    Male_Workers = sum(full_time_male + part_time_male, na.rm = TRUE),
    Female_Workers = sum(full_time_female + part_time_female, na.rm = TRUE),
    Trans_Workers = sum(full_time_trans + part_time_trans, na.rm = TRUE),
    Total_Hired_Workers = Male_Workers + Female_Workers + Trans_Workers,
    .groups = "drop"
  )

# ----------------------------------------------------------------
# 9.3 EXTRACT WAGE DATA FROM BLOCK10 (FIXED)
# ----------------------------------------------------------------

# Check Block10 columns
wage_cols <- names(block10)[grepl("amount|earning|wage|salary|value",
  names(block10),
  ignore.case = TRUE
)]
if (length(wage_cols) > 0) {
  wages_enhance <- block10 %>%
    mutate(
      Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum, sample_est_no, sep = "_")
    ) %>%
    # Convert to numeric
    mutate(across(all_of(wage_cols), ~ as.numeric(.))) %>%
    group_by(Unique_ID) %>%
    summarise(
      Total_Wages = sum(!!!syms(wage_cols), na.rm = TRUE),
      .groups = "drop"
    )
} else {
  wages_enhance <- tibble(Unique_ID = character(), Total_Wages = numeric())
}

# ============================================
# STEP 10: MERGE ALL VARIABLES
# ============================================


asuse_final_comprehensive <- analysis_base %>%
  # Merge Block02 muscle variables
  left_join(block2_muscle, by = "Unique_ID") %>%
  # Merge properly extracted gender-specific employment
  left_join(employment_enhance, by = "Unique_ID") %>%
  # Merge wage data
  left_join(wages_enhance, by = "Unique_ID") %>%
  mutate(
    # Clean and create analysis-ready variables

    # 1. THE "INVISIBLE WORKPLACE" VARIABLE
    Work_Location = case_when(
      Location_Code == "1" ~ "Home-Based (Invisible)",
      Location_Code == "2" ~ "Fixed Premises",
      Location_Code == "3" ~ "Street/Mobile",
      Location_Code == "4" ~ "Construction Site",
      Location_Code == "5" ~ "Other",
      TRUE ~ "Not Reported"
    ),
    Is_Home_Based = ifelse(Work_Location == "Home-Based (Invisible)", 1, 0),

    # 2. THE "PAPER CEILING" VARIABLE
    Is_Registered = case_when(
      Registered_Code == "1" ~ "Formal (Registered)",
      Registered_Code == "2" ~ "Informal (Unregistered)",
      TRUE ~ "Unknown"
    ),

    # 3. THE "SISTERHOOD" VARIABLES (NOW WITH REAL DATA!)
    Female_Hired_Workers = replace_na(Female_Workers, 0),
    Male_Hired_Workers = replace_na(Male_Workers, 0),
    Trans_Hired_Workers = replace_na(Trans_Workers, 0),
    Total_Hired_Workers = replace_na(Total_Hired_Workers, 0),

    # Calculate hiring ratios
    Female_Hiring_Ratio = ifelse(Total_Hired_Workers > 0,
      round(Female_Hired_Workers / Total_Hired_Workers * 100, 1),
      0
    ),
    Has_Female_Employees = ifelse(Female_Hired_Workers > 0, 1, 0),
    Has_Any_Hired_Workers = ifelse(Total_Hired_Workers > 0, 1, 0),

    # 4. OPERATIONAL VARIABLES
    Operation_Type = case_when(
      Nature_Operation == "1" ~ "Perennial",
      Nature_Operation == "2" ~ "Seasonal",
      Nature_Operation == "3" ~ "Casual",
      TRUE ~ "Unknown"
    ),

    # 5. Create sector classifications from NIC codes
    NIC_numeric = as.numeric(str_extract(NIC_2digit, "\\d+")),

    # Broad Sector Classification
    Broad_Sector = case_when(
      NIC_numeric >= 10 & NIC_numeric <= 33 ~ "Manufacturing",
      NIC_numeric >= 45 & NIC_numeric <= 47 ~ "Trade",
      (NIC_numeric >= 36 & NIC_numeric <= 39) |
        (NIC_numeric >= 50 & NIC_numeric <= 96) ~ "Services",
      TRUE ~ "Other"
    ),

    # Detailed Manufacturing Sub-sectors
    Manufacturing_Subsector = case_when(
      NIC_numeric >= 10 & NIC_numeric <= 12 ~ "Food Products",
      NIC_numeric >= 13 & NIC_numeric <= 15 ~ "Beverages & Tobacco",
      NIC_numeric >= 16 & NIC_numeric <= 18 ~ "Textiles & Apparel",
      NIC_numeric >= 19 & NIC_numeric <= 22 ~ "Leather & Wood",
      NIC_numeric >= 23 & NIC_numeric <= 25 ~ "Paper & Chemicals",
      NIC_numeric >= 26 & NIC_numeric <= 28 ~ "Pharma & Rubber",
      NIC_numeric >= 29 & NIC_numeric <= 30 ~ "Metals & Machinery",
      NIC_numeric >= 31 & NIC_numeric <= 33 ~ "Electronics & Transport",
      TRUE ~ NA_character_
    ),

    # 6. Create financial ratios
    Debt_to_Asset_Ratio = ifelse(Total_Assets > 0, Total_Loans / Total_Assets, NA),
    Income_per_Worker = ifelse(Total_Workers > 0, Income / Total_Workers, NA),

    # Subsistence indicator (₹1.2 lakh/year = ₹10,000/month)
    Below_Subsistence = ifelse(Income < 120000, 1, 0),

    # 7. PROFITABILITY METRICS
    Wage_Share = ifelse(Income > 0 & !is.na(Total_Wages) & Total_Wages > 0,
      round(Total_Wages / Income * 100, 1),
      NA
    ),
    Value_Added_per_Worker = ifelse(Total_Workers > 0 & !is.na(Total_Wages),
      (Income - Total_Wages) / Total_Workers,
      NA
    ),

    # 8. PROBLEM CATEGORIZATION
    Problem_Category = case_when(
      Most_Severe_Problem == "1" ~ "Finance/Credit",
      Most_Severe_Problem == "2" ~ "Raw Material",
      Most_Severe_Problem == "3" ~ "Power Supply",
      Most_Severe_Problem == "4" ~ "Market/Linkage",
      Most_Severe_Problem == "5" ~ "Transport",
      Most_Severe_Problem == "6" ~ "Skilled Labor",
      Most_Severe_Problem == "7" ~ "Technology",
      Most_Severe_Problem == "8" ~ "Other",
      TRUE ~ "No Problem/Not Reported"
    ),

    # 9. SOCIAL GROUP
    Social_Group_Category = case_when(
      Social_Group == "1" ~ "ST",
      Social_Group == "2" ~ "SC",
      Social_Group == "3" ~ "OBC",
      Social_Group == "4" ~ "General",
      TRUE ~ "Not Reported"
    ),

    # 10. EDUCATION LEVEL
    Owner_Education_Level = case_when(
      Owner_Education == "1" ~ "Illiterate",
      Owner_Education == "2" ~ "Literate/Below Primary",
      Owner_Education == "3" ~ "Primary",
      Owner_Education == "4" ~ "Middle",
      Owner_Education == "5" ~ "Secondary",
      Owner_Education == "6" ~ "Higher Secondary",
      Owner_Education == "7" ~ "Graduate & Above",
      TRUE ~ "Not Reported"
    ),

    # 11. Clean up NA values
    across(
      c(
        Total_Loans, Total_Assets, Total_Workers, Total_Hired_Workers,
        Female_Hired_Workers, Male_Hired_Workers, Trans_Hired_Workers
      ),
      ~ replace_na(., 0)
    ),

    # 12. Clean digital access
    across(
      c(Internet_Use, Computer_Use),
      ~ replace_na(., "Not reported")
    )
  )

# ============================================
# STEP 11: CREATE FINAL ANALYSIS DATASET
# ============================================


final_analysis_variables <- c(
  # Core Identifiers
  "Unique_ID", "Weight", "PSU", "Strata",

  # Owner Demographics
  "Gender", "Is_Female_Owned", "Social_Group_Category", "Owner_Education_Level",

  # Geographic & Sectoral
  "Sector_Tag", "Region_Code", "District_Code",
  "NIC_2digit", "NIC_3digit", "NIC_5digit", "Broad_Sector", "Manufacturing_Subsector",

  # Establishment Characteristics (THE MUSCLE)
  "Establishment_Type", "Work_Location", "Is_Home_Based",
  "Is_Registered", "Operation_Type", "Years_Operation",

  # Employment (SISTERHOOD VARIABLES)
  "Total_Workers", "Total_Hired_Workers", "Female_Hired_Workers",
  "Male_Hired_Workers", "Female_Hiring_Ratio", "Has_Female_Employees", "Has_Any_Hired_Workers",

  # Financials
  "Income", "Total_Wages", "Total_Loans", "Total_Assets",
  "Income_per_Worker", "Wage_Share", "Value_Added_per_Worker",
  "Debt_to_Asset_Ratio", "Below_Subsistence",

  # Digital Access
  "Internet_Use", "Computer_Use",

  # Constraints
  "Problem_Category"
)

# Keep only variables that exist
final_vars_existing <- final_analysis_variables[final_analysis_variables %in% names(asuse_final_comprehensive)]

# Pre-dedup diagnostic: flag unexpected duplicates before silently dropping them
n_before <- nrow(asuse_final_comprehensive)
n_dups   <- sum(duplicated(asuse_final_comprehensive$Unique_ID))
if (n_dups > 0) {
  warning(sprintf(
    "%d duplicate Unique_IDs detected before deduplication (%.2f%% of rows). "
    , n_dups, n_dups / n_before * 100
  ))
}

asuse_final <- asuse_final_comprehensive %>%
  select(all_of(final_vars_existing)) %>%
  distinct(Unique_ID, .keep_all = TRUE)

cat(sprintf("  Rows before dedup: %d  |  After: %d  |  Dropped: %d\n",
            n_before, nrow(asuse_final), n_before - nrow(asuse_final)))

# Remove extreme outliers for realistic analysis
asuse_final <- asuse_final %>%
  filter(
    Income <= 10000000, # Remove > ₹1 crore income outliers
    Total_Assets <= 50000000, # Remove > ₹5 crore asset outliers
    Total_Loans <= 10000000 # Remove > ₹1 crore loan outliers
  )

# NSSO weights usually have a multiplier divisor (often 100)
# We calculate weighted totals for population estimates
asuse_survey <- asuse_final %>%
  filter(!is.na(Weight)) %>%
  as_survey_design(
    ids = PSU,
    strata = Strata,
    weights = Weight,
    nest = TRUE
  )

# ============================================
# STEP 12: DATA QUALITY CHECKS - CORRECTED
# ============================================


# Helper function for rigorous weighted percentages
calculate_weighted_percentages <- function(survey_obj, group_var, count_var) {
  tryCatch(
    {
      res <- survey_obj %>%
        group_by(!!sym(group_var), !!sym(count_var)) %>%
        summarise(
          Population_Estimate = survey_total(vartype = NULL),
          .groups = "drop"
        )

      # Use as.numeric to strip any srvyr attributes that might break sum()
      res <- res %>%
        group_by(!!sym(group_var)) %>%
        mutate(
          Population_Estimate = as.numeric(Population_Estimate),
          Percent = round(Population_Estimate / sum(Population_Estimate, na.rm = TRUE) * 100, 1)
        ) %>%
        ungroup()

      return(res)
    },
    error = function(e) {
      cat("  ERROR in calculate_weighted_percentages:", as.character(e), "\n")
      return(NULL)
    }
  )
}

# 1. HOME-BASED WORK ANALYSIS (WEIGHTED)
home_based_weighted <- calculate_weighted_percentages(asuse_survey, "Gender", "Work_Location")
if (!is.null(home_based_weighted)) print(home_based_weighted)

# 2. SISTERHOOD ANALYSIS (WEIGHTED)
hiring_summary_weighted <- asuse_survey %>%
  filter(Total_Hired_Workers > 0) %>%
  group_by(Gender) %>%
  summarise(
    Est_Firms_with_Hired_Workers = survey_total(vartype = NULL),
    With_Female_Employees = survey_total(Has_Female_Employees == 1, vartype = NULL),
    Avg_Female_Hiring_Ratio = survey_mean(Female_Hiring_Ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Percent_Hiring_Women = round(With_Female_Employees / Est_Firms_with_Hired_Workers * 100, 1)
  )
print(hiring_summary_weighted)

# 3. FORMALITY ANALYSIS (WEIGHTED)
cat("\n3. Registration Status by Gender (Weighted Population Estimates):\n")
registration_weighted <- calculate_weighted_percentages(asuse_survey, "Gender", "Is_Registered")
print(registration_weighted)

# 4. ASSET SANITY CHECK (WEIGHTED)
cat("\n4. Asset Distribution (Weighted Medians):\n")
asset_weighted <- asuse_survey %>%
  group_by(Gender, Is_Home_Based) %>%
  summarise(
    Pop_Count = survey_total(vartype = NULL),
    Median_Assets = survey_median(Total_Assets, na.rm = TRUE),
    .groups = "drop"
  )
print(asset_weighted)

# 5. PROBLEM CONSTRAINTS (WEIGHTED)
cat("\n5. Most Severe Problems by Gender (Weighted Population Estimates):\n")
problem_weighted <- calculate_weighted_percentages(asuse_survey, "Gender", "Problem_Category") %>%
  arrange(Gender, desc(Population_Estimate))
print(problem_weighted)

# 6. SISTERHOOD EFFECT (WEIGHTED)
cat("\n6. Sisterhood Effect - Female Hiring Ratio (Weighted Mean/Median):\n")
sisterhood_weighted <- asuse_survey %>%
  filter(Total_Hired_Workers > 0) %>%
  group_by(Gender) %>%
  summarise(
    Mean_Female_Ratio = survey_mean(Female_Hiring_Ratio, na.rm = TRUE),
    Median_Female_Ratio = survey_median(Female_Hiring_Ratio, na.rm = TRUE),
    .groups = "drop"
  )
print(sisterhood_weighted)

# 7. CROSS-TAB: HOME-BASED vs HIRING (WEIGHTED)
cat("\n7. Home-Based vs Hiring (Weighted Population Estimates):\n")
home_hiring_weighted <- asuse_survey %>%
  filter(Total_Hired_Workers > 0) %>%
  group_by(Gender, Is_Home_Based) %>%
  summarise(
    Pop_Count = survey_total(vartype = NULL),
    Avg_Female_Hiring_Ratio = survey_mean(Female_Hiring_Ratio, na.rm = TRUE),
    .groups = "drop"
  )
print(home_hiring_weighted)

# 8. FINANCIAL SUMMARY BY GENDER (WEIGHTED)
cat("\n8. Financial Summary by Gender (Weighted Population Estimates):\n")
financial_weighted <- asuse_survey %>%
  group_by(Gender) %>%
  summarise(
    Est_Total_Enterprises = survey_total(vartype = NULL),
    Median_Income = survey_median(Income, na.rm = TRUE),
    Mean_Income = survey_mean(Income, na.rm = TRUE),
    Median_Assets = survey_median(Total_Assets, na.rm = TRUE),
    Percent_Below_Subsistence = survey_mean(Below_Subsistence, na.rm = TRUE) * 100,
    .groups = "drop"
  )
print(financial_weighted)
# NOTE: Transgender estimates with NaN SEs indicate insufficient PSUs in strata for variance
# estimation. These cells are statistically unreliable and should NOT be reported with point
# estimates alone. Flag them as 'insufficient sample' in any publication.

# ============================================
# STEP 13: SAVE ENHANCED DATASET
# ============================================

cat("\n=== SAVING ENHANCED DATASET ===\n")

# ============================================
# WEIGHT SCALING VERIFICATION (Issue 7)
# ============================================
# NSS/MOSPI weights (mlt) are design weights that expand the sample to the population.
# Before using in print/reporting, verify total makes sense:
# India's enterprise universe ≈ 63 million (Eco Census 2013), ~200 million (projected).
# If sum(Weight) / n_establishments is >> 200, weights may need a divisor.
# Check:
wt_check <- asuse_final %>%
  group_by(Gender) %>%
  summarise(n_sample = n(), sum_wt = sum(Weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(avg_wt = sum_wt / n_sample)
cat("\nWeight Scaling Diagnostic:\n")
print(wt_check)
cat("\nIMPORTANT: If sum(Female Weight) >> 1B enterprises, divide Weight by 100 before reporting\n")
cat("Population totals reported by survey_total() are used for between-gender RATIOS — those are unaffected by a constant divisor.\n")
cat("Only standalone population count claims (e.g., '198.9 crore women entrepreneurs') require the correct divisor.\n\n")

saveRDS(asuse_final, file.path(output_dir, "asuse_final_gendered_analysis.rds"))
write.csv(asuse_final, file.path(output_dir, "asuse_final_gendered_analysis.csv"), row.names = FALSE)


cat("\n", strrep("=", 80), "\n", sep = "")
cat("DATASET READY — next steps:\n")
cat(strrep("=", 80), "\n", sep = "")
cat("\nRun in order:\n")
cat("  02_analysis_main.r   → survey-adjusted metrics + 7 publication charts\n")
cat("  03_charts_advanced.r → advanced visualisations (ridge, violin, lollipop ...)\n")
cat("\nDataset saved in:", output_dir, "\n")
cat("Main file: asuse_final_gendered_analysis.rds\n")
cat("\nWeight scaling reminder:\n")
cat("  Ratios (e.g. 2.8× more home-based) — unaffected by weight divisor.\n")
cat("  Standalone counts (e.g. '19.9 crore women') — divide sum(Weight) by 100.\n")
