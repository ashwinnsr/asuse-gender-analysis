# ==============================================================================
# CORRECTED ASUSE 2022-23 ANALYSIS WITH PROPER COLUMN MAPPING
# ==============================================================================

# Load libraries
library(tidyverse)
library(haven)
library(janitor)
library(ggplot2)

cat("=== CORRECTED ASUSE 2022-23 ANALYSIS ===\n\n")

# ============================================
# STEP 1: LOAD DATA WITH PROPER NAMES
# ============================================

cat("1. Loading data with proper column checking...\n")

# Load all blocks
block01 <- read_sav("ASUSE202223sav/LEVEL - 01(Block 1 & item 1503, 1508 of Block 15).sav")
block02 <- read_sav("ASUSE202223sav/LEVEL - 02(Block 2).sav")
block08 <- read_sav("ASUSE202223sav/LEVEL - 08 (Block 7.1 & 7.2).sav")  # Actually Block 7 (Expenses & Receipts)
block10 <- read_sav("ASUSE202223sav/LEVEL - 11 (Block 10).sav")  # Employment data

# Clean names
block01 <- clean_names(block01)
block02 <- clean_names(block02)
block08 <- clean_names(block08)
block10 <- clean_names(block10)

cat("   ✓ Data loaded\n")

# ============================================
# STEP 2: CHECK COLUMN NAMES
# ============================================

cat("\n2. Checking column names...\n")

# Check Block02 columns
cat("\nBlock02 column names (first 30):\n")
print(names(block02)[1:30])

# Look for ownership/gender column
cat("\nLooking for ownership/gender column...\n")
ownership_cols <- names(block02)[grep("own|type|202|proprietor", names(block02), ignore.case = TRUE)]
cat("Possible ownership columns:", paste(ownership_cols, collapse = ", "), "\n")

# Look for segment and sample columns
cat("\nLooking for segment/sample columns...\n")
segment_cols <- names(block02)[grep("segment|sample|estab", names(block02), ignore.case = TRUE)]
cat("Segment/sample columns:", paste(segment_cols, collapse = ", "), "\n")

# Show actual column names with their positions
cat("\nFull Block02 column list:\n")
for(i in 1:length(names(block02))) {
  cat(sprintf("%3d. %s\n", i, names(block02)[i]))
}

# ============================================
# STEP 3: EXPLORE ACTUAL DATA VALUES
# ============================================

cat("\n3. Exploring actual data values...\n")

# Check what b2q201 contains (should be establishment type)
cat("\nValues in b2q201 (should be establishment type):\n")
print(table(block02$b2q201)[1:10])

# Check what b2214 contains (should be location)
cat("\nValues in b2214 (should be location):\n")
print(table(block02$b2214)[1:10])

# Check what b2223 contains (should be registration)
cat("\nValues in b2223 (should be registration):\n")
print(table(block02$b2223)[1:10])

# Check what b2224 contains (should be social group)
cat("\nValues in b2224 (should be social group):\n")
print(table(block02$b2224)[1:10])

# ============================================
# STEP 4: FIND THE CORRECT GENDER COLUMN
# ============================================

cat("\n4. Searching for gender/ownership column...\n")

# According to NSS ASUSE, ownership type should be in Item 202
# Let's look for columns with "202" in the name
item202_cols <- names(block02)[grep("202", names(block02))]
cat("\nColumns with '202' in name:", paste(item202_cols, collapse = ", "), "\n")

if(length(item202_cols) > 0) {
  cat("\nValues in these columns:\n")
  for(col in item202_cols) {
    cat(paste("\n", col, ":\n", sep = ""))
    print(table(block02[[col]])[1:10])
  }
}

# Also check for ownership in b2202_b (since b2202_a was NIC codes)
cat("\nChecking b2202_b (might be ownership):\n")
if("b2202_b" %in% names(block02)) {
  print(table(block02$b2202_b)[1:10])
}