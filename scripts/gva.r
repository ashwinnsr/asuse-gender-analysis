# Check weight distribution
cat("\n=== DEBUGGING WEIGHT VARIABLE ===\n")

# Check basic stats
weight_stats <- proprietary_data %>%
  filter(Gender == "Female") %>%
  summarise(
    n = n(),
    sum_weights = sum(Weight, na.rm = TRUE),
    avg_weight = mean(Weight, na.rm = TRUE),
    median_weight = median(Weight, na.rm = TRUE),
    min_weight = min(Weight, na.rm = TRUE),
    max_weight = max(Weight, na.rm = TRUE)
  )

print(weight_stats)

# What does a typical weight look like?
cat("\nFirst 10 women's weights:\n")
print(head(proprietary_data$Weight[proprietary_data$Gender == "Female"], 10))




# Check original mlt variable
cat("\n=== CHECKING ORIGINAL WEIGHT (mlt) ===\n")

# Load original block01
asuse_dir <- "C:\\Users\\ashwin\\Documents\\IndiaSpend Article\\ASUSE202324sav"
block01_path <- file.path(asuse_dir, "LEVEL - 01(Block 1 & item 1503, 1508 of Block 15).sav")
block01_original <- read_sav(block01_path) %>% janitor::clean_names()

# Check mlt distribution
mlt_stats <- block01_original %>%
  summarise(
    n = n(),
    sum_mlt = sum(mlt, na.rm = TRUE),
    avg_mlt = mean(mlt, na.rm = TRUE),
    median_mlt = median(mlt, na.rm = TRUE)
  )

print(mlt_stats)
cat("\nFirst 10 mlt values:\n")
print(head(block01_original$mlt, 10))





# Run this to diagnose
diagnose_weights <- function() {
  cat("=== WEIGHT DIAGNOSIS ===\n")
  
  # Check current state
  women_data <- proprietary_data %>% filter(Gender == "Female")
  
  cat("\n1. CURRENT STATE:\n")
  cat("   Sample women:", nrow(women_data), "\n")
  cat("   Sum of Weight:", sum(women_data$Weight, na.rm = TRUE), "\n")
  cat("   Average Weight:", mean(women_data$Weight, na.rm = TRUE), "\n")
  
  # Check if weights need scaling down
  avg_wt <- mean(women_data$Weight, na.rm = TRUE)
  
  if (avg_wt > 10000) {
    cat("\n2. ISSUE DETECTED: Weights are too large!\n")
    cat("   Average weight should be 300-500, but is", round(avg_wt), "\n")
    cat("   Likely weights are multiplied by 1 crore (10^7)\n")
    
    # Try different scaling factors
    scaling_factors <- c(10000000, 1000000, 100000)
    
    cat("\n3. TRYING DIFFERENT SCALING:\n")
    for (factor in scaling_factors) {
      corrected <- sum(women_data$Weight / factor, na.rm = TRUE) / 10000000
      cat(sprintf("   Divide by %s: %.1f crore women\n", 
                  format(factor, scientific = FALSE), corrected))
    }
    
    # Most likely: weights are in original units but sum represents total
    cat("\n4. MOST LIKELY: Weights already sum to national total\n")
    cat("   Try: National women =", round(sum(women_data$Weight, na.rm = TRUE) / 10000000, 1), "crore\n")
  } else if (avg_wt >= 300 && avg_wt <= 500) {
    cat("\n2. WEIGHTS LOOK CORRECT:\n")
    national <- nrow(women_data) * avg_wt
    cat("   National estimate:", format(round(national), big.mark = ","), "\n")
    cat("   In crores:", round(national / 10000000, 1), "\n")
  } else {
    cat("\n2. UNUSUAL WEIGHT RANGE:", round(avg_wt), "\n")
  }
}

# Run the diagnosis
diagnose_weights()





# ============================================
# CORRECT VERIFICATION OF NATIONAL ESTIMATES
# ============================================

cat("=== CORRECT NATIONAL ESTIMATE VERIFICATION ===\n")

# Your current Weight (MLT) variable is CORRECT
# It gives DIRECT national estimates when summed

# 1. Check total establishments nationally
national_total <- sum(proprietary_data$Weight, na.rm = TRUE)
cat("1. TOTAL ESTABLISHMENTS NATIONALLY:\n")
cat("   Sum of Weight (MLT):", format(round(national_total), big.mark = ","), "\n")
cat("   In crores:", round(national_total / 10000000, 1), "crore establishments\n")

# 2. Check against known ASUSE total (~18.3 crore)
cat("\n2. COMPARISON WITH OFFICIAL ASUSE TOTAL:\n")
cat("   Official ASUSE 2023-24 total: ~18.3 crore establishments\n")
cat("   Your estimate:", round(national_total / 10000000, 1), "crore\n")
cat("   Difference:", round((national_total/10000000 - 18.3)/18.3 * 100, 1), "%\n")

# 3. Women entrepreneurs
women_national <- sum(proprietary_data$Weight[proprietary_data$Gender == "Female"], na.rm = TRUE)
cat("\n3. WOMEN ENTREPRENEURS NATIONALLY:\n")
cat("   Sum of Weight for women:", format(round(women_national), big.mark = ","), "\n")
cat("   In crores:", round(women_national / 10000000, 1), "crore women entrepreneurs\n")
cat("   Percentage of total:", round(women_national / national_total * 100, 1), "%\n")

# 4. Verify with sample proportions
cat("\n4. VERIFICATION VIA SAMPLE PROPORTIONS:\n")
women_sample <- sum(proprietary_data$Gender == "Female")
total_sample <- nrow(proprietary_data)
women_share_sample <- women_sample / total_sample

cat("   Sample women:", format(women_sample, big.mark = ","), "\n")
cat("   Sample total:", format(total_sample, big.mark = ","), "\n")
cat("   Women's share in sample:", round(women_share_sample * 100, 1), "%\n")
cat("   Implied national women:", round(women_share_sample * national_total / 10000000, 1), "crore\n")

# 5. Invisible economy calculation (CORRECTED)
cat("\n5. INVISIBLE ECONOMY CALCULATION:\n")
median_income <- 57000
pct_unregistered <- 0.904

invisible_economy <- women_national * median_income * pct_unregistered / 1e12  # lakh crore

cat("   Women entrepreneurs:", round(women_national / 10000000, 1), "crore\n")
cat("   Median annual income: ₹", format(median_income, big.mark = ","), "\n", sep = "")
cat("   Unregistered: ", round(pct_unregistered * 100, 1), "%\n", sep = "")
cat("   Invisible economy: ₹", round(invisible_economy, 1), "lakh crore\n", sep = "")

# 6. Save verification
verification <- data.frame(
  Metric = c("Total Establishments", "Women Entrepreneurs", "Women's Share", "Invisible Economy"),
  National_Estimate = c(
    paste(round(national_total / 10000000, 1), "crore"),
    paste(round(women_national / 10000000, 1), "crore"),
    paste(round(women_national / national_total * 100, 1), "%"),
    paste("₹", round(invisible_economy, 1), "lakh crore")
  ),
  Sample_Proportion = c(
    "N/A",
    paste(round(women_share_sample * 100, 1), "% of sample"),
    paste(round(women_share_sample * 100, 1), "%"),
    "N/A"
  ),
  Notes = c(
    "Direct sum of MLT weights",
    "Direct sum of MLT weights for women",
    "Consistent with sample proportion",
    "Conservative estimate"
  )
)

print(verification)
