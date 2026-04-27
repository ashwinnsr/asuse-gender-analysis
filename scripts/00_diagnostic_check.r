# ==============================================================================
# 00_diagnostic_check.r
# Validate Unique_ID construction & column consistency across all blocks
# Run BEFORE 01_data_pipeline.r to catch join mismatches
# ==============================================================================

library(haven)
library(janitor)
library(dplyr)

asuse_dir <- "C:\\Users\\ashwin\\Documents\\IndiaSpend Article\\ASUSE202324sav"

# --- 1. Load first 500 rows of each block used in the pipeline ---
blocks_to_check <- list(
  block01 = "LEVEL - 01(Block 1 & item 1503, 1508 of Block 15).sav",
  block02 = "LEVEL - 02(Block 2).sav",
  block07 = "LEVEL - 07 (Block 6).sav",
  block08 = "LEVEL - 08 (Block 7.1 & 7.2).sav",
  block09 = "LEVEL - 09 (Block 8).sav",
  block10 = "LEVEL - 10 (Block 9).sav",
  block11 = "LEVEL - 11 (Block 10).sav"
)

cat("=== DIAGNOSTIC 1: Column Name Audit ===\n\n")

# Columns used to build Unique_ID
id_cols_expected <- c("fsu_serial_no", "segment_no", "second_stage_stratum_no",
                      "second_stage_stratum", "sample_est_no",
                      "sample_establishment_no", "sample_estab_no")

for (bname in names(blocks_to_check)) {
  fpath <- file.path(asuse_dir, blocks_to_check[[bname]])
  d <- read_sav(fpath, n_max = 5) %>% clean_names()
  
  cat(sprintf("--- %s (%s) ---\n", bname, blocks_to_check[[bname]]))
  
  # Show which ID columns exist
  found <- id_cols_expected[id_cols_expected %in% names(d)]
  missing <- id_cols_expected[!id_cols_expected %in% names(d)]
  cat("  ID cols present:", paste(found, collapse = ", "), "\n")
  
  # Show the stratum column name specifically
  stratum_cols <- names(d)[grepl("stratum", names(d), ignore.case = TRUE)]
  cat("  Stratum columns:", paste(stratum_cols, collapse = ", "), "\n")
  
  # Show establishment/sample columns
  est_cols <- names(d)[grepl("sample.*est|est.*no", names(d), ignore.case = TRUE)]
  cat("  Est/Sample cols:", paste(est_cols, collapse = ", "), "\n")
  
  cat("\n")
  rm(d); gc(verbose = FALSE)
}

# --- 2. Test Unique_ID construction match rates ---
cat("\n=== DIAGNOSTIC 2: Unique_ID Join Match Rates ===\n\n")
cat("Loading block02 (base)...\n")
block02 <- read_sav(file.path(asuse_dir, blocks_to_check[["block02"]])) %>% clean_names()

base_ids <- block02 %>%
  mutate(Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum_no, sample_est_no, sep = "_")) %>%
  filter(ownership_type %in% c("1", "2", "3")) %>%  # proprietary only
  pull(Unique_ID) %>% unique()

cat(sprintf("Base (block02 proprietary): %d unique IDs\n\n", length(base_ids)))

# Check each block
check_blocks <- list(
  block01 = list(
    file = blocks_to_check[["block01"]],
    id_expr = quote(paste(fsu_serial_no, segment_no, second_stage_stratum_no, sample_establishment_no, sep = "_"))
  ),
  block08 = list(
    file = blocks_to_check[["block08"]],
    id_expr = quote(paste(fsu_serial_no, segment_no, second_stage_stratum, sample_est_no, sep = "_"))
  ),
  block09 = list(
    file = blocks_to_check[["block09"]],
    id_expr = quote(paste(fsu_serial_no, segment_no, second_stage_stratum, sample_est_no, sep = "_"))
  ),
  block07 = list(
    file = blocks_to_check[["block07"]],
    id_expr = quote(paste(fsu_serial_no, segment_no, second_stage_stratum, sample_est_no, sep = "_"))
  ),
  block11 = list(
    file = blocks_to_check[["block11"]],
    id_expr = quote(paste(fsu_serial_no, segment_no, second_stage_stratum_no, sample_estab_no, sep = "_"))
  ),
  block10 = list(
    file = blocks_to_check[["block10"]],
    id_expr = quote(paste(fsu_serial_no, segment_no, second_stage_stratum, sample_est_no, sep = "_"))
  )
)

for (bname in names(check_blocks)) {
  info <- check_blocks[[bname]]
  cat(sprintf("Checking %s...\n", bname))
  
  tryCatch({
    d <- read_sav(file.path(asuse_dir, info$file)) %>% clean_names()
    d <- d %>% mutate(Unique_ID = !!info$id_expr)
    block_ids <- unique(d$Unique_ID)
    
    matched <- sum(base_ids %in% block_ids)
    pct <- round(matched / length(base_ids) * 100, 1)
    
    cat(sprintf("  Block unique IDs: %d | Matched to base: %d / %d (%.1f%%)\n",
                length(block_ids), matched, length(base_ids), pct))
    
    if (pct < 50) cat("  *** WARNING: Very low match rate! Check ID construction ***\n")
    
    rm(d); gc(verbose = FALSE)
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
  cat("\n")
}

# --- 3. Check for duplicate IDs in base ---
cat("=== DIAGNOSTIC 3: Duplicate Check in block02 ===\n")
block02_ids <- block02 %>%
  mutate(Unique_ID = paste(fsu_serial_no, segment_no, second_stage_stratum_no, sample_est_no, sep = "_")) %>%
  filter(ownership_type %in% c("1", "2", "3"))

n_total <- nrow(block02_ids)
n_unique <- length(unique(block02_ids$Unique_ID))
cat(sprintf("  Total rows: %d | Unique IDs: %d | Duplicates: %d (%.2f%%)\n",
            n_total, n_unique, n_total - n_unique, (n_total - n_unique) / n_total * 100))

# --- 4. Check value distributions ---
cat("\n=== DIAGNOSTIC 4: Key Variable Distributions ===\n")
cat("Ownership type:\n")
print(table(block02$ownership_type, useNA = "ifany"))
cat("\nEstablishment type:\n")
print(table(block02$est_type, useNA = "ifany"))
cat("\nSector:\n")
print(table(block02$sector, useNA = "ifany"))

# --- 5. Check block08 item_no values for GVA ---
cat("\n=== DIAGNOSTIC 5: Block08 GVA Items ===\n")
b08 <- read_sav(file.path(asuse_dir, blocks_to_check[["block08"]]), n_max = 50000) %>% clean_names()
cat("item_no values present:\n")
print(table(b08$item_no, useNA = "ifany"))
cat(sprintf("\nRows with item_no 769 (GVA): %d\n", sum(b08$item_no == "769", na.rm = TRUE)))
cat(sprintf("Rows with item_no 779 (NVA): %d\n", sum(b08$item_no == "779", na.rm = TRUE)))

rm(block02, b08); gc(verbose = FALSE)
cat("\n=== DIAGNOSTICS COMPLETE ===\n")
