# ==============================================================================
# 03_charts_advanced.r
# ASUSE 2023-24 — Advanced Descriptive Visualisations
#
# PURPOSE: Produce survey-weighted advanced charts beyond the seven basic
#          stacked-bar charts in 02_analysis_main.r. All weighted estimates
#          use srvyr / survey; no unweighted proportions are reported.
#
# PREREQUISITES: Run 01_data_pipeline.r first to create:
#   C:/Users/ashwin/Documents/IndiaSpend Article/ASUSE202324sav/
#   Analysis_Ready_Data/asuse_final_gendered_analysis.rds
#
# OUTPUT: charts/advanced/*.png  (all 300 dpi, publication-ready)
# ==============================================================================

# ── 0. Libraries ──────────────────────────────────────────────────────────────
pkgs <- c(
  "tidyverse", "survey", "srvyr",
  "ggridges", # ridge / joy plots
  "ggdist", # half-violin / raindrop
  "corrplot", # correlogram
  "scales",
  "ggrepel",
  "patchwork" # multi-panel layouts
)
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

options(survey.lonely.psu = "adjust")

# ── 1. Load data & build survey design ────────────────────────────────────────
asuse_dir <- "C:\\Users\\ashwin\\Documents\\IndiaSpend Article\\ASUSE202324sav"
data_file <- file.path(
  asuse_dir, "Analysis_Ready_Data",
  "asuse_final_gendered_analysis.rds"
)

if (!file.exists(data_file)) {
  stop("Run 01_data_pipeline.r first — RDS not found:\n  ", data_file)
}

df <- readRDS(data_file) |>
  # Replicate the binary helpers from 02_analysis_main.r
  mutate(
    is_home_based = as.integer(Work_Location == "Home-Based (Invisible)"),
    is_employer = as.integer(Total_Hired_Workers > 0),
    is_registered = as.integer(Is_Registered == "Formal (Registered)"),
    uses_internet = as.integer(Internet_Use == "Yes"),
    has_loan = as.integer(Total_Loans > 0),
    low_education = Owner_Education_Level %in%
      c("Illiterate", "Literate/Below Primary", "Primary"),
    # NIC 2-digit for industry flags
    is_apparel = as.integer(NIC_2digit == "14"),
    is_tobacco = as.integer(NIC_2digit == "12")
  ) |>
  filter(Gender %in% c("Female", "Male")) # exclude Transgender for charts
# (n=16; NaN SEs — flag separately)

svy <- df |>
  filter(!is.na(Weight)) |>
  as_survey_design(ids = PSU, strata = Strata, weights = Weight, nest = TRUE)

# Output directory
out_dir <- file.path(
  "C:\\Users\\ashwin\\Documents\\asuse-gender-analysis\\charts", "advanced"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
save_chart <- function(name, w = 12, h = 8) {
  path <- file.path(out_dir, paste0(name, ".png"))
  ggsave(path, width = w, height = h, dpi = 300, bg = "white")
  cat("  Saved:", path, "\n")
}

# ── Shared design tokens ───────────────────────────────────────────────────────
gender_pal <- c("Female" = "#c0392b", "Male" = "#2980b9")

pub_theme <- theme_minimal(base_size = 15, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0),
    plot.subtitle = element_text(
      size = 13, color = "grey40", hjust = 0,
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 9, color = "grey55",
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#fafafa", color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

cat("\n=== COMPUTING SURVEY-WEIGHTED METRICS ===\n")

# ── 2. Core survey-adjusted proportions (used by multiple charts) ─────────────
core_stats <- svy |>
  group_by(Gender) |>
  summarise(
    home_pct = survey_mean(is_home_based, na.rm = TRUE, vartype = "ci"),
    employer_pct = survey_mean(is_employer, na.rm = TRUE, vartype = "ci"),
    reg_pct = survey_mean(is_registered, na.rm = TRUE, vartype = "ci"),
    internet_pct = survey_mean(uses_internet, na.rm = TRUE, vartype = "ci"),
    loan_pct = survey_mean(has_loan, na.rm = TRUE, vartype = "ci"),
    lowedu_pct = survey_mean(low_education, na.rm = TRUE, vartype = "ci"),
    .groups = "drop"
  )
print(core_stats)

# Manufacturing concentration (survey-weighted, within Manufacturing sector)
mfg_stats <- svy |>
  filter(Broad_Sector == "Manufacturing") |>
  group_by(Gender) |>
  summarise(
    apparel_pct = survey_mean(is_apparel, na.rm = TRUE, vartype = "ci"),
    tobacco_pct = survey_mean(is_tobacco, na.rm = TRUE, vartype = "ci"),
    .groups = "drop"
  )
cat("Manufacturing concentration:\n")
print(mfg_stats)

# ==============================================================================
# CHART A: DUMBBELL — Manufacturing Gender Gap (survey-weighted, FIXED)
# ==============================================================================
cat("\nA. Dumbbell: Manufacturing Gender Gap\n")

dumbbell_df <- mfg_stats |>
  select(Gender, apparel_pct, tobacco_pct) |>
  pivot_longer(
    cols      = c(apparel_pct, tobacco_pct),
    names_to  = "industry",
    values_to = "pct"
  ) |>
  mutate(
    pct = pct * 100,
    industry = recode(industry,
      apparel_pct = "Apparel / Garments (NIC 14)",
      tobacco_pct = "Tobacco / Beedi (NIC 12)"
    )
  )

wide_db <- dumbbell_df |>
  pivot_wider(names_from = Gender, values_from = pct)

p_dumbbell <- ggplot(wide_db) +
  geom_segment(
    aes(y = industry, yend = industry, x = Male, xend = Female),
    color = "grey70", linewidth = 2
  ) +
  geom_point(aes(x = Male, y = industry),
    color = gender_pal["Male"],
    size = 11
  ) +
  geom_point(aes(x = Female, y = industry),
    color = gender_pal["Female"],
    size = 11
  ) +
  geom_text(aes(x = Male, y = industry, label = paste0(round(Male, 1), "%")),
    color = "white", size = 4.2, fontface = "bold"
  ) +
  geom_text(aes(x = Female, y = industry, label = paste0(round(Female, 1), "%")),
    color = "white", size = 4.2, fontface = "bold"
  ) +
  annotate("text",
    x = wide_db$Male[1] + 1, y = 2.35, label = "Men",
    color = gender_pal["Male"], fontface = "bold", size = 4
  ) +
  annotate("text",
    x = wide_db$Female[1] - 1, y = 2.35, label = "Women",
    color = gender_pal["Female"], fontface = "bold", size = 4,
    hjust = 1
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 75), breaks = seq(0, 70, 10)
  ) +
  pub_theme +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 13, face = "bold"),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = "THE PINK GHETTO IN MANUFACTURING",
    subtitle = "Women are hyper-concentrated in apparel & tobacco — industries notorious\nfor home-based piece-rate work and low pay",
    x = "Share of each gender's manufacturing workforce (%)",
    caption = paste0(
      "Source: ASUSE 2023-24 | Survey-weighted estimates (srvyr) | ",
      sprintf(
        "Apparel: Women %.1f%% vs Men %.1f%%  |  ",
        wide_db$Female[2], wide_db$Male[2]
      ),
      sprintf(
        "Tobacco: Women %.1f%% vs Men %.1f%%",
        wide_db$Female[1], wide_db$Male[1]
      )
    )
  )
print(p_dumbbell)
save_chart("A_dumbbell_manufacturing_gender_gap")

# ==============================================================================
# CHART B: LOLLIPOP — Multi-dimension Disadvantage Index
# ==============================================================================
cat("\nB. Lollipop: Multi-dimension Disadvantage\n")

lollipop_df <- core_stats |>
  select(Gender,
    `Home-Based\nWork` = home_pct,
    `No Hired\nWorkers` = employer_pct, # we'll invert this
    `Unregistered\nBusiness` = reg_pct, # invert
    `No Internet\nAccess` = internet_pct, # invert
    `No Formal\nCredit` = loan_pct, # invert
    `Low\nEducation` = lowedu_pct
  ) |>
  pivot_longer(-Gender, names_to = "dimension", values_to = "pct") |>
  mutate(
    pct = case_when(
      dimension %in% c(
        "No Hired\nWorkers", "Unregistered\nBusiness",
        "No Internet\nAccess", "No Formal\nCredit"
      ) ~ 1 - pct,
      TRUE ~ pct
    ),
    pct = pct * 100,
    # Set factor order by female disadvantage (largest gap first)
    dimension = factor(dimension)
  )

# Order dimensions by female-male gap
gap_order <- lollipop_df |>
  pivot_wider(names_from = Gender, values_from = pct) |>
  mutate(gap = Female - Male) |>
  arrange(desc(gap)) |>
  pull(dimension)

lollipop_df <- lollipop_df |>
  mutate(dimension = factor(dimension, levels = gap_order))

p_lollipop <- ggplot(
  lollipop_df,
  aes(x = pct, y = dimension, color = Gender)
) +
  geom_line(aes(group = dimension), color = "grey80", linewidth = 1.5) +
  geom_point(size = 8, alpha = 0.9) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
    color = "white", size = 3.2, fontface = "bold"
  ) +
  scale_color_manual(values = gender_pal) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100)
  ) +
  pub_theme +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title    = "THE DISADVANTAGE PROFILE",
    subtitle = "Six structural disadvantages — each dot pair is one gender gap.\nWomen consistently sit at the more-disadvantaged end.",
    x        = "Percent of enterprises (%)",
    caption  = "Source: ASUSE 2023-24 | Survey-weighted proportions\nNote: 'No Hired Workers', 'Unregistered', 'No Internet', 'No Credit' are inverted (higher = more disadvantaged)"
  )
print(p_lollipop)
save_chart("B_lollipop_disadvantage_profile", w = 13, h = 8)

# ==============================================================================
# CHART C: RIDGE PLOT — Income Distribution by Gender × Home-Based Status
# ==============================================================================
cat("\nC. Ridge plot: Income distribution\n")

# Use micro-data (observations), weighted via after_stat in ggridges
ridge_df <- df |>
  filter(Income > 0, Income <= 1e7) |>
  mutate(
    Group = paste0(
      Gender, "\n(",
      ifelse(is_home_based == 1, "Home-based", "Fixed/outside"), ")"
    ),
    Group = factor(Group, levels = c(
      "Male\n(Fixed/outside)", "Male\n(Home-based)",
      "Female\n(Fixed/outside)", "Female\n(Home-based)"
    ))
  )

p_ridge <- ggplot(ridge_df, aes(
  x = Income / 1000, y = Group,
  fill = Gender, weight = Weight
)) +
  geom_density_ridges(
    alpha = 0.75,
    scale = 1.5,
    rel_min_height = 0.01,
    quantile_lines = TRUE,
    quantiles = 2 # median line
  ) +
  scale_fill_manual(values = gender_pal) +
  scale_x_continuous(
    labels = function(x) paste0("\u20b9", x, "K"),
    breaks = seq(0, 600, 100)
  ) +
  # coord_cartesian zooms the axis AFTER stats are computed — no rows deleted
  coord_cartesian(xlim = c(0, 600)) +
  pub_theme +
  theme(legend.position = "none") +
  labs(
    title    = "THE INCOME CANYON",
    subtitle = "Weighted income distributions reveal home-based work compresses\nwomen into the very bottom of the earnings range",
    x        = "Annual income (\u20b9 thousands, capped at \u20b96 lakh)",
    y        = NULL,
    caption  = "Source: ASUSE 2023-24 | Survey-weighted kernel density | Vertical line = weighted median\nOutliers >\u20b91 crore excluded (< 0.01% of sample)"
  )
print(p_ridge)
save_chart("C_ridge_income_by_gender_location", w = 12, h = 7)

# ==============================================================================
# CHART D: HALF-VIOLIN + DOTS — Asset Distribution by Gender
# ==============================================================================
cat("\nD. Violin + strip: Asset distribution\n")

violin_df <- df |>
  filter(Total_Assets > 0, Total_Assets <= 5e6) |>
  mutate(Assets_K = Total_Assets / 1000)

p_violin <- ggplot(violin_df, aes(
  x = Gender, y = Assets_K,
  fill = Gender, weight = Weight
)) +
  # Half-violin on right side
  ggdist::stat_halfeye(
    aes(fill = Gender),
    adjust = 0.6,
    width = 0.5,
    .width = c(0.5, 0.9), # 50% and 90% intervals
    justification = -0.25,
    point_colour = NA
  ) +
  # Boxplot overlay
  geom_boxplot(
    aes(weight = Weight),
    width = 0.15,
    outlier.shape = NA,
    color = "grey30",
    fill = "white",
    alpha = 0.8
  ) +
  scale_fill_manual(values = gender_pal) +
  scale_y_continuous(
    labels = function(x) paste0("\u20b9", x, "K")
  ) +
  # coord_flip with ylim zooms AFTER stats are computed — no rows deleted
  coord_flip(ylim = c(0, 1500)) +
  pub_theme +
  theme(legend.position = "none") +
  labs(
    title    = "THE ASSET GAP",
    subtitle = "Half-violin shows the full weighted distribution;\nboxplot highlights median and interquartile range",
    x        = NULL,
    y        = "Fixed assets (\u20b9 thousands, capped at \u20b950 lakh)",
    caption  = "Source: ASUSE 2023-24 | Survey-weighted | Shaded bands: 50% and 90% credible intervals"
  )
print(p_violin)
save_chart("D_violin_asset_distribution", w = 11, h = 7)

# ==============================================================================
# CHART E: CORRELOGRAM — Financial & Structural Variables
# ==============================================================================
cat("\nE. Correlogram: financial & structural variables\n")

cor_vars <- df |>
  select(
    Income,
    Total_Assets,
    Total_Workers,
    Female_Hiring_Ratio,
    is_home_based,
    is_registered,
    uses_internet,
    has_loan,
    Is_Female_Owned = Is_Female_Owned
  ) |>
  rename(
    `Annual\nIncome`       = Income,
    `Fixed\nAssets`        = Total_Assets,
    `Total\nWorkers`       = Total_Workers,
    `Female\nHiring %`     = Female_Hiring_Ratio,
    `Home-\nBased`         = is_home_based,
    `Registered`           = is_registered,
    `Internet\nUse`        = uses_internet,
    `Has\nLoan`            = has_loan,
    `Female\nOwned`        = Is_Female_Owned
  ) |>
  mutate(across(everything(), as.numeric))

cor_mat <- cor(cor_vars, use = "pairwise.complete.obs")

png(file.path(out_dir, "E_correlogram_financial_structural.png"),
  width = 2400, height = 2100, res = 200, bg = "white"
)

# ── Layout: focused on top title and large subtitle ──
par(
  oma = c(2, 0, 6, 0), # outer margins: small bottom, generous top
  mar = c(0, 0, 1, 0) # inner margins
)

# ── Core correlogram ──────────────────────────────────────────────────────────
corrplot::corrplot(
  cor_mat,
  method      = "ellipse",
  type        = "lower",
  col         = corrplot::COL2("RdBu", 10),
  tl.col      = "black",
  tl.srt      = 0,
  tl.cex      = 0.95,
  addCoef.col = "grey20",
  number.cex  = 0.75,
  cl.cex      = 0.85,
  mar         = c(0, 0, 1, 0)
)

# ── Title & Large Subtitle ──────────────────────────────────────────────────
mtext(
  "Pairwise Correlations — ASUSE 2023-24 Enterprise Variables",
  side = 3, outer = TRUE, line = 3.8,
  cex = 1.4, font = 2, col = "grey10", adj = 0.04
)
mtext(
  "Each cell = Pearson r correlation. Blue = positive association | Red = negative association | Thinner ellipse = stronger relationship",
  side = 3, outer = TRUE, line = 1.6,
  cex = 1.05, font = 1, col = "grey25", adj = 0.04
)

# ── Data source footnote ─────────────────────────────────────────────────────
mtext(
  "Source: ASUSE 2023-24, Ministry of Statistics & PI | Pairwise complete observations (unweighted)",
  side = 1, outer = TRUE, line = 0.5,
  cex = 0.65, font = 3, col = "grey55", adj = 0.02
)

dev.off()
cat("  Saved:", file.path(out_dir, "E_correlogram_financial_structural.png"), "\n")

# ==============================================================================
# CHART F: STACKED DENSITY — Problem Constraint by Gender
# ==============================================================================
cat("\nF. Density-like: Problem constraints by gender\n")

problem_wt <- svy |>
  filter(Problem_Category != "No Problem/Not Reported") |>
  group_by(Gender, Problem_Category) |>
  summarise(Pop = survey_total(vartype = NULL), .groups = "drop") |>
  group_by(Gender) |>
  mutate(Pct = Pop / sum(Pop) * 100) |>
  ungroup()

female_order <- problem_wt |>
  filter(Gender == "Female") |>
  arrange(Pct) |>
  pull(Problem_Category)

problem_wt <- problem_wt |>
  mutate(Problem_Category = factor(Problem_Category, levels = female_order))

p_problems <- ggplot(
  problem_wt,
  aes(x = Pct, y = Problem_Category, fill = Gender)
) +
  geom_col(
    position = position_dodge(width = 0.7), width = 0.6,
    alpha = 0.88
  ) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")),
    position = position_dodge(width = 0.7),
    hjust = -0.1, size = 3.8, fontface = "bold",
    color = "grey20"
  ) +
  scale_fill_manual(values = gender_pal) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 40),
    expand = expansion(mult = c(0, 0.1))
  ) +
  pub_theme +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title    = "DIFFERENT STRUGGLES",
    subtitle = "Among enterprises reporting a problem, women are far more constrained\nby transport; men face more market-linkage and technology gaps",
    x        = "Share of enterprises with any problem (%)",
    caption  = "Source: ASUSE 2023-24 | Survey-weighted | Excludes 'No Problem / Not Reported' (~80% of all enterprises)"
  )
print(p_problems)
save_chart("F_dodged_problem_constraints", w = 13, h = 7)

# ==============================================================================
# CHART G: PATCHWORK SUMMARY PANEL — 4-in-1 quick overview
# ==============================================================================
cat("\nG. Patchwork 4-in-1 summary panel\n")

make_mini_bar <- function(data, col, title, ylab = "Percent (%)") {
  d <- data |>
    select(Gender, value = all_of(col)) |>
    mutate(value = value * 100)
  ggplot(d, aes(x = Gender, y = value, fill = Gender)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = paste0(round(value, 1), "%")),
      vjust = -0.4, fontface = "bold", size = 5
    ) +
    scale_fill_manual(values = gender_pal) +
    scale_y_continuous(
      limits = c(0, 105),
      labels = function(x) paste0(x, "%")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.title.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(title = title, y = ylab)
}

p_g1 <- make_mini_bar(core_stats, "home_pct", "Home-Based Work")
p_g2 <- make_mini_bar(core_stats, "reg_pct", "Formally Registered")
p_g3 <- make_mini_bar(core_stats, "internet_pct", "Uses Internet")
p_g4 <- make_mini_bar(core_stats, "loan_pct", "Has Formal Loan")

p_panel <- (p_g1 | p_g2 | p_g3 | p_g4) +
  plot_annotation(
    title = "FOUR DIMENSIONS OF GENDER EXCLUSION",
    subtitle = "Survey-adjusted estimates, ASUSE 2023-24 | Female vs Male proprietors",
    caption = "Source: ASUSE 2023-24 | Ministry of Statistics & PI | survey-weighted via srvyr",
    theme = theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0),
      plot.subtitle = element_text(size = 14, color = "grey40", hjust = 0),
      plot.caption = element_text(size = 9, color = "grey55"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )
print(p_panel)
save_chart("G_panel_four_exclusion_dimensions", w = 16, h = 7)

# ==============================================================================
# SUMMARY
# ==============================================================================
cat("\n", strrep("=", 72), "\n")
cat("ADVANCED CHARTS SAVED TO: ", out_dir, "\n")
cat(strrep("=", 72), "\n\n")
cat("A_dumbbell_manufacturing_gender_gap.png\n")
cat("  → Survey-weighted dumbbell; apparel+tobacco concentration by gender\n\n")
cat("B_lollipop_disadvantage_profile.png\n")
cat("  → 6-dimension gender disadvantage lollipop\n\n")
cat("C_ridge_income_by_gender_location.png\n")
cat("  → Weighted ridge / joy-plot of income by gender × home-based\n\n")
cat("D_violin_asset_distribution.png\n")
cat("  → Half-violin + boxplot of asset distribution\n\n")
cat("E_correlogram_financial_structural.png\n")
cat("  → Pairwise correlation matrix (corrplot)\n\n")
cat("F_dodged_problem_constraints.png\n")
cat("  → Survey-weighted dodged bar: business constraints by gender\n\n")
cat("G_panel_four_exclusion_dimensions.png\n")
cat("  → 4-in-1 patchwork summary panel\n\n")
cat("NOTE: Transgender enterprises (n=16, NaN SEs) excluded from all charts.\n")
cat("      Standalone population-count claims require Weight ÷ 100.\n")
