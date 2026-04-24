# Gendered Analysis of Unincorporated Sector Enterprises — ASUSE 2023-24

> **An IndiaSpend investigation** into the structural barriers facing women
> entrepreneurs in India's vast unincorporated enterprise sector, using the
> latest Annual Survey of Unincorporated Sector Enterprises (ASUSE 2023-24)
> from the Ministry of Statistics and Programme Implementation (MoSPI).

---

## 📊 Key Findings

| Finding | Women | Men |
|---|---|---|
| **Home-based work** | 75.2% | 27.0% |
| **Formally registered** | ~10% | ~43% |
| **Has hired workers** | ~3% | ~16% |
| **Internet access** | ~14% | ~32% |
| **Median annual income** | ₹47,040 | ₹1,57,080 |
| **Female hiring ratio** (among employers) | 96% | 4% |

The analysis reveals four structural patterns:

- **The Invisible Workshop** — three-quarters of women entrepreneurs work from home, outside any regulatory, credit or market linkage system.
- **The Paper Ceiling** — 90% of women-owned businesses are informal/unregistered vs 57% for men.
- **The Solopreneur Trap** — only ~3% of women are employers; 97% work entirely alone.
- **The Sisterhood Effect** — women employers hire almost exclusively other women (96%); male employers hire almost exclusively men.

---

## 🛠️ Methodology

- **Software**: R 4.1+
- **Survey design**: Stratified cluster sampling via `srvyr` / `survey` packages — all estimates are nationally representative population-weighted figures.
- **Design variables**: `PSU` (primary sampling unit = FSU), `Strata` (2nd-stage stratum), `Weight` (MLT multiplier).
- **Lonely PSU handling**: `options(survey.lonely.psu = "adjust")`.
- **Outlier treatment**: Income capped at ₹1 cr, Assets at ₹5 cr, Loans at ₹1 cr before survey design creation.
- **Transgender note**: n = 16 sample observations; variance estimates = NaN. Excluded from all charts; included in tabular checks only with a reliability caveat.
- **Weight scaling**: Raw `sum(Weight)` ≈ 7.3 billion. Ratios between genders are unaffected by this scale. Standalone population-count claims (e.g. "19.9 crore women entrepreneurs") require `sum(Weight) / 100`.

---

## 📂 Project Structure

```
asuse-gender-analysis/
│
├── scripts/
│   ├── 01_data_pipeline.r       ← Load + clean raw SPSS blocks → RDS
│   ├── 02_analysis_main.r       ← Survey metrics + 7 publication charts
│   └── 03_charts_advanced.r     ← Ridge, violin, lollipop, dumbbell, correlogram
│
├── charts/
│   ├── Chart1_...png  …  Chart7_...png   ← from 02_analysis_main.r
│   └── advanced/                          ← from 03_charts_advanced.r
│       ├── A_dumbbell_manufacturing_gender_gap.png
│       ├── B_lollipop_disadvantage_profile.png
│       ├── C_ridge_income_by_gender_location.png
│       ├── D_violin_asset_distribution.png
│       ├── E_correlogram_financial_structural.png
│       ├── F_dodged_problem_constraints.png
│       └── G_panel_four_exclusion_dimensions.png
│
├── data/
│   └── processed/
│       └── chart_data_survey_adjusted.rds
│
└── docs/
    ├── 2026-02-19-gender-gaps-asuse.md   ← Article draft
    ├── block02_columns.txt               ← Column reference
    ├── block08_columns.txt               ← Column reference
    ├── Integrated Analysis.txt
    └── archive/
        └── 2022-23_column_discovery.r    ← Diagnostic script (old round)
```

> **Raw data** (`ASUSE202324sav/*.sav`) lives outside this repo at
> `C:/Users/ashwin/Documents/IndiaSpend Article/ASUSE202324sav/`
> and is not tracked by Git (see `.gitignore`).

---

## 🚀 How to Run

```r
# Step 1 — Build the analysis-ready dataset (~5 min)
source("scripts/01_data_pipeline.r")

# Step 2 — Survey-weighted metrics + 7 charts
source("scripts/02_analysis_main.r")

# Step 3 — Advanced visualisations (ridge, violin, lollipop, correlogram)
source("scripts/03_charts_advanced.r")
```

---

## 📦 R Package Requirements

```r
install.packages(c(
  "haven", "tidyverse", "labelled", "janitor", "skimr",
  "naniar", "assertr", "lubridate", "srvyr", "survey",
  "scales", "ggrepel", "ggridges", "ggdist", "corrplot", "patchwork"
))
```

---

## 📜 Data Source

Ministry of Statistics and Programme Implementation (MoSPI), Government of India.
*Annual Survey of Unincorporated Sector Enterprises (ASUSE) 2023-24.*
[mospi.gov.in](https://mospi.gov.in)
