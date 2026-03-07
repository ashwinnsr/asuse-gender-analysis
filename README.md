# Gendered Analysis of Unincorporated Sector Enterprises (ASUSE 2023-24)

This repository contains the R-based analysis and visualizations for a story investigating the unincorporated enterprise sector in India from a gendered perspective. The study utilizes the latest **Annual Survey of Unincorporated Sector Enterprises (ASUSE) 2023-24** data.

## 📊 Key Findings
The analysis highlights significant gender disparities across several dimensions:
- **The Invisible Workshop**: 75.2% of women entrepreneurs work from home (invisible workshops) compared to only 27% of men.
- **The Solopreneur Trap**: Only 3.3% of women entrepreneurs are employers with hired workers; 96.7% work alone.
- **The Paper Ceiling**: 90% of women-owned businesses are unregistered/informal, vs 57% for men.
- **The Sisterhood Effect**: Women owners are 26x more likely to hire other women compared to male owners.
- **The Digital & Credit Divide**: Significant gaps exist in internet usage and access to formal credit.

## 🛠️ Methodology
The analysis uses survey-weighted descriptive statistics to ensure national representativeness.
- **Software**: R (Tidyverse, Survey, Srvyr)
- **Design**: Stratified survey design with clustering (PSUs) and sampling weights.
- **Data Source**: Ministry of Statistics and Programme Implementation (MoSPI) - ASUSE 2023-24.

## 📂 Project Structure
- `scripts/`:
  - `new.r`: Main analysis script generating survey-adjusted metrics and publication-ready charts.
  - `Analysis.r`: Statistical modeling and detailed data validation.
  - `Dataset_Created.r`: Data cleaning, merging blocks, and preparing the "Analysis Ready" dataset.
- `docs/`:
  - `2026-02-19-gender-gaps-asuse.md`: Blog post content with updated relative paths.
  - Auxiliary documentation and column mappings.
- `charts/`: High-resolution PNG visualizations for reporting.
- `data/`: Placeholder for raw and processed datasets (ignored by Git if large).

## 🚀 How to Run
1. Ensure the raw ASUSE `.sav` files are in the expected directory structure.
2. Run `story_flowcopy.r` to generate the processed dataset.
3. Run `new.r` to perform the analysis and generate the 7 "killer charts."

## 📜 Requirements
- R version 4.1+
- Packages: `tidyverse`, `survey`, `srvyr`, `haven`, `scales`, `ggrepel`
