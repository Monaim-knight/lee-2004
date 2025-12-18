# Setup Guide

This guide will help you get started with the Lee-Moretti-Butler (2004) RD replication project.

## Initial Setup

### 1. Initialize renv (First Time Only)

If this is your first time running the project, you need to initialize renv:

```r
# Install renv if not already installed
if (!require("renv")) install.packages("renv")

# Initialize renv (creates renv.lock from current packages)
renv::init()

# Restore packages from lock file
renv::restore()
```

**Note**: If `renv.lock` doesn't exist yet, the setup script will create it automatically. You can also run `renv::snapshot()` after installing packages to create/update the lock file.

### 2. Run the Analysis

#### Option A: One-Command Run (Recommended)

```r
source("run_all.R")
```

This executes the entire pipeline:
1. Setup and package installation
2. Data download and verification
3. Data cleaning
4. Baseline analysis
5. Robustness checks
6. Figure generation

#### Option B: Step-by-Step

Run scripts in order:

```r
source("scripts/00_setup.R")
source("scripts/01_download_data.R")
source("scripts/02_clean_data.R")
source("scripts/03_analysis_baseline.R")
source("scripts/04_analysis_robustness.R")
source("scripts/05_figures.R")
```

### 3. View Results

- **Tables**: Check `outputs/tables/` for all result tables
- **Figures**: Check `outputs/figures/` for all plots
- **Report**: Read `reports/paper.md` for narrative summary

### 4. Run Tests

```r
source("tests/test_rd.R")
```

This runs sanity checks on:
- Sample sizes
- Cutoff behavior
- Estimate direction and stability
- Placebo tests
- Data integrity

## Troubleshooting

### Package Installation Issues

If packages fail to install:

1. **Check R version**: Requires R ≥ 4.1
2. **Update packages**: `update.packages(ask = FALSE)`
3. **Install from source**: Some packages may need compilation

### Data Download Issues

If data download fails:

1. **Check internet connection**
2. **Verify URL**: The data URL is in `scripts/01_download_data.R`
3. **Manual download**: You can manually download `lmb-data.dta` and place it in `data/raw/`

### renv Issues

If renv causes problems:

1. **Reset renv**: `renv::deactivate()` then `renv::init()`
2. **Skip renv**: Comment out renv lines in `scripts/00_setup.R` (not recommended for reproducibility)

## Next Steps

- Review `README.md` for project overview
- Read `reports/paper.md` for detailed results
- Check `CHANGELOG.md` for version history
- See `.github/workflows/ci.yml` for CI configuration

## Getting Help

- Check error messages carefully
- Review script comments for explanations
- Ensure all prerequisites are met (R ≥ 4.1, internet connection)


