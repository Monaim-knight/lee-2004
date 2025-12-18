# Quick Start Guide

## 🚀 Get Started in 3 Steps

### 1. Initialize Environment (First Time Only)
```r
source("scripts/00_setup.R")
```

### 2. Run Complete Analysis
```r
source("run_all.R")
```

### 3. View Results
- **Tables**: `outputs/tables/all_rd_models_summary.csv`
- **Figures**: `outputs/figures/`
- **Report**: `reports/paper.md`

## 📊 What Gets Generated

### Tables
- `baseline_rd_results.csv` - Baseline RD specifications
- `all_rd_models_summary.csv` - Comprehensive model comparison
- `bandwidth_sensitivity.csv` - Robustness across bandwidths
- `donut_placebo_results.csv` - Placebo and donut RD tests
- `mccrary_test_results.txt` - Density test results
- `local_randomization_results.txt` - Randomization inference

### Figures
- `rd_main_plot.pdf` - Main RD binned scatter plot
- `density_plot.pdf` - Running variable density
- `rdplot_rdrobust.pdf` - rdrobust RD plot
- `bandwidth_sensitivity.pdf` - Bandwidth sensitivity plot
- `placebo_tests.pdf` - Placebo cutoff tests

## 🧪 Run Tests
```r
source("tests/test_rd.R")
```

## 📁 Project Structure
```
lee-2004/
├── run_all.R              # One-command runner
├── scripts/               # Analysis pipeline
├── R/                     # Reusable functions
├── data/                  # Data (raw & processed)
├── outputs/               # Results (tables & figures)
├── reports/               # Documentation
└── tests/                 # Quality checks
```

## ⚡ Common Commands

| Task | Command |
|------|---------|
| Run everything | `source("run_all.R")` |
| Setup only | `source("scripts/00_setup.R")` |
| Download data | `source("scripts/01_download_data.R")` |
| Clean data | `source("scripts/02_clean_data.R")` |
| Baseline analysis | `source("scripts/03_analysis_baseline.R")` |
| Robustness checks | `source("scripts/04_analysis_robustness.R")` |
| Generate figures | `source("scripts/05_figures.R")` |
| Run tests | `source("tests/test_rd.R")` |

## 🔧 Troubleshooting

**Packages won't install?**
- Check R version (needs ≥ 4.1)
- Run `update.packages(ask = FALSE)`

**Data download fails?**
- Check internet connection
- Verify URL in `scripts/01_download_data.R`

**renv issues?**
- Run `renv::restore()` manually
- Or comment out renv lines in setup script

## 📚 More Information

- **Full setup**: See `SETUP.md`
- **Project overview**: See `README.md`
- **Detailed results**: See `reports/paper.md`
- **What changed**: See `CHANGELOG.md`
- **Transformation summary**: See `PROJECT_SUMMARY.md`

---

**Ready to go?** Run `source("run_all.R")` and grab a coffee! ☕


