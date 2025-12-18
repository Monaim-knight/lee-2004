# Step-by-Step RStudio Guide

Follow these steps in RStudio to run the analysis and collect results for the report.

## Prerequisites

- R (≥ 4.1) installed
- RStudio installed
- Internet connection (for data download)

---

## Step 0: Fix Working Directory (IMPORTANT - DO THIS FIRST!)

### Code to Run:
```r
# FIRST: Set the working directory correctly
# Run this script to automatically fix it:
source("FIX_WORKING_DIRECTORY.R")

# OR manually set it (adjust path to your actual location):
setwd("C:/Users/monai/OneDrive - student.uni-halle.de/Desktop/Repair/lee-2004")

# OR in RStudio menu:
# Session > Set Working Directory > To Source File Location
# (Make sure you have a file from this project open in RStudio)

# OR open the RStudio project:
# File > Open Project > Navigate to lee-2004 folder > Open "problem Set 8.Rproj"
```

### What to Check:
- ✅ Script says "SUCCESS! Working directory is correct"
- ✅ All checks show ✓ (not ✗)
- ✅ Current working directory shows the correct path

### If It Fails:
- **Option 1**: In RStudio, go to `File > Open Project` and open `problem Set 8.Rproj`
- **Option 2**: Manually set: `setwd("your/full/path/to/lee-2004")`
- **Option 3**: Use RStudio menu: `Session > Set Working Directory > Choose Directory...`

---

## Step 1: Initialize renv (First Time Only)

### Code to Run:
```r
# If you see "renv is out-of-sync" message, run this first:
source("INITIALIZE_RENV.R")
```

### What to Check:
- ✅ renv initializes successfully
- ✅ All packages install
- ✅ renv.lock file is created
- ✅ Message says "renv SETUP COMPLETE!"

### Expected Output:
```
INITIALIZING renv FOR THIS PROJECT
Initializing renv...
✓ renv initialized!
Installing required packages...
✓ haven already installed
✓ dplyr already installed
...
✓ renv SETUP COMPLETE!
```

### Notes:
- This step only needs to be done ONCE
- If packages fail to install, note which ones and we'll fix them
- This may take 5-10 minutes the first time

---

## Step 2: Setup Environment

### Code to Run:
```r
# Verify you're in the right place first
getwd()  # Should show path ending in "lee-2004"
list.files()  # Should show README.md, scripts/, etc.

# Now run setup script
source("scripts/00_setup.R")
```

### What to Check:
- ✅ Packages load without errors
- ✅ Message says "Setup complete!"
- ✅ No renv sync warnings

### Expected Output:
```
Installing and loading packages...
Setup complete!
Packages loaded: haven, dplyr, ggplot2, tidyr, digest, rdd, rddensity, rdrobust, rdlocrand
```

### Notes:
- First run may take a few minutes to install packages
- If a package fails, note which one and we'll fix it

---

## Step 3: Download Data

### Code to Run:
```r
source("scripts/01_download_data.R")
```

### What to Check:
- ✅ Data downloads successfully
- ✅ File appears in `data/raw/lmb-data.dta`
- ✅ File size is reasonable (should be several hundred KB)

### Expected Output:
```
Downloading data from: https://github.com/scunning1975/mixtape/raw/master/lmb-data.dta
Data downloaded to: data/raw/lmb-data.dta
Data file size: [XXX] KB
Download date: [timestamp]
File MD5 checksum: [hash]
```

### Record This:
- Download date: _______________
- File size: _______________ KB
- Checksum: _______________

---

## Step 4: Clean Data

### Code to Run:
```r
source("scripts/02_clean_data.R")
```

### What to Check:
- ✅ Data reads successfully
- ✅ Missing values are removed
- ✅ Treatment indicator is created correctly

### Expected Output:
```
Reading data from: data/raw/lmb-data.dta
Original data dimensions: [N] rows, [M] columns
After removing missing values: [N] rows

Treatment assignment summary:
Democrat winners (demvoteshare > 0.5): [X]
Democrat losers (demvoteshare <= 0.5): [Y]
Observations exactly at cutoff: [Z]

Summary statistics:
Score - Mean: [X], SD: [Y]
Demvoteshare - Mean: [X], SD: [Y]
Range: [[min], [max]]
```

### Record This:
- Total observations: _______________
- Democrat winners: _______________
- Democrat losers: _______________
- Mean score: _______________
- Mean vote share: _______________

---

## Step 5: Baseline Analysis

### Code to Run:
```r
source("scripts/03_analysis_baseline.R")
```

### What to Check:
- ✅ All regressions run successfully
- ✅ Results table is created
- ✅ McCrary test completes

### Expected Output:
```
======================================================================
BASELINE RD ANALYSIS
======================================================================

1. Running baseline RD regressions...
  - Global regression complete
  - Centered RD regression complete
  - Interaction RD regression complete
  - Quadratic RD regression complete
  - Windowed RD regression complete
  - Local narrow window regression complete

2. Baseline results saved to: outputs/tables/baseline_rd_results.csv
```

### Important: View the Results Table

```r
# After running the script, view the results:
baseline_results <- read.csv("outputs/tables/baseline_rd_results.csv")
print(baseline_results)
View(baseline_results)  # Opens in RStudio viewer
```

### Record This for Each Model:

| Model | Coefficient | SE | P-value | N | Notes |
|-------|-------------|----|---------|----|----|
| global | _____ | _____ | _____ | _____ | _____ |
| centered | _____ | _____ | _____ | _____ | _____ |
| interaction | _____ | _____ | _____ | _____ | _____ |
| quadratic | _____ | _____ | _____ | _____ | _____ |
| windowed_quadratic | _____ | _____ | _____ | _____ | _____ |
| local_narrow | _____ | _____ | _____ | _____ | _____ |

### McCrary Test Results:

```r
# View McCrary test results
mccrary_test  # Should be in your environment after running script
summary(mccrary_test)
```

### Record This:
- McCrary test statistic: _______________
- P-value: _______________
- Interpretation: _______________ (significant = manipulation, not significant = no manipulation)

---

## Step 6: Robustness Analysis

### Code to Run:
```r
source("scripts/04_analysis_robustness.R")
```

### What to Check:
- ✅ rdrobust estimation completes
- ✅ Bandwidth sensitivity analysis runs
- ✅ Placebo tests complete
- ✅ Local randomization test runs

### Expected Output:
```
======================================================================
ROBUSTNESS ANALYSIS
======================================================================

1. Running bias-corrected RD estimation (rdrobust)...
  - rdrobust estimation complete

2. Running bandwidth sensitivity analysis...
  - Bandwidth sensitivity results saved

3. Running donut RD...
  - Donut RD complete

4. Running placebo cutoff tests...
  - Placebo tests complete

5. Running local randomization test...
  - Local randomization test complete
```

### Important: View All Results

```r
# View comprehensive summary
all_results <- read.csv("outputs/tables/all_rd_models_summary.csv")
print(all_results)
View(all_results)

# View bandwidth sensitivity
bw_results <- read.csv("outputs/tables/bandwidth_sensitivity.csv")
print(bw_results)
View(bw_results)

# View placebo and donut results
donut_placebo <- read.csv("outputs/tables/donut_placebo_results.csv")
print(donut_placebo)
View(donut_placebo)

# View rdrobust main result
rd_est  # Should be in environment
summary(rd_est)
```

### Record This:

**Main rdrobust Estimate:**
- Coefficient: _______________
- Robust SE: _______________
- P-value: _______________
- N observations: _______________

**Bandwidth Sensitivity:**

| Bandwidth | Coefficient | SE | P-value | N |
|-----------|-------------|----|---------|----|
| 0.03 | _____ | _____ | _____ | _____ |
| 0.05 | _____ | _____ | _____ | _____ |
| 0.08 | _____ | _____ | _____ | _____ |
| 0.10 | _____ | _____ | _____ | _____ |

**Placebo Tests:**

| Cutoff | Coefficient | P-value | Interpretation |
|--------|-------------|---------|----------------|
| 0.45 | _____ | _____ | Should be null |
| 0.50 (true) | _____ | _____ | Should be significant |
| 0.55 | _____ | _____ | Should be null |

**Donut RD:**
- Coefficient: _______________
- SE: _______________
- P-value: _______________
- N: _______________

**Local Randomization:**
- P-value: _______________
- Interpretation: _______________

---

## Step 7: Generate Figures

### Code to Run:
```r
source("scripts/05_figures.R")
```

### What to Check:
- ✅ All figures are created
- ✅ PDFs appear in `outputs/figures/`

### Expected Output:
```
======================================================================
GENERATING FIGURES
======================================================================

1. Creating main RD plot...
2. Creating density plot...
3. Creating rdrobust RD plot...
4. Creating bandwidth sensitivity plot...
5. Creating placebo test plot...

All figures generated!
Output directory: outputs/figures
```

### Verify Figures:

```r
# List generated figures
list.files("outputs/figures", pattern = "\\.pdf$")

# Open figures to verify they look good
# In RStudio: File > Open File > outputs/figures/rd_main_plot.pdf
```

### Record This:
- ✅ Main RD plot created
- ✅ Density plot created
- ✅ rdrobust plot created
- ✅ Bandwidth sensitivity plot created
- ✅ Placebo test plot created

---

## Step 8: Run Tests

### Code to Run:
```r
source("tests/test_rd.R")
```

### What to Check:
- ✅ All sanity checks pass
- ✅ No critical errors

### Expected Output:
```
======================================================================
RUNNING SANITY CHECKS
======================================================================

1. Sample size checks...
2. Cutoff behavior checks...
3. Estimate direction and stability checks...
4. Placebo test checks...
5. Data integrity checks...

Tests passed: [X] / [Y]
All checks passed! ✓
```

### Record This:
- Tests passed: _____ / _____
- Any warnings? _______________

---

## Step 9: Collect All Results

### Code to Run:
```r
# Load all result tables
baseline <- read.csv("outputs/tables/baseline_rd_results.csv")
all_models <- read.csv("outputs/tables/all_rd_models_summary.csv")
bandwidth <- read.csv("outputs/tables/bandwidth_sensitivity.csv")
placebo_donut <- read.csv("outputs/tables/donut_placebo_results.csv")

# Print summary
cat("\n=== MAIN RD ESTIMATE (rdrobust) ===\n")
print(all_models[all_models$model == "rdrobust", ])

cat("\n=== BANDWIDTH SENSITIVITY ===\n")
print(bandwidth)

cat("\n=== PLACEBO TESTS ===\n")
print(placebo_donut[grepl("placebo", placebo_donut$model), ])

cat("\n=== MCCRARY TEST ===\n")
# mccrary_test should be in environment
if(exists("mccrary_test")) {
  print(summary(mccrary_test))
}
```

### Final Summary Table

Create this summary for the report:

```r
# Create executive summary
main_estimate <- all_models[all_models$model == "rdrobust", ]
cat("\n=== EXECUTIVE SUMMARY ===\n")
cat("Main RD Estimate: ", round(main_estimate$coef, 3), "\n")
cat("Robust SE: ", round(main_estimate$se, 3), "\n")
cat("P-value: ", round(main_estimate$p_value, 4), "\n")
cat("N observations: ", main_estimate$n_obs, "\n")
cat("Interpretation: Winning as Democrat increases liberalism score by ", 
    round(main_estimate$coef, 3), " points\n")
```

---

## Step 10: Fill in the Report

Now you have all the results! Update these files:

1. **reports/paper.md** - Replace all `[X]`, `[Y]`, `[Z]` placeholders with actual values
2. **README.md** - Update the "Results (Executive Summary)" section

### Quick Find & Replace Guide:

In `reports/paper.md`, search for and replace:
- `[X]` → Your coefficient estimate
- `[Y]` → Your standard error
- `[Z]` → Your p-value
- `[N]` → Your sample size
- `[years]` → The time period of your data
- `[%]` → Percentage change calculation

### Example Calculation for Percentage Change:

```r
# Calculate percentage change
mean_score <- mean(data$score, na.rm = TRUE)
effect_size <- main_estimate$coef
percent_change <- (effect_size / mean_score) * 100

cat("Mean liberalism score: ", round(mean_score, 2), "\n")
cat("Effect size: ", round(effect_size, 3), "\n")
cat("Percentage change: ", round(percent_change, 1), "%\n")
```

---

## Troubleshooting

### If a script fails:

1. **Check error message** - Read it carefully
2. **Check previous steps** - Make sure earlier scripts ran successfully
3. **Check data** - Verify data files exist in correct locations
4. **Check packages** - Ensure all packages installed correctly

### Common Issues:

**"Object not found" error:**
- Make sure you ran scripts in order (1, 2, 3, 4, 5)
- Some scripts depend on objects created in previous scripts

**Package installation fails:**
- Try: `install.packages("package_name", dependencies = TRUE)`
- Check R version: `R.version.string` (needs ≥ 4.1)

**Data download fails:**
- Check internet connection
- Try downloading manually and placing in `data/raw/`

---

## Next Steps After Running

1. ✅ All scripts run successfully
2. ✅ All results collected
3. ✅ Report filled in with actual values
4. ✅ README updated with results
5. ✅ Figures verified and look good
6. ✅ Tests pass

Then you're ready to commit and push to GitHub!

---

**Need help?** Check the error message and let me know what step failed.


