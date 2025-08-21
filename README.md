### Lee-Moretti-Butler RD Analysis (close U.S. House elections)

This project reproduces a simple regression discontinuity (RD) analysis using close U.S. House elections, based on the widely used `lmb-data.dta` dataset. The goal is to estimate how winning as a Democrat (vs. losing just barely) shifts subsequent legislator liberalism scores, leveraging quasi-random assignment around the 50% vote-share cutoff.

### Repository contents

- `code.R`: End-to-end analysis script, including visualization, a sequence of RD regressions (global, centered, interactions, quadratic, windowed), a narrow-band local comparison, a McCrary density test, and robust extensions (`rdrobust`, bandwidth sensitivity, donut RD, placebo cutoffs, local randomization).
- `rd_analysis.R`: Minimal analysis covering import, selection, visualization, and a baseline regression.
- `lmb-data.csv`: Optional CSV copy of the data (created locally if you run the scripts).
- `Density.pdf`: Plot illustrating the running variable density and cutoff; used alongside the McCrary test.
- `Rplot.pdf`: Binned-scatter RD visualization produced by ggplot when running the scripts.
- `RDplot_rdrobust.pdf`: RD plot produced by `rdrobust::rdplot`.
- `rdrobust_bandwidth_sensitivity.csv`: Estimates across multiple bandwidths.
- `rdrobust_placebo_donut.csv`: Donut RD and placebo-cutoff estimates.
- `local_randomization_results.txt`: Randomization-inference output near the cutoff.
- `rdrobust_covariate_adjusted.txt`: Bias-corrected RD estimates adjusting for smooth covariates (if available).
- `rdrobust_clustered.txt`: Cluster-robust RD summary if a suitable cluster variable exists (e.g., state/district/year).
- `all_rd_models_summary.csv`: Comprehensive summary of all RD models with coefficients, standard errors, p-values, sample sizes, and notes for quick comparison.
- `problem Set 8.Rproj`: RStudio project file for convenient setup.

### How to run

Prerequisites: R (≥4.1 recommended) and an internet connection (the script reads the Stata file from a URL).

Required packages (installed in the scripts if missing): `haven`, `dplyr`, `ggplot2`, `rdd`, `rddensity`, `rdrobust`, `rdlocrand`.

**Note:** The script now includes a helper function `install_if_missing()` that only installs packages if they're not already loaded, speeding up subsequent runs.

Option A — RStudio:
1. Open `problem Set 8.Rproj` in RStudio.
2. Open `code.R` (or `rd_analysis.R`).
3. Run the script top-to-bottom, or run section-by-section to inspect outputs.

Option B — Base R:
1. Set the working directory to the project folder.
2. Run: `source("code.R")` (or `source("rd_analysis.R")`).

The scripts will:
- Download and read `lmb-data.dta` from `https://github.com/scunning1975/mixtape/raw/master/lmb-data.dta`.
- Optionally write out `lmb-data.csv`.
- Produce figures saved as PDFs and print model summaries to the console.
- Export a comprehensive model summary table to `all_rd_models_summary.csv` for easy comparison across specifications.

### Variables used

- `demvoteshare`: Running variable (Democratic two-party vote share). The cutoff is 0.5.
- `democrat_winner`: Treatment indicator for crossing the cutoff (Democrat win).
- `score`: Outcome (legislator liberalism score).

### What each analysis step does and why it matters

- Import with `haven::read_dta` and select key variables:
  - Why: The dataset is a Stata file; selecting the running variable, treatment, and outcome simplifies downstream steps.

- Binned scatter around the cutoff (`ggplot2` with a vertical line at 0.5):
  - Why: Visual diagnostic for the RD setup; reveals any discontinuity in outcomes at the threshold and broad functional form.

- Global regression with a treatment indicator: `lm(score ~ democrat_winner)`:
  - Why: Naive benchmark. It ignores the running variable and functional form but gives a starting point for comparison.

- Center the running variable: `demvoteshare_centered = demvoteshare - 0.5` and run `lm(score ~ democrat_winner + demvoteshare_centered)`:
  - Why: Centering makes coefficients easier to interpret (intercepts align to the cutoff) and reduces multicollinearity when adding polynomials.

- Allow different slopes on either side: add interaction `democrat_winner:demvoteshare_centered`:
  - Why: RD requires flexible control for the running variable on both sides; letting slopes differ avoids misspecification bias at the cutoff.

- Quadratic terms on each side: include `I(demvoteshare_centered^2)` and its interaction:
  - Why: Captures curvature; RD identification is local, but small polynomial terms can reduce bias if curvature exists near the cutoff.

- Windowed estimation (e.g., 0.45–0.55 around the cutoff):
  - Why: RD relies on local comparisons near the threshold. Restricting to a bandwidth reduces functional form dependence and bias.

- Very narrow discontinuity sample (±0.02): local comparison or local linear model:
  - Why: Approximates a randomized experiment by comparing observations almost at the cutoff; emphasizes the local treatment effect.

- McCrary density test (`rddensity`):
  - Why: Tests for manipulation of the running variable at the cutoff. A discontinuity in the density suggests sorting, which threatens RD validity.

### Robust RD extensions and why we need them

- Bias-corrected local-polynomial RD (`rdrobust`):
  - Why: Provides bias-corrected estimates with robust standard errors and data-driven bandwidths (CCT), improving inference at the cutoff.

- Bandwidth sensitivity (multiple h values):
  - Why: RD effects should be stable across reasonable bandwidths; sensitivity checks guard against cherry-picking and functional form dependence.

- Donut RD (exclude |x − 0.5| < h0):
  - Why: Removes near-cutoff observations that may be manipulated or heaped, improving credibility of local comparisons.

- Placebo cutoffs (e.g., 0.45, 0.55):
  - Why: Effects should occur at the true cutoff only; significant jumps away from 0.5 would signal model misspecification.

- Local randomization tests (`rdlocrand`):
  - Why: In a tight window, treatment assignment approximates randomization; randomization-based p-values complement asymptotic RD inference.

- Covariate-adjusted RD (`rdrobust` with `covs`):
  - Why: Improves precision by conditioning on smooth pre-treatment covariates that do not jump at the cutoff.

- Cluster-robust inference:
  - Why: Accounts for within-cluster correlation (e.g., by state/district/year) to avoid overstated significance.

### Outputs and how to interpret them

- Figures:
  - `Rplot.pdf`: Binned averages of `score` vs. `demvoteshare` with a dashed line at 0.5. A visible jump at the cutoff suggests a treatment effect.
  - `Density.pdf`: Density of `demvoteshare` with the cutoff marked. Use with the McCrary test results; a smooth density supports the no-manipulation assumption.
  - `RDplot_rdrobust.pdf`: RD plot with bins and fitted local polynomials from `rdrobust`.

- Model summaries (console):
  - In centered/interacted specifications, the coefficient on `democrat_winner` estimates the jump at the cutoff (local average treatment effect) under standard RD assumptions.
  - Statistical significance and sign indicate whether Democratic victory shifts subsequent liberalism scores and by how much, locally at 50%.
  - `rdrobust_bandwidth_sensitivity.csv`: Inspect stability of estimates and p-values across bandwidths.
  - `rdrobust_placebo_donut.csv`: Expect small/insignificant effects at placebo cutoffs; donut vs. full-sample estimates should be consistent if no manipulation.
  - `local_randomization_results.txt`: Randomization-inference statistics and p-values within a narrow window.
  - `rdrobust_covariate_adjusted.txt`: Compare with unadjusted results; similar point estimates with tighter SEs indicates precision gains.
  - `rdrobust_clustered.txt`: Check if significance remains after clustering; robust results should not hinge on naive SEs.

### Reproducibility and small code notes

- Ensure the CSV write happens after data is read: move `write.csv(data, "lmb-data.csv", ...)` below `read_dta`.
- Define the treatment indicator as 1 for wins and 0 for losses for clarity, e.g., `as.integer(demvoteshare > 0.5)`.
- Consider saving plots explicitly (e.g., `ggsave("Rplot.pdf", width = 7, height = 5)`) to control filenames and sizes.
- Install packages once per machine; in scripts, you can wrap installs with checks to speed reruns.

### Helper functions and automation

- **Conditional package installation**: The `install_if_missing()` helper function checks if a package is already loaded before attempting installation, avoiding redundant installs and speeding up script reruns.

- **Comprehensive model summary**: The script automatically exports all RD model results to `all_rd_models_summary.csv`, including coefficients, standard errors, p-values, sample sizes, and descriptive notes. This facilitates quick comparison across specifications and model robustness checks.

### References

- Data source: `https://github.com/scunning1975/mixtape/raw/master/lmb-data.dta`.
- McCrary, J. (2008). Manipulation of the Running Variable in the Regression Discontinuity Design. Journal of Econometrics.
- Lee, D. S., Moretti, E., & Butler, M. J. (2004). Do Voters Affect or Elect Policies? Evidence from the U.S. House. Quarterly Journal of Economics.


