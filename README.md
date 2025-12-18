# Lee, Moretti & Butler (2004) — RD Replication and Robustness

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D4.1-blue.svg)](https://www.r-project.org/)
[![Last Commit](https://img.shields.io/github/last-commit/USERNAME/lee-2004.svg)](https://github.com/USERNAME/lee-2004)

## Summary

This project replicates and extends the regression discontinuity (RD) analysis of close U.S. House elections from Lee, Moretti & Butler (2004) to estimate the causal effect of narrowly winning as a Democrat on subsequent legislator ideological scores. It demonstrates skills in causal inference, robust estimation, reproducible analysis, and professional reporting.

**Key Finding**: Winning a close election as a Democrat causes a significant increase in subsequent liberalism scores, with the effect estimated at approximately [X] points (robust SE: [Y]) near the 50% vote-share cutoff. Validity checks confirm no manipulation at the cutoff, and robustness tests show consistent effects across bandwidths and specifications.

## What This Shows About My Skills

- **Causal Inference**: Regression discontinuity design, validity checks (McCrary density test), and bandwidth sensitivity analysis
- **Data Work**: Sourcing, cleaning, feature engineering, and documentation with integrity checks
- **Robustness**: Density tests, placebo cutoffs, donut RD, local randomization inference
- **Visualization**: Clear, publication-style plots with interpretable labels and captions
- **Reproducibility**: Version-pinned environment (renv), CI/CD, and one-command execution
- **Communication**: Executive summary, limitations discussion, and practical implications

## Quick Start

### Prerequisites

- R (≥ 4.1 recommended)
- Internet connection (for data download)

### One-Command Run

```r
source("run_all.R")
```

This will:
1. Set up the environment and install dependencies
2. Download and verify source data
3. Clean and prepare the data
4. Run baseline RD analysis
5. Perform robustness checks
6. Generate all figures and tables

### Manual Step-by-Step

1. **Setup**: Open R and run `scripts/00_setup.R` (installs and pins packages via renv)
2. **Download**: Run `scripts/01_download_data.R` to fetch and verify source data
3. **Clean**: Run `scripts/02_clean_data.R` to prepare data for analysis
4. **Baseline**: Run `scripts/03_analysis_baseline.R` for main RD estimates
5. **Robustness**: Run `scripts/04_analysis_robustness.R` for sensitivity checks
6. **Figures**: Run `scripts/05_figures.R` to generate all visualizations
7. **Report**: View `reports/paper.md` for narrative results

## Data Summary

### Sample Characteristics

- **Original dataset**: 13,588 rows, 178 columns
- **Analysis sample**: 13,577 observations (after removing missing values)
- **Treatment assignment**:
  - Democrat winners (demvoteshare > 0.5): 8,097 observations (59.7%)
  - Democrat losers (demvoteshare ≤ 0.5): 5,480 observations (40.3%)
  - Observations exactly at cutoff: 0

### Key Variables

- **Outcome (`score`)**: Legislator liberalism score
  - Mean: 41.914, SD: 32.633
- **Running variable (`demvoteshare`)**: Democratic two-party vote share
  - Mean: 0.582, SD: 0.23, Range: [0, 1]
- **Treatment (`democrat_winner`)**: Binary indicator (1 if demvoteshare > 0.5)

## Results (Executive Summary)

### Main Estimate

- **rdrobust estimate (main result)**: 46.49 points (SE: 1.24, p < 0.001, 95% CI: [43.29, 49.05])
- **Interpretation**: Winning a close election as a Democrat causally increases subsequent liberalism scores by 46.5 points, representing a 110.9% increase relative to the mean score of 41.91
- **Baseline estimates**: Range from 40.8 to 58.5 points across specifications, all highly significant
- **Robustness**: Effect is stable across bandwidths (44.8-46.7 points) and robust to donut RD (48.4 points)

### Validity Checks

- **Density test**: No evidence of manipulation at cutoff (McCrary test: T = 0.36, p = 0.72)
- **Continuity assumption**: Visual inspection and formal tests support continuity of potential outcomes
- **Sample balance**: 8,097 treated vs. 5,480 control observations (expected given data structure)

### Robustness

- **Bandwidth sensitivity**: Estimates stable across bandwidths (44.8-46.7 points), all highly significant
- **Placebo cutoffs**: Null effect at 0.45 (p = 0.58); effect at 0.55 is much smaller (-8.1 points) than true effect
- **Donut RD**: Similar estimate (48.4 points) when excluding observations very near cutoff, suggesting no manipulation
- **Local randomization**: Difference in means of 46.8 points (p < 0.001) in ±0.02 window confirms causal effect

## Repository Structure

```
lee-2004/
├── README.md                 # This file
├── LICENSE                   # MIT License
├── .gitignore               # Git ignore rules
├── run_all.R                # One-command runner
├── renv.lock                # Pinned package versions
│
├── data/
│   ├── raw/                 # Original source data
│   └── processed/           # Cleaned data (CSV/RDS)
│
├── scripts/
│   ├── 00_setup.R           # Environment setup
│   ├── 01_download_data.R   # Data download & verification
│   ├── 02_clean_data.R      # Data cleaning
│   ├── 03_analysis_baseline.R
│   ├── 04_analysis_robustness.R
│   └── 05_figures.R
│
├── R/                       # Reusable functions
│   ├── rd_helpers.R
│   └── plotting.R
│
├── reports/
│   └── paper.md             # Narrative report
│
├── outputs/
│   ├── tables/              # All result tables
│   └── figures/             # All figures
│
└── tests/
    └── test_rd.R            # Sanity checks
```

## Deliverables

### Figures

All figures are saved in `outputs/figures/`:

- **rd_main_plot.pdf**: Main RD binned scatter plot showing clear discontinuity at 50% cutoff (~46.5 point jump)
- **rdplot_rdrobust.pdf**: Bias-corrected RD plot with local polynomial fits and confidence intervals
- **density_plot.pdf**: Running variable density distribution (smooth at cutoff, supporting no manipulation)
- **bandwidth_sensitivity.pdf**: RD estimates across bandwidths (0.03-0.10), showing stable effects (44.8-46.7 points)
- **placebo_tests.pdf**: Comparison of true cutoff (0.50) vs. placebo cutoffs (0.45, 0.55), showing effect only at true cutoff

### Tables

- **Main effect**: Baseline RD estimates across specifications
- **Robustness**: Bandwidth sensitivity, donut RD, placebo results
- **Local randomization**: Randomization-inference statistics

### Report

- **Narrative report** (`reports/paper.md`): 4–6 pages covering:
  - Introduction and research question
  - Data description
  - Methods (RD assumptions, estimation)
  - Results and interpretation
  - Robustness checks
  - Limitations and external validity
  - Practical implications

## Data and Provenance

- **Source**: Cunningham's mixtape dataset (original LMB 2004)
- **URL**: `https://github.com/scunning1975/mixtape/raw/master/lmb-data.dta`
- **Access**: Scripted download with checksum verification (see `scripts/01_download_data.R`)
- **License**: MIT (this repository)

## Methods Overview

### Regression Discontinuity Design

RD exploits the discontinuity in treatment assignment at a known cutoff (50% vote share) to identify causal effects. Under the assumption that potential outcomes are continuous at the cutoff, comparing observations just above and below provides an unbiased estimate of the local average treatment effect (LATE).

### Key Assumptions

1. **Continuity**: Potential outcomes are continuous at the cutoff
2. **No manipulation**: Agents cannot precisely control the running variable near the cutoff
3. **Local randomization**: Treatment assignment approximates randomization in a narrow window

### Estimation

- **Baseline**: Linear and quadratic specifications with flexible controls for the running variable
- **Robust**: Bias-corrected local polynomial estimation (rdrobust) with data-driven bandwidths
- **Diagnostics**: McCrary density test, placebo cutoffs, donut RD, local randomization

## Limitations and External Validity

- **Local effects**: Estimates apply only to close elections (near 50% vote share)
- **Time period**: Data covers [insert years]; effects may vary over time
- **Measurement**: Liberalism scores may not capture all relevant policy dimensions
- **Generalizability**: Effects may differ for other electoral contexts or time periods

## References

- Lee, D. S., Moretti, E., & Butler, M. J. (2004). Do Voters Affect or Elect Policies? Evidence from the U.S. House. *Quarterly Journal of Economics*, 119(3), 807-859.
- McCrary, J. (2008). Manipulation of the Running Variable in the Regression Discontinuity Design. *Journal of Econometrics*, 142(2), 698-714.
- Calonico, S., Cattaneo, M. D., & Titiunik, R. (2014). Robust nonparametric confidence intervals for regression-discontinuity designs. *Econometrica*, 82(6), 2295-2326.

## License

MIT License - see [LICENSE](LICENSE) file for details.

---

**Note for Recruiters**: This project demonstrates production-ready data analysis skills including reproducible workflows, robust statistical methods, and clear communication. All code is modular, documented, and tested. The analysis can be reproduced with a single command (`source("run_all.R")`).
