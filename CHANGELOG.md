# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2024-XX-XX

### Added

- Professional repository structure with modular scripts
- Comprehensive RD analysis pipeline:
  - Baseline specifications (global, centered, interaction, quadratic, windowed, local)
  - Robustness checks (bandwidth sensitivity, placebo tests, donut RD, local randomization)
  - Validity diagnostics (McCrary density test)
- Reusable functions in `R/` directory:
  - `rd_helpers.R`: RD estimation and summary extraction functions
  - `plotting.R`: Publication-quality visualization functions
- Professional documentation:
  - Upgraded README with skills mapping and outcomes
  - Narrative report (`reports/paper.md`) with methods, results, and limitations
- Reproducibility infrastructure:
  - `renv` for package version management
  - One-command runner (`run_all.R`)
  - Data integrity checks with checksum verification
  - Deterministic seeds for reproducibility
- Testing and CI:
  - Sanity checks in `tests/test_rd.R`
  - GitHub Actions CI workflow for automated testing
- Professional polish:
  - MIT License
  - Comprehensive `.gitignore`
  - Organized output directories

### Changed

- Restructured from monolithic scripts to modular pipeline
- Improved code organization and documentation
- Enhanced visualization with consistent themes and captions

### Fixed

- Corrected treatment indicator assignment
- Improved error handling and data validation
- Fixed package namespace issues in plotting functions

---

[1.0.0]: https://github.com/USERNAME/lee-2004/releases/tag/v1.0.0


