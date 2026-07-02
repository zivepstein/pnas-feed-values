# PNAS Feed Values

Replication code and data for analyzing how Schwartz human values relate to social-media content amplification and engagement. The repository contains R scripts that reproduce paper figures and run the underlying statistical models.

## Repository structure

```
pnas-feed-values/
├── fig_1.R                          # Figure 1: cluster-level bar chart + value radar chart
├── fig_2.R                          # Figure 2: alignment across the social media funnel
├── amplification_inventory.R        # Mixed-effects models for value amplification
├── R/
│   └── utils.R                      # Shared helpers (paths, SE, alignment)
└── data/
    ├── amplification_data.csv                    # Aggregated amplification coefficients (29 rows)
    ├── amplification_data_disaggregated.csv      # Tweet-level data (~2.95M rows, ~2.2 GB)
    ├── global_engagement_results.csv             # Platform-wide engagement metrics by value
    ├── individual_engagement_results.csv         # Individual-level engagement coefficients
    └── postfeed_clean.csv                        # Cleaned survey responses with value inventories
```

## Requirements

R (≥ 4.0 recommended) and the following packages:

| Script | Packages |
|--------|----------|
| `fig_1.R` | `fmsb` (base R otherwise) |
| `fig_2.R` | `tidyverse`, `ggplot2` |
| `amplification_inventory.R` | `parameters`, `lme4`, `car`, `multcomp`, `tidyverse` |

Install dependencies with:

```r
install.packages(c("fmsb", "tidyverse", "ggplot2", "parameters", "lme4", "car", "multcomp"))
```

**Note:** `amplification_inventory.R` loads `amplification_data_disaggregated.csv` (~2.2 GB). Ensure sufficient RAM and disk space before running.

## Usage

Run scripts from the repository root (or via `Rscript` — paths are resolved automatically from the script location):

```bash
cd pnas-feed-values
Rscript fig_1.R
Rscript fig_2.R
Rscript amplification_inventory.R
```

Shared helpers live in `R/utils.R` (`data_path()`, `se()`, `spearman_alignment()`, etc.) and are sourced automatically by each script.

### Outputs

| Script | Output files |
|--------|--------------|
| `fig_1.R` | `fig1a.pdf`, `fig1b.pdf` |
| `fig_2.R` | `fig2_left.pdf`, `fig_right_individual.pdf`, `fig2_right_global.pdf` |
| `amplification_inventory.R` | Console output (model summaries, hypothesis tests) |

## Data overview

### `amplification_data.csv`

Aggregated results for 19 Schwartz values plus higher-order clusters (Self-Transcendence, Conservation, etc.) and PCA components. Key columns:

- `base_rate` — baseline amplification rate per value
- `coefficient_amplification`, `se_amplification` — marginal amplification effects
- `ampcoef_pca` — PCA-based amplification coefficients

### `postfeed_clean.csv`

Survey data from participants who completed a Schwartz value inventory and social-media feed experiment. Includes:

- 19 value scores (`Thought` through `Dependability`)
- Perceived value content (`value_perceptions_1`–`value_perceptions_19`)
- Partisanship (`DemRep_C`: Democrats coded ≤ 3, Republicans > 3 in `fig_2.R`)
- `InterfaceID` for linking to tweet-level data

### `amplification_data_disaggregated.csv`

Tweet-level observations with predicted value scores (`val3_*_yhat`), marginal effects (`val3_*_marginal`), engagement counts, and amplification indicators (`is_amplified`).

## Analysis overview

1. **`fig_1.R`** — Bar chart of cluster-level amplification (rows 20–23 of `amplification_data.csv`) and a radar chart of value-level PCA amplification coefficients.
2. **`fig_2.R`** — Computes Spearman correlations between each participant's normalized value profile and amplification/engagement coefficient vectors, then plots mean alignment by party and funnel stage.
3. **`amplification_inventory.R`** — Fits binomial GLMMs (`glmer`) predicting `is_amplified` from Schwartz value scores, with user random effects. Tests marginal amplification hypotheses via `glht()`.
