# Sepsis Flow Model Build Repository

This repository reproduces the modelling workflow for the Sepsis Flow research project:
outcome construction, preprocessing, train/test split, predictor-set preparation, day-1/day-2 model training, analysis summaries, export bundles, and API evaluation.

## 1) Required Data Inputs

Place the source files in `data/raw/` using these exact names:

- `data/raw/AllFields_Follow-up_Nov2025.csv`
- `data/raw/AllFields_Discharge_Nov2025.csv`
- `data/raw/AllFields_Enrolment_Nov2025.csv`
- `data/raw/predictive.analysis.univariate_17Oct2024.xlsx`

The repository may include placeholder files for structure only.

## 2) Canonical Repository Layout

```text
sepsis-flow-build/
├── R/ # shared bootstrap/config/helpers
├── config/ # pipeline + input manifests
├── data/
│   ├── raw/ # required source data (input)
│   ├── processed/ # pipeline-generated datasets
│   └── reference/ # pipeline-generated lookup/reference objects
├── scripts/ # staged modelling scripts
└── outputs/
    ├── models/ # trained model artifacts + ensemble workspace
    ├── results/ # non-tabular stage outputs (rds/json bundles)
    │   ├── analysis/
    │   ├── evaluation/
    │   ├── exports/
    │   └── intermediate/
    ├── tables/ # tabular artifacts (csv/rds tables)
    │   ├── analysis/
    │   ├── evaluation/
    │   └── preprocessing/
    └── figures/ # plots and graphics
        ├── analysis/
        └── evaluation/
```

## 3) Script and Path Contract

- Shared initialization: `R/bootstrap.R`
- Path configuration: `R/config.R`
- Pipeline order: `config/pipeline_manifest.csv`
- Pipeline runner: `scripts/run_pipeline.R`

All scripts use repo-local paths from `PATHS` in `bootstrap.R`.
No machine-specific absolute paths are required.

## 4) Run the Full Pipeline

From the repository root:

```bash
Rscript scripts/run_pipeline.R
```

Or run staged scripts manually in the order listed in `config/pipeline_manifest.csv`.

## 5) Output Locations by Type

- Models: `outputs/models/`
- Analysis/evaluation/export/intermediate result objects: `outputs/results/`
- Tables: `outputs/tables/`
- Figures: `outputs/figures/`

## 6) Reproducibility Notes

- Seeds and runtime options are set in `R/bootstrap.R`.
- Evaluation scripts can use environment overrides such as `D1_API_BASE_URL`, `D2_API_BASE_URL`, and `EVAL_MODE`.

## 7) Disclaimer

This repository contains research code and placeholder data only.
It is not for clinical decision-making.
