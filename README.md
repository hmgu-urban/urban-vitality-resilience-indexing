# Urban Vitality Resilience Indexing (UVRI) method package

This package contains files for preparing a MethodsX-style method article based on the
published study:

Lee, S., Gu, H., Kim, S., & Kim, K. (2026). Resilience of urban vitality under external shocks:
Planning insights from Seoul during the COVID-19 pandemic. Sustainable Cities and Society, 141, 107232.
https://doi.org/10.1016/j.scs.2026.107232

## Files

### Method code

- `uvri_method_demo.R`
  - Clean R implementation of the UVRI method.
  - `index_vitality_resilience()` calculates resistance (RSTN), recovery capacity (RCVY),
    and adaptability (ADPT).
  - `run_full_validation()` reproduces the method-article validation tables: descriptives,
    comparison with simple pre-post change, and the two sensitivity tables.
  - The integer time index is derived from the calendar month itself, so `base_lag` and the
    phase spans remain correct even when a unit has missing months (no row-index tricks).
  - `mean_stat` selects the local-mean statistic (`"mean"` or `"median"`).
  - Includes a Seoul administrative-code harmonization helper.

### Synthetic data (safe to redistribute)

- `synthetic_vitality_monthly.csv`
  - Synthetic monthly activity data for 424 spatial units from 2018-01 to 2024-02.
  - Columns: `unit_id`, `month`, `t`, `vitality`, `trajectory_type`.
  - The activity column is named `vitality` (pass `value_col = "vitality"`).
- `synthetic_unit_metadata.csv`
  - Trajectory type and simulated turning points for each synthetic unit.
- `synthetic_resilience_outputs.csv`
  - UVRI output calculated from the synthetic dataset.
- `synthetic_validation_summary.csv`
  - Table 7-style summary of RSTN, RCVY, and ADPT for the synthetic dataset.

### Seoul application (aggregate results only; raw data not redistributed)

- `actual_validation_summary.csv`
  - Table 7-style summary (mean / SD / min / max of RSTN, RCVY, ADPT) for the Seoul
    de facto population panel. Reproduces the published descriptive values:
    RSTN mean ~= -0.65, RCVY mean ~= 0.34, ADPT mean ~= -0.45.
- Per-neighborhood outputs and the raw `pop.csv` are not included, since they would expose
  unit-level de facto population values. The Seoul de facto population data are available from
  the authors upon reasonable request, subject to data-use conditions. (Because the RSTN,
  RCVY, and ADPT ratios are invariant to positive, unit-specific scaling, counts and densities
  yield identical indicators.)

## Reproducing the validation tables

Running `uvri_method_demo.R` calls `run_full_validation()` and writes the files below. They
carry a `_from_R` suffix so the shipped reference files are not overwritten.

| Article table | Output file (`synthetic_*`, and `actual_*` when `pop.csv` is supplied) | Reports |
| --- | --- | --- |
| Table 7 | `*_table7_descriptives_from_R.csv` | Mean / SD / min / max of RSTN, RCVY, ADPT |
| Table 8 | `*_table8_vs_simple_from_R.csv` | Correlation with simple pre-post change |
| Table 9 | `*_table9_adaptation_window_from_R.csv` | Sensitivity to the local-mean window (8-month, 6-month, median) |
| Table 10 | `*_table10_base_lag_from_R.csv` | Sensitivity to the base lag (6 vs 12 months) |

The synthetic example runs out of the box. For the Seoul tables, place `pop.csv` in this
folder and run the script; Table 7 then reproduces the published means and Table 8 reproduces
the published correlations (0.361 / 0.346, 0.377 / 0.464, 0.436 / 0.496).

## Default temporal windows

The package uses the code-equivalent windows from the original analysis:

- Peak window: 2019-06 to 2020-02
- Local minimum window: 2020-03 to 2022-03
- Local maximum window: 2022-04 to 2023-04
- Local mean window: 2023-05 to 2024-02
- Base point: 12 months before the peak

The windows are non-overlapping: each policy-transition month is assigned to the later phase.
They can be changed in `index_vitality_resilience()`.
