# Bank Churners: Customer Segmentation & Churn Analysis

An R-based analysis that uses feature engineering and K-means clustering to segment 10,127 bank credit card customers by churn risk, and generates actionable retention recommendations for each segment.

## Dataset

**BankChurners.csv** — 10,127 credit card customers with demographic, account, and transaction attributes. The overall churn rate is ~16%.

> The dataset is not included in this repository. Place `BankChurners.csv` in the project directory before running.

## Requirements

R 4.0+ with the following packages:

```r
install.packages(c("dplyr", "ggplot2", "cluster", "factoextra", "scales"))
```

## Usage

```bash
Rscript BankChurners_Analysis.R
```

Or from RStudio:

```r
source("BankChurners_Analysis.R")
```

## How It Works

The script runs an 11-step pipeline:

1. **Load & clean** — drops Naive Bayes leakage columns included in the source dataset
2. **Feature engineering** — derives 12 behavioral features from raw transaction data:
   - `monthly_spend`, `avg_txn_value`, `txn_frequency`, `spend_growth`, `txn_growth`
   - `utilization`, `revolving_dependency`, `inactivity_rate`, `contact_intensity`
   - `contact_to_txn_ratio`, `relationship_breadth`, `tenure`
3. **Scale** — standardises all features (required for distance-based clustering)
4. **Elbow method** — evaluates K = 2–9 to find the optimal number of clusters
5. **K-means clustering** — fits with K = 5, 50 starts, seed = 42 for reproducibility
6. **Segment profiling** — summarises churn rate, spend, utilization, etc. per cluster
7. **Segment labeling** — assigns business labels based on churn rate and behavioral thresholds
8. **Key findings** — quantifies the share of total churn driven by low-engagement segments
9. **Visualisations** — generates churn rate bar chart and segment bubble map
10. **Export** — writes a Tableau-ready CSV with all original + engineered columns and segment labels
11. **Retention recommendations** — prints segment-specific strategies to the console

## Outputs

| File | Description |
|------|-------------|
| `elbow_plot.png` | Within-cluster SS by K to justify cluster count |
| `churn_by_segment.png` | Churn rate per segment (bar chart) |
| `segment_map.png` | Monthly spend vs churn rate, bubble sized by customer count |
| `BankChurners_Segmented.csv` | Full dataset with engineered features and segment assignments |

## Segments

| Segment | Profile |
|---------|---------|
| **High-Risk Disengaged** | ≥ 40% churn rate; high inactivity, declining spend |
| **Low-Engagement At-Risk** | ≥ 20% churn; low transaction frequency |
| **High-Value Active** | Top-quartile monthly spend; low churn risk |
| **Revolving Heavy Users** | Top-quartile utilization; financially stretched |
| **Stable Moderate Users** | All remaining customers |
