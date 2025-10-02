# Estimating Climate Sensitivity Using Bayesian Model Averaging for CMIP Models

This repository contains the scripts and data processing for analyzing CMIP6 climate model projections, linking temperature trajectories with SSP CO₂ emissions, and performing Bayesian Model Averaging (BMA) for projection and variance decomposition.  

---

## Scripts Overview

- **`clean_CMIP6_data_bma.R`**  
  Reads raw CMIP6 `.dat` files, cleans and aggregates monthly/annual temperature series, computes ensemble quantiles, and saves the processed dataset as `cleaned_all_data_bma_cmip6.Rda`.

- **`clean_all_data_adjusted_cmip6.R`**  
  Takes the processed CMIP6 dataset, averages multiple ensemble members within the same model–scenario to create representative trajectories, and saves the adjusted dataset as `cleaned_all_data_bma_adjusted_cmip6.Rda`.

- **`get_all_df.R`**  
  Combines the adjusted CMIP6 temperature trajectories with interpolated and cumulative SSP CO₂ emissions, aligns them for 2016–2100, and saves the merged long-format dataset as `all_df_allmodel_new.RData` (with HadCRUT5 observations for validation).

- **`bma_cmip6.R`**  
  Runs Bayesian Model Averaging (BMA) projections using the processed CMIP6 temperature and CO₂ data.

- **`bma_cmip6_decompose_step1.R` & `bma_cmip6_decompose_step2.R`**  
  Implements the two-step variance decomposition analysis for the BMA projections.

---

## Data Availability  

The processed CMIP6 datasets used in this analysis are available at:  
**[https://drive.google.com/drive/folders/1FWKNscRbaQ2oTz2IU0K3v6GnnJI9iPMx]**
