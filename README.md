# BMSM_PrEP
Estimation of causal effect of PrEP in Black MSM

These files contain data to perform the analysis reported by Meisner et al. (2023+). The goal of the analysis was to combine data from two non-randomized HIV Prevention Trial Network studies to estimate the causal effect of pre-exposure prophylaxis (PrEP) for HIV prevention in Black men who have sex with men (MSM) in the US. The files include:

* alldata_wide20230413.Rda: R dataset containing all data relevent to the analyis. Note that all data have been de-identified.
* BMSManalysis_pt1.R: R code for performing the analysis to estimate the effect of PrEP. For the sake of speed, this code includes only the first 100 (out of 1000) bootstraps used to estimate confidence intervals.
* process_results.R: R code to create plots and tables of results.
  
