# Title: Cox overall effect
# Author: Clara S. Grønkjær
# Date: 2025-11-24
#
# Description ------------------------------------------------------------------
# In this R-script, we fit a Cox model yielding an estimated hazard ratio the 
# overall effect of test status (no test, negative test, and positive test). 
# The Cox proportional hazards models are adjusted for confounders and 
# stratified by age group. 
#
# Content ----------------------------------------------------------------------
# - Format data
# - Compute descriptive statistics
# - Fit Cox model 
# - Extract hazard ratio estimates
#
# Requirements -----------------------------------------------------------------
# To run this program, the dataset tmerge_data is required. 
# This data set is created by the 1_make_time_varying_data.R program.
# I.e., you should be able to run the following line:
# tmerge_data

# Libraries --------------------------------------------------------------------
library(data.table) # For data handling
library(survival)   # For coxph
library(emmeans)    # For emmeans and pairs

# Format data ------------------------------------------------------------------
time_varying_data <- copy(tmerge_data)
setDT(time_varying_data)

# Compute test-variable:
time_varying_data[i_first_test == 0,    c_test := "no test"] 
time_varying_data[i_first_test == 1,    c_test := "negative test"] 
time_varying_data[i_positive_test == 1, c_test := "positive test"] 
time_varying_data[, f_test := factor(c_test, 
                                     levels = c("no test", 
                                                "negative test", 
                                                "positive test"))]

# Duration of record in years with decimal places:
time_varying_data[, duration := tstop - tstart] 

# Compute age groups:
age_group_levels <- c(0, 18, 30, 40, 50, 60, 70, 80, Inf)
time_varying_data[, f_age_group := cut(n_age_start,
                                       breaks=age_group_levels,
                                       include.lowest = TRUE,
                                       right = FALSE)]

# Descriptive statistics  ------------------------------------------------------
descriptive_table <- time_varying_data[, .(individuals = uniqueN(id),
                                           observation_time = sum(duration), 
                                           events = sum(i_outcome)), 
                                       keyby = f_test]

# Event rate in units of 1000 person-years:
descriptive_table[, rate := events / observation_time * 1000] 
descriptive_table[, rate_ratio := rate / rate[1]]
descriptive_table

# Fit Cox model  ---------------------------------------------------------------
# Fit Cox model of the overall effect of test status:
fit <- coxph(Surv(tstart, tstop, event = i_outcome) ~ # Survival object
               f_test +                               # Exposure
               strata(f_age_group) +                  # Stratification
               f_sex +                                # Confounders
               f_charlson_comorbidity_index + 
               f_education +
               f_work + 
               f_income +                   
               f_psych_parents +                      # Parental confounders
               f_charlson_comorbidity_index_parents,                         
             data = time_varying_data)

# Extract hazard ratio estimates -----------------------------------------------
# Estimated marginal means (not reported) are averaged over the confounders:
reference_grid <- ref_grid(fit, 
                           nuisance = c("f_sex", 
                                        "f_charlson_comorbidity_index", 
                                        "f_psych_parents", 
                                        "f_charlson_comorbidity_index_parents", 
                                        "f_education", 
                                        "f_work", 
                                        "f_income"))
estimated_marginal_means <- emmeans(reference_grid, 
                                    specs = "f_test")

# Pairwise comparisons of no test, negative test, and positive test:
pairwise_estimated_marginal_means <- pairs(estimated_marginal_means, 
                                           type = "response", 
                                           reverse = TRUE, 
                                           adjust = "none") 
confidence_intervals <- confint(pairwise_estimated_marginal_means)

# Format estimates
data_pairwise <- as.data.table(pairwise_estimated_marginal_means)
data_pairwise <- data_pairwise[, .(contrast, 
                                   hazard_ratio = ratio, 
                                   pvalue = p.value)]
data_confidence <- as.data.table(confidence_intervals)
data_confidence <- data_confidence[, .(contrast, 
                                       confidence_interval_lower = asymp.LCL, 
                                       confidence_interval_upper = asymp.UCL)]
estimates <- merge(data_pairwise, 
                   data_confidence, 
                   by="contrast")
estimates

# R session info ---------------------------------------------------------------
# > sessionInfo()
# R version 4.4.1 (2024-06-14 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows Server 2022 x64 (build 20348)
# 
# Matrix products: default
# 
# 
# locale:
#   [1] LC_COLLATE=Danish_Denmark.utf8  LC_CTYPE=Danish_Denmark.utf8    LC_MONETARY=Danish_Denmark.utf8
# [4] LC_NUMERIC=C                    LC_TIME=Danish_Denmark.utf8    
# 
# time zone: Europe/Copenhagen
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] emmeans_1.10.2    survival_3.7-0    data.table_1.16.2
# 
# loaded via a namespace (and not attached):
#   [1] codetools_0.2-20   multcomp_1.4-25    Matrix_1.7-0       lattice_0.22-6     coda_0.19-4.1      TH.data_1.1-2     
# [7] splines_4.4.1      estimability_1.5.1 zoo_1.8-12         mvtnorm_1.2-5      xtable_1.8-4       grid_4.4.1        
# [13] sandwich_3.1-0     compiler_4.4.1     tools_4.4.1        MASS_7.3-60.2   

# Program end ------------------------------------------------------------------