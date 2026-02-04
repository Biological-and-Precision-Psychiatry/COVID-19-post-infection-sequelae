# Title:      Cox overall effect
# Author:     Clara S. Grønkjær
# Reviewer:   Rune Haubo B. Christensen
# Date:       2025-11-24
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

# Packages --------------------------------------------------------------------
library(data.table) # For data handling
library(survival)   # For coxph()
library(emmeans)    # For emmeans() and pairs()

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

# We extract the hazard ratios using the emmeans package. 
# Note, that specifying the ref_grid-object is not 
# required, but by enabling the specification of nuisance parameters it reduces 
# the computation time.
# The estimated marginal means are not meaningful themselves, but their 
# contrasts are the required the hazard ratios.

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

# Compute hazard ratios of positive versus negative test for each 2-month
# time interval:
hr_estimates <- pairs(estimated_marginal_means,
                        type = "response",
                        reverse = TRUE,
                        adjust = "none")

# Add confidence intervals for the hazard ratios:
hr_estimates <- summary(hr_estimates, infer = TRUE)

# Optionally display as data.table:
setDT(hr_estimates)
hr_estimates


# R session info ---------------------------------------------------------------
sessionInfo()
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
