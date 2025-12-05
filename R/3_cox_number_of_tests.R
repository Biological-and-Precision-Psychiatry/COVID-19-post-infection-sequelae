# Title: Cox number of tests
# Author: Clara S. Grønkjær
# Date: 2025-11-24
#
# Description ------------------------------------------------------------------
# In this R-script, we fit a Cox model yielding an estimated hazard ratio of 
# positive versus negative test by the number of conducted tests. 
# The Cox proportional hazards models are adjusted for confounders and 
# stratified by age group. 
# One can estimate hazard ratios for various effect modifiers (e.g., vaccination 
# status) by altering the variables in the table.
#
# Content ----------------------------------------------------------------------
# - Construct time-varying data with number of conducted tests
# - Format data
# - Compute descriptive statistics
# - Fit Cox model 
# - Extract hazard ratio estimates
#
# Requirements -----------------------------------------------------------------
# To run this program, the following data are required:
#
# - tmerge_data: This data set is created by the 1_make_time_varying_data.R 
#   program.
# - test_data: Individual level data with dates of all PCR tests and test results 
#   (one record per person and test)
#   - id: Identification number of the individual
#   - d_test: Date of test 
#   - f_test: Test status (negative or positive)
#   - n_test_seq: Sequence number of test
#
# I.e., you should be able to run the following lines:
# tmerge_data
# test_data[, .(id, d_test, f_test, n_test_seq)]

# Libraries --------------------------------------------------------------------
library(data.table) # For data handling
library(survival)   # For coxph and tmerge
library(emmeans)    # For emmeans and pairs 

# Functions --------------------------------------------------------------------
convert_date_to_numeric <- function(date) {
  if(!is(date, "Date")) {
    date <- as.Date(date)
    if(!is(date, "Date")) stop("Failed to coerce 'date' to class 'Date'")
  }
  yr <- year(date)
  dstart <- unclass(as.Date(paste0(yr, "-01-01"), format = "%Y-%m-%d"))
  dend <- unclass(as.Date(paste0(yr + 1, "-01-01"), format = "%Y-%m-%d"))
  days_yr <- dend - dstart
  days <- unclass(date) - dstart
  yr + days / days_yr
}

# Format test data -------------------------------------------------------------
# Add baseline row for all individuals:
test_data <- rbind(population_data[, .(id, 
                                       d_test = d_start, 
                                       n_test_seq = 0)], 
                   test_data[, .(id, 
                                 d_test, 
                                 n_test_seq)])

# Group number of tests:
number_of_tests_levels <- c(-1, 3.5, 9.5, 14.5, Inf)
number_of_tests_labels <- c("00-03", "04-09", "10-14", "15+")
test_data[, f_number_of_tests := cut(n_test_seq, 
                                     breaks = number_of_tests_levels, 
                                     labels = number_of_tests_labels)]

# Get first record per person and grouping of number of tests:
setorder(test_data, id, f_number_of_tests)
test_data <- test_data[, .SD[1], .(id, f_number_of_tests)]

# Format date variable:
test_data[, t_test := convert_date_to_numeric(d_test)]

# Construct time-varying data --------------------------------------------------
number_of_tests_tmerge_data <- tmerge(tmerge_data, 
                                      test_data,
                                      id = id,
                                      f_number_of_tests = tdc(t_test, 
                                                              f_number_of_tests))

# Format data ------------------------------------------------------------------
number_of_tests_data <- copy(number_of_tests_tmerge_data)
setDT(number_of_tests_data)

# Compute test-variable:
number_of_tests_data[i_first_test == 0,    c_test := "no test"] 
number_of_tests_data[i_first_test == 1,    c_test := "negative test"] 
number_of_tests_data[i_positive_test == 1, c_test := "positive test"] 

# Remove individuals not tested:
number_of_tests_data <- number_of_tests_data[c_test != "no test"]
number_of_tests_data[, f_test := factor(c_test, 
                                        levels = c("negative test", 
                                                   "positive test"))]

# Duration of record in years with decimal places:
number_of_tests_data[, duration := tstop - tstart] 

# Compute age groups:
age_group_broad_levels <- c(0, 18, 40, 60, 80, Inf)
number_of_tests_data[, f_age_group_broad := cut(n_age_start,
                                                breaks=age_group_broad_levels,
                                                include.lowest = TRUE,
                                                right = FALSE)]

# Descriptive statistics  ------------------------------------------------------
descriptive_table <- number_of_tests_data[, .(individuals = uniqueN(id),
                                              observation_time = sum(duration), 
                                              events = sum(i_outcome)), 
                                          keyby = .(f_test, f_number_of_tests)]

# Event rate ratio in units of 1000 person-years:
descriptive_table[, rate := events / observation_time * 1000] 
descriptive_table[, rate_ratio := rate / rate[1], f_number_of_tests]
descriptive_table

# Fit Cox model  ---------------------------------------------------------------
# Fit Cox model with different effects for each grouping of number of tests:
fit <- coxph(Surv(tstart, tstop, event = i_outcome) ~  # Survival object
               f_number_of_tests /                     # Effect modifier
               f_test +                                # Exposure
               strata(f_age_group_broad) +             # Stratification
               f_sex +                                 # Confounders
               f_charlson_comorbidity_index  + 
               f_education + 
               f_work + 
               f_income +                   
               f_psych_parents +                       # Parental confounders
               f_charlson_comorbidity_index_parents,                         
             data = number_of_tests_data)

# Fit Cox model with same effect to extract p-value for heterogeneity:
fit0 <- coxph(Surv(tstart, tstop, event = i_outcome) ~ # Survival object
                f_number_of_tests +                    # Effect modifier
                f_test +                               # Exposure
                strata(f_age_group_broad) +            # Stratification
                f_sex +                                # Confounders
                f_charlson_comorbidity_index + 
                f_education + 
                f_work + 
                f_income +                   
                f_psych_parents +                      # Parental confounders
                f_charlson_comorbidity_index_parents,      
              data = number_of_tests_data)

pvalue_heterogeneity <- anova(fit, fit0)[2, "Pr(>|Chi|)"]

# Extract hazard ratio estimates -----------------------------------------------

# We extract the hazard ratios for positive versus negative test by no. tests 
# using the emmeans package. Note, that specifying the ref_grid-object is not 
# required, but by enabling the specification of nuisance parameters it reduces 
# the computation time.
# The estimated marginal means are not meaningful themselves, but their 
# contrasts are the required the hazard ratios.

# Estimated marginal means are averaged over the confounders:
reference_grid <- ref_grid(fit,
                           nuisance = c("f_sex",
                                        "f_charlson_comorbidity_index",
                                        "f_psych_parents",
                                        "f_charlson_comorbidity_index_parents",
                                        "f_education",
                                        "f_work",
                                        "f_income"))
estimated_marginal_means <- emmeans(reference_grid,
                                    specs = "f_test",
                                    by = "f_number_of_tests")

# Compute hazard ratios of positive versus negative test for each 2-month
# time interval:
hr_by_no_tests <- pairs(estimated_marginal_means,
                        type = "response",
                        reverse = TRUE,
                        adjust = "none")

# Add confidence intervals for the hazard ratios:
hr_by_no_tests <- summary(hr_by_no_tests, infer=TRUE)

# Optionally display as data.table:
setDT(hr_by_no_tests)
hr_by_no_tests

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