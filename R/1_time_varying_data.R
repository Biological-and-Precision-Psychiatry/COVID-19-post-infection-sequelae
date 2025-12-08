# Title: Make time-varying data
# Author: Clara S. Grønkjær
# Date: 2025-11-24
#
# Description ------------------------------------------------------------------
# In this R-script, we create the time-varying data used in analyses.
#
# Content ----------------------------------------------------------------------
# - Load data sets
# - Construct time-varying data using tmerge
# - Format data
#
# Requirements -----------------------------------------------------------------
# To run this program, the following data are required:
#
# - Population data: Individual level data with dates of first PCR test and 
# first positive PCR test (one record per person)
#   - id: Identification number of the individual
#   - d_start: Start date of record
#   - d_end: End date of record
#   - d_first_test: Date of first PCR test, if any, otherwise NA
#   - d_positive_test: Date of first positive PCR test, if any, otherwise NA
#   - d_outcome: Date of outcome, if any, otherwise NA
#
# - Covariate data: Individual level data with covariates at the study start 
#   (one record per person)
#   - id: Identification number of the individual
#   - f_sex: Sex of the individual
#   - n_age_start: Age of the individual at start
#   - f_charlson_comorbidity_index: The individual's Charlson comorbidity index
#   - f_education: Level of education of the individual
#   - f_work: Work status of the individual
#   - f_income: Income quintile of the individual
#   - f_charlson_comorbidity_index_parents: Parents' Charlson comorbidity index
#   - f_psych_parents: Indicator of whether the parents have a psychiatric diagnosis
# 
# I.e., you should be able to run the following lines:
# population_data[, .(id, 
#                     d_start, 
#                     d_end, 
#                     d_first_test, 
#                     d_positive_test, 
#                     d_outcome)] 
# covariate_data[, .(id, 
#                    f_sex, 
#                    n_age_start, 
#                    f_charlson_comorbidity_index, 
#                    f_charlson_comorbidity_index_parents, 
#                    f_psych_parents, 
#                    f_education, 
#                    f_work, 
#                    f_income)]
# 
# Packages ---------------------------------------------------------------------
library(data.table) # For data handling
library(survival)   # For tmerge

# Functions --------------------------------------------------------------------
convert_date_to_numeric <- function(date) {
  # Convenience function to change "Date" objects to numeric numbers.
  # Not strictly required but simplifies some things.
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

# Construct time-varying data --------------------------------------------------
# Convert dates to floats to simplify calculations:
population_data[, `:=` (t_start         = convert_date_to_numeric(d_start),
                        t_stop          = convert_date_to_numeric(d_end),
                        t_first_test    = convert_date_to_numeric(d_first_test),
                        t_positive_test = convert_date_to_numeric(d_positive_test),
                        t_outcome       = convert_date_to_numeric(d_outcome))]

tmerge_data <- tmerge(covariate_data[, .(id, 
                                         f_sex, 
                                         n_age_start, 
                                         f_charlson_comorbidity_index, 
                                         f_charlson_comorbidity_index_parents, 
                                         f_psych_parents, 
                                         f_education, 
                                         f_work, 
                                         f_income)],
                      population_data[, .(id, 
                                          t_start, 
                                          t_stop, 
                                          t_first_test, 
                                          t_positive_test, 
                                          t_outcome)],
                      id = id,
                      tstart = t_start,
                      tstop  = t_stop,
                      i_outcome = event(t_outcome),
                      i_first_test = tdc(t_first_test),
                      i_positive_test = tdc(t_positive_test),
                      options = list(idname = "id"))

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
#   [1] survival_3.7-0    data.table_1.16.2
# 
# loaded via a namespace (and not attached):
#   [1] compiler_4.4.1 Matrix_1.7-0   tools_4.4.1    splines_4.4.1  grid_4.4.1     lattice_0.22-6
# Program end ------------------------------------------------------------------
