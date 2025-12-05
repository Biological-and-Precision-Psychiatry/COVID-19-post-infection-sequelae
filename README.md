# COVID-19-post-infection-sequelae

Paper: COVID-19 post-infection sequelae are comparable to sequelae observed after other infections of similar severity – a nationwide Danish study with 40-month follow-up

- Authors: Clara S. Grønkjær, Rune H. B. Christensen, Daniel Kondziella, and Michael E. Benros
- Link to abstract: [updated once paper is published]


## Purpose of the repository:

In this repository, we show the necessary information to run the analyses from the paper titled above.

## Content of the repository:

- R-scripts

## R-scripts:

- 1_make_time_varying_data.R: 
  This program creates the time-varying data used for the analyses.
  Requirements: Population and covariate data.

- 2_cox_overall_effect.R:
  This program fits a Cox model to estimate the overall effect of test status (no test, negative test, and positive test).
  Requirements: The data created in 1_make_time_varying_data.R.

- 3_cox_number_of_tests.R:
  This program creates a new time-varying dataset, splitting at every new (grouped) conducted test. Then it fits a Cox model with effect modification by number of conducted tests.
  Requirements: The data created in 1_make_time_varying_data.R and test data.

- 4_cox_2_month_intervals.R:
  This program creates a new time-varying dataset by splitting the data in 2-month intervals. Then it fits a Cox model with different hazard ratios per time interval.
  Requirements: The data created in 1_make_time_varying_data.R.


## Requirements:

- [R](www.r-project.org) 
- RStudio (optional but recommended)
- Required R packages listed in R-scripts/packages.R

### Datasets:

These programs require three datasets:

- Population data: Individual level data with dates of first PCR test and first positive PCR test (one record per person):
  - id: Identification number of the individual.
  - d_start: Start date of record.
  - d_end: End date of record.
  - d_first_test: Date of first PCR test, if any.
  - d_positive_test: Date of first positive PCR test, if any.
  - d_outcome: Date of outcome, if any.

- Covariate data: Individual level data with covariates at the study start (one record per person):
  - id: Identification number of the individual.
  - f_sex: Sex of the individual.
  - n_age_start: Age of the individual at start.
  - f_charlson_comorbidity_index: The individual's Charlson comorbidity index.
  - f_education: Level of education of the individual.
  - f_work: Work status of the individual.
  - f_income: Income quintile of the individual.
  - f_charlson_comorbidity_index_parents: Parents' Charlson comorbidity index
  - f_psych_parents: Indicator of whether the parents have a psychiatric diagnosis.

- Test data: Individual level data with dates of all PCR tests and test results (one record per person and test):
  - id: Identification number of the individual.
  - d_test: Date of test.
  - f_test: Test status (negative or positive).
  - n_test_seq: Number of test.

