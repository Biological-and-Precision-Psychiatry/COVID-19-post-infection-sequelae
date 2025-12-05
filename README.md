# COVID-19-post-infection-sequelae

Paper: COVID-19 post-infection sequelae are comparable to sequelae observed after other infections of similar severity---a nationwide Danish study with 40-month follow-up

- Authors: [Clara S. Grønkjær](https://github.com/claragronkjar), 
[Rune H. B. Christensen](https://github.com/runehaubo), 
[Daniel Kondziella](https://research.regionh.dk/en/persons/daniel-kondziella/), and 
[Michael E. Benros](https://research.regionh.dk/da/persons/michael-eriksen-benros/)
- Link to abstract: [updated once paper is published]
- Status: Currently under review


## R-scripts:

The R-scripts listed below are available in the `R` folder in this repository. The files contain code that reproduces key analyses of the paper. The data to run the code cannot be made publicly available so the code will not run outside of the computing environment provided by [Statistics Denmark](https://www.dst.dk/en/)

- `1_make_time_varying_data.R`: 
  This program creates the time-varying data used for the analyses.
  Requirements: Population and covariate data.

- `2_cox_overall_effect.R`:
  This program fits a Cox model to estimate the overall effect of test status (no test, negative test, and positive test).
  Requirements: The data created in `1_make_time_varying_data.R`.

- `3_cox_number_of_tests.R`:
  This program creates a new time-varying dataset, splitting at every new (grouped) conducted test. Then it fits a Cox model with effect modification by number of conducted tests.
  Requirements: The data created in `1_make_time_varying_data.R` and test data.

- `4_cox_2_month_intervals.R`:
  This program creates a new time-varying dataset by splitting the data in 2-month intervals. Then it fits a Cox model with different hazard ratios per time interval.
  Requirements: The data created in `1_make_time_varying_data.R`.


## Requirements:

- [R](https://www.r-project.org/) 
- [RStudio](https://posit.co/download/rstudio-desktop/) (optional but recommended)
- Required R packages all available on [CRAN](https://cran.r-project.org/) are explicitly loaded in each of the R-scripts


### Datasets:

Most variables have a prefix indicating the type of variable: 

- `d_`: date (class 'Date')
- `f_`: factor
- `n_`: numeric

These programs require three datasets:

- Population data: Individual level data with dates of first PCR test and first positive PCR test (one record per person):
  - `id`: Identification number of the individual.
  - `d_start`: Start date of record.
  - `d_end`: End date of record.
  - `d_first_test`: Date of first PCR test, if any.
  - `d_positive_test`: Date of first positive PCR test, if any.
  - `d_outcome`: Date of outcome, if any.

- Covariate data: Individual level data with covariates at the study start (one record per person):
  - `id`: Identification number of the individual.
  - `f_sex`: Sex of the individual.
  - `n_age_start`: Age of the individual at start.
  - `f_charlson_comorbidity_index`: The individual's Charlson comorbidity index.
  - `f_education`: Level of education of the individual.
  - `f_work`: Work status of the individual.
  - `f_income`: Income quintile of the individual.
  - `f_charlson_comorbidity_index_parents`: Parents' Charlson comorbidity index
  - `f_psych_parents`: Indicator of whether the parents have a psychiatric diagnosis.

- Test data: Individual level data with dates of all PCR tests and test results (one record per person and test):
  - `id`: Identification number of the individual.
  - `d_test`: Date of test.
  - `f_test`: Test status (negative or positive).
  - `n_test_seq`: Number of test.

