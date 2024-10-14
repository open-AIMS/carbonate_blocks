## Configure project
source("05_configure_project.R")

## Read in the data
source("10_read_data.R")

## Validate the data
source("15_validate_data.R")

## Process the data
source("20_process_data.R")

## Note, there are an extremely large number of responses and
## covariates in this dataset. We will not be able to explore all
## combinations. I suggest that we focus on the ones that Katharina
## has highlighted in ../data/raw/Variable explanations for Blocks.xlsx

## This should illustrate the main routines and provide enough
## guidance for her should she elect to expand to the additional
## responses and covariates

## EDA
source("30_eda.R")
