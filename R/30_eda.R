## When there are a large number of combinations of EDA to perform, my
## preference is to create a nested tibble such that each row
## represents a single figure.  The columns would represent:
## - Response
## - Predictors
## - subset data
## - pointer to file containing the ggplot instructions
## - figure caption (as string)

## ---- eda_load_data
dat readRDS(paste0(DATA_PATH, "processed/dat.rds"))

## ----end
