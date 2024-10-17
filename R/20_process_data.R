## ---- load_data
dat <- readRDS(paste0(DATA_PATH, "primary/dat.rds"))
## ----end

## Processing steps

## At this point, we should join in a lookup that contains the units
## for each of the responses and covariates for use in figures and tables.
## I will ask Katharina to supply a txt file with these units

## ---- save_data
saveRDS(dat, paste0(DATA_PATH, "processed/dat.rds"))
## ----end
