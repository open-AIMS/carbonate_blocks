## ---- load_data
dat <- readRDS(paste0(DATA_PATH, "primary/dat.rds"))
## ----end

## Processing steps

## ---- save_data
saveRDS(dat, paste0(DATA_PATH, "processed/dat.rds"))
## ----end
