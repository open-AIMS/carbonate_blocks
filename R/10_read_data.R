## ---- import data
dat <- read_csv(paste0(RAW_DATA_PATH, "Blocks_eReefs_silt_biota_erosion_metab_2024Oct.csv"))
saveRDS(dat, paste0(DATA_PATH, "primary/dat.rds"))
## ----end
