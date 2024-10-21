## ---- load_data
dat <- readRDS(paste0(DATA_PATH, "primary/dat.rds"))
## ----end

## ---- load_units
units <- readRDS(paste0(DATA_PATH, "primary/units.rds"))
## ----end

## Processing steps

## At this point, we should join in a lookup that contains the units
## for each of the responses and covariates for use in figures and tables.
## I will ask Katharina to supply a txt file with these units

## Might need to pivot the data longer at this point

## ---- process units
units <- units |>
  mutate(pretty_units = case_when(
    unit == "Percent" ~ "(%)",
    unit == "degrees" ~ "(°)",
    unit == "m s^-1" ~ "(ms⁻¹)",
    unit == "grams" ~ "(g)",
    unit == "g cm^-3" ~ "(g.cm⁻³)",
    unit == "cm3" ~ "(cm³)",
    unit == "cm2" ~ "(cm²)",
    .default = ""
  ))
## ----end

## ---- join_units
## ----end

## ---- save_data
saveRDS(dat, paste0(DATA_PATH, "processed/dat.rds"))
## ----end
