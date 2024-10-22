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
    unit == "Percent cover" ~ "(%)",
    unit == "degrees S" ~ "(°S)",
    unit == "degrees E" ~ "(°E)",
    unit == "m" ~ "(m)",
    unit == "m s^-1" ~ "(ms⁻¹)",
    unit == "grams" ~ "(g)",
    unit == "g cm^-3" ~ "(g.cm⁻³)",
    unit == "cm3" ~ "(cm³)",
    unit == "cm2" ~ "(cm²)",
    .default = ""
  )) |>
  mutate(label = paste0(label, " ", pretty_units))
## remove any that are not present in the data
units <- units |>
  filter(`Variable name` %in% colnames(dat))
## ----end

## Note, the above removed the NonCalc.Inverts.Perc from units as it is not in the data

## ---- get variables
responses <-
  units |>
  filter(str_detect(purpose, "Response")) |>
  pull(`Variable name`)
covariates <-
  units |>
  filter(str_detect(purpose, "Covariate")) |>
  filter(!`Variable name` %in% c("Reef", "Region", "Inshore")) |> 
  pull(`Variable name`)
spatial_covariates <-
  units |>
  filter(str_detect(purpose, "Covariate")) |>
  filter(`Variable name` %in% c("Reef", "Region", "Inshore")) |>
  pull(`Variable name`)

variables <- list(
  responses = sapply(responses, function(x) {
    units |>
      filter(`Variable name` == x) |>
      pull(label)
  }),
  covariates = sapply(covariates, function(x) {
    units |>
      filter(`Variable name` == x) |>
      pull(label)
  }),
  spatial_covariates = sapply(spatial_covariates, function(x) {
    units |>
      filter(`Variable name` == x) |>
      pull(label)
  })
)
saveRDS(variables, paste0(DATA_PATH, "processed/variables.rds"))
## ----end

## ---- join_units
## ----end

## ---- save_data
saveRDS(dat, paste0(DATA_PATH, "processed/dat.rds"))
## ----end
