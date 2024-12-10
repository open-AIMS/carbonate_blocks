## ---- load_data
dat <- readRDS(paste0(DATA_PATH, "primary/dat.rds"))
## ----end

## ---- load_units
units <- readRDS(paste0(DATA_PATH, "primary/units.rds"))
## ----end


## After an initial run of the analyses, Katharina has requested the
## following changes:
## - express the erosion and accretion values per area of reef and
##   per 3 years
## - add an additional response
##   (weightDifference = PostWeightClean - preWeightEpoxied)

## Processing steps

## ---- add filter feeders
dat <- dat |>
  mutate(erosionFilterFeeders = erosionbivalve + erosionSponge +
           erosionAnnelid + erosionVermetid,
    accretionFilterFeeders = accretionBivalve + accretionBryozoan +
      accretionMollusk + accretionVermetid,
    weightDifference = postweightClean - preweightEpoxied
    )
units <- units |>
  bind_rows(
    tibble(
      `Variable name` = "erosionFilterFeeders",
      label = "Total of block erosion by filter feeders",
      unit = "cm3",
      `Sampling unit` = "block",
      purpose = "Important Response"
    ),
    tibble(
      `Variable name` = "accretionFilterFeeders",
      label = "Volume of block accretion by filter feeders",
      unit = "cm3",
      `Sampling unit` = "block",
      purpose = "Important Response"
    ),
    tibble(
      `Variable name` = "weightDifference",
      label = "Change in block weight",
      unit = "grams",
      `Sampling unit` = "block",
      purpose = "Important Response"
    )
  )
## ----end

## Standardise the erosion and accretion values
## ---- standardise 
dat <- dat |>
  mutate(across(contains(c("erosion", "accretion")),
    list(s = function(x) (x/ durationYears * 3)/(preSA/10000))))
new_names <- str_subset(names(dat), ".*_s$")
units <- units |>
  bind_rows(
    tibble(
      `Variable name` = new_names,
      `Old Variable name` = str_replace(new_names, "(.*)_s$", "\\1"),
      unit = "cm^3 m-2 3yrs-1",
    `Sampling unit` = "block",
    purpose = "Important Response"
    ) |>
      left_join(units |> dplyr::select(`Variable name`, label),
        by = c("Old Variable name" = "Variable name")) |>
      dplyr::select(-`Old Variable name`) 
    )
## ----end

## Katharina would also like to add pre CT density as a covariate

## ---- add preCTDensity
units <- units |>
  mutate(purpose = ifelse(`Variable name` == "preCTDensity", "Covariate", purpose))

## ----end


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
    unit == "cm^3 m-2 3yrs-1" ~ "(cm⁻³ m⁻² 3yr⁻¹)",
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

## For those that have both a standardised and unstandardised verions,
## e.g. totalErosion and totalErosion_s, exclude the unstandardised version.
stand_versions <- responses |> str_detect(".*_s")
base_versions <- responses[stand_versions] |> str_replace("(.*)_s", "\\1")
responses <- responses[stand_versions | !responses %in% base_versions]

covariates <-
  units |>
  filter(str_detect(purpose, "Covariate")) |>
  ## filter(!`Variable name` %in% c("Reef", "Region", "Inshore", "Depth")) |> 
  filter(!`Variable name` %in% c("Reef", "Region", "Inshore")) |> 
  pull(`Variable name`)
spatial_covariates <-
  units |>
  filter(str_detect(purpose, "Covariate")) |>
  filter(`Variable name` %in% c("Reef", "Region", "Inshore", "Depth")) |>
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
saveRDS(units, paste0(DATA_PATH, "processed/units.rds"))
## ----end
