source("../R/simple_modelling_functions.R")

## ---- model_load_data
dat <- readRDS(paste0(DATA_PATH, "processed/dat.rds"))
variables <- readRDS(paste0(DATA_PATH, "processed/variables.rds"))
## ----end


## Response is collected from AIMS_tag (sampling units) within Reef.
## There are 12 Reefs.

## ---- model_configs
model_config <- list(
  volDifference = list(
    trans = function(x) -1 * x,
    do_censor = TRUE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.01, x),
    family = Gamma(link = "log")
    ),
  densityDifference = list(
    trans = function(x) 1 * x,
    do_censor = FALSE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.01, x),
    family = gaussian(link = "identity")
  ),
  erosionGrazing = list(
    trans = function(x) 1 * x,
    do_censor = TRUE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.01, x),
    family = Gamma(link = "log")
    ),
  erosionGrazing_s = list(
    trans = function(x) 1 * x,
    do_censor = TRUE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 1, x),
    family = Gamma(link = "log")
    ),
  totalErosion = list(
    trans = function(x) 1 * x,
    do_censor = TRUE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.01, x),
    family = Gamma(link = "log")
    ),
  totalErosion_s = list(
    trans = function(x) 1 * x,
    do_censor = TRUE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 1, x),
    family = Gamma(link = "log")
    ),
  accretionCCA = list(
    trans = function(x) 1 * x,
    do_censor = TRUE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.01, x),
    family = Gamma(link = "log")
    ),
  accretionCCA_s = list(
    trans = function(x) 1 * x,
    do_censor = TRUE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.1, x),
    family = Gamma(link = "log")
    ),
  totalAccretion = list(
    trans = function(x) 1 * x,
    do_censor = FALSE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.01, x),
    family = Gamma(link = "log")
    ),
  totalAccretion_s = list(
    trans = function(x) 1 * x,
    do_censor = FALSE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 1, x),
    family = Gamma(link = "log")
    ),
  erosionFilterFeeders = list(
    trans = function(x) 1 * x,
    do_censor = FALSE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.01, x),
    family = Gamma(link = "log")
    ),
  erosionFilterFeeders_s = list(
    trans = function(x) 1 * x,
    do_censor = FALSE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.1, x),
    family = Gamma(link = "log")
    ),
  accretionFilterFeeders = list(
    trans = function(x) 1 * x,
    do_censor = TRUE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 0.01, x),
    family = Gamma(link = "log")
    ),
  accretionFilterFeeders_s = list(
    trans = function(x) 1 * x,
    do_censor = TRUE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 1, x),
    family = Gamma(link = "log")
    ),
  weightDifference = list(
    trans = function(x) 1 * x,
    do_censor = FALSE,
    flag = function(x) ifelse(x <= 0, -1, 0),
    censor = function(x) ifelse(x <= 0, 1, x),
    family = gaussian(link = "identity")
    ),
  Abiotic.Perc =  list(
    trans_str = "log(Abiotic.Perc + 1)"
  ),
  Calc.Perc =  list(
    trans_str = "Calc.Perc"
  ),
  CCA.Perc =  list(
    trans_str = "CCA.Perc"
  ),
  ClaySilt.Percent =  list(
    trans_str = "log(ClaySilt.Percent + 1)"
  ),
  cur_mean =  list(
    trans_str = "log(cur_mean)"
  ),
  MA.Perc =  list(
    trans_str = "log(MA.Perc + 1)"
  ),
  omega_ar.mean =  list(
    trans_str = "omega_ar.mean"
  ),
  Oth.calc.Perc =  list(
    trans_str = "log(Oth.calc.Perc + 1)"
  ),
  Secchi.mean =  list(
    trans_str = "Secchi.mean"
  ),
  TA.Perc =  list(
    trans_str = "TA.Perc"
  ),
  WQ.Ind =  list(
    trans_str = "WQ.Ind"
  ),
  preCTDensity =  list(
    trans_str = "preCTDensity"
  ),
  Depth =  list(
    trans_str = "factor(Depth)"
  )
  )
## ----end


## ---- model_prep
models <- tibble(responses = names(variables$responses)) |>
  crossing(covariates = names(variables$covariates))

models <- models |>
  ## filter(responses == "volDifference")
   ## filter(responses == "erosionGrazing")
  filter(responses %in% c(
    "volDifference",
    "densityDifference",
    "erosionGrazing_s",
    "totalErosion_s",
    "accretionCCA_s",
    "totalAccretion_s",
    "erosionFilterFeeders_s",
    "accretionFilterFeeders_s",
    "weightDifference"
    ))

models <- models |>
  ## focus the data
  mutate(data = pmap(.l = list(responses, covariates),
    .f = ~ {
      dat |> dplyr::select(all_of(c(..1, ..2, "Reef", "Depth")))
      }
  )) |> 
  ## transform the data
  mutate(data = map2(.x= data, .y = responses,
    .f = ~ model_transforms(.x, .y)
  )) |>
  ## create the formula
  mutate(formula = map2(.x = responses, .y = covariates,
    ## .f = ~ model_formula(.x, .y)
    .f = ~ model_formula(.x, model_config[[.y]]$trans_str)
  )) |>
  ## calculate simple sums to inform priors
  mutate(simple_sums = pmap(.l = list(data, responses, covariates),
    .f = ~ simple_sums(..1, ..2, ..3, model_config[[..2]]$family))) |>
  ## create priors
  mutate(priors = pmap(.l = list(simple_sums, responses),
    .f = ~ make_priors(..1, model_config[[..2]]$family$family))) |>
  mutate(path = paste0(DATA_PATH, "modelled/", responses, "_", covariates))
## ----end

## ---- fit_models
models <-
  models |>
  mutate(
    model = pmap(.l = list(data, formula, priors, path),
      .f = ~ fit_model(..1, ..2, ..3, ..4)
    )
  )
saveRDS(models, file = paste0(DATA_PATH, "modelled/models.rds"))
# ----end
readRDS(models$model[[11]])
readRDS(models$model[[11]]) |> conditional_effects()

## ---- model_diagnostics
models <- readRDS(file = paste0(DATA_PATH, "modelled/models.rds"))
models_diagnostics <-
  models |> 
  mutate(diagnostics = pmap(.l = list(model, responses, covariates, path),
    .f = ~ make_sampler_diagnostics(..1, ..2, ..3, ..4))
  )  
saveRDS(models_diagnostics, file = paste0(DATA_PATH, "modelled/models_diagnostics.rds"))
## ----end


## Get the posteriors.  These include:
## - partial (full marginal posteriors for a sequence of the covariate)
## - partial_sum (summarised marginal posteriors for a sequence of the covariate)
## - eff (magnitude of effect)
## - eff_sum (summarised magnitude of effect)
## - eff_range (magnitude of effect over the range of the covariate)
## - eff_range_sum (summarised magnitude of effect over the range of the covariate)
## ---- get_posteriors
models <- readRDS(file = paste0(DATA_PATH, "modelled/models.rds"))
models <-
  models |>
  mutate(posteriors = pmap(.l = list(data, model, responses),
    .f = ~ get_posteriors(..1, ..2, ..3, back_trans = FALSE))
  )
## ----end

## ---- plot_partials
models <-
  models |>
  mutate(partials = pmap(.l = list(data, model, posteriors, responses, path),
    .f = ~ make_partial_plots(..1, ..2, ..3$partial_sum, ..4, ..5))
  ) |>
  mutate(partials_cap = pmap(.l = list(responses, covariates),
    .f =  ~ paste0("Partial (modelled) association between ", variables$responses[..1], " and ", variables$covariates[..2], " marginalising over reef and depth within reef.
    The orange ribbon represents the 95% credible interval.  The points are the observed data.")))
saveRDS(models, file = paste0(DATA_PATH, "modelled/models_phase2.rds"))
## ----end

## compare effects within a response
## ---- compare_effects
models_compare <- models |> compare_models()
saveRDS(models_compare, file = paste0(DATA_PATH, "modelled/models_compare.rds"))
## ----end




