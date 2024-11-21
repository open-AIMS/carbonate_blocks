
## ---- model_transforms_functions
model_transforms <- function(data, responses) {
  data <- data |> 
    ## mutate(resp = model_config[[responses]]$trans(data[[responses]])) |>
    mutate(resp = model_config[[responses]]$trans(!!sym(responses))) |>
    mutate(do_censor = model_config[[responses]]$do_censor)
  if (model_config[[responses]]$do_censor) {
    data <- data |> 
      mutate(flag = model_config[[responses]]$flag(resp)) |>
      mutate(resp = model_config[[responses]]$censor(resp))
  }
  data
}
## ----end

model_formula <- function(response, covariate) {
  bf(paste0("resp ~ 1 + (1 |Reef/Depth) + ", covariate),
    family = model_config[[response]]$family) 
}

simple_sums <- function(data, response, covariate, family) {
  ## Intercept
  intercept <-
    data |> 
    summarise(median = median(family$linkfun(resp), na.rm = TRUE),
      mad = mad(family$linkfun(resp), na.rm = TRUE)) |>
    suppressMessages() |>
    suppressWarnings()
  ## variance in Depths within Reefs
  depth_var <- data |> 
    group_by(Reef, Depth) |> 
    summarise(median = median(family$linkfun(resp), na.rm = TRUE)) |>
    ungroup() |>
    group_by(Reef) |> 
    summarise(
      mad = mad(median, na.rm = TRUE),
      median = median(median, na.rm = TRUE)
    ) |> 
    ungroup() |>
    summarise(
      mad = median(mad, na.rm = TRUE)) |> 
    pull(mad) |> 
    suppressMessages() |>
    suppressWarnings()
  ## variance in Reefs
  reef_var <- data |> 
    group_by(Reef) |> 
    summarise(median = median(family$linkfun(resp), na.rm = TRUE)) |>
    ungroup() |>
    summarise(
      mad = mad(median, na.rm = TRUE)) |> 
    pull(mad) |> 
    suppressMessages() |>
    suppressWarnings()
  ## variance in effect
  effect_var <- data |> 
    group_by(Reef) |> 
    summarise(median = median(family$linkfun(resp), na.rm = TRUE),
      cov = median(!!sym(covariate), na.rm = TRUE)) |>
    ungroup() |>
    ## summarise(mad = mad(median)/mad(cov))
    summarise(mad = mad(median))
  
  list(intercept = intercept, depth_var = depth_var,
    reef_var = reef_var,
    effect_var = effect_var)
}

make_priors <- function(simple_sums, family = "gaussian") {
  priors <- prior_string(paste0("normal(",
    simple_sums$intercept[["median"]], ", ",
    simple_sums$intercept[["mad"]], ")"), class = "Intercept") +
    prior_string(paste0("student_t(3, 0, ", simple_sums$depth_var, ")"), class = "sd", group = "Reef:Depth") +
    prior_string(paste0("student_t(3, 0, ", simple_sums$reef_var, ")"), class = "sd", group = "Reef") +
    prior_string(paste0("normal(0, ", simple_sums$effect_var, ")"), class = "b")
  if (family == "gaussian") {
    priors <- priors + prior_string(paste0("student_t(3, 0, ", simple_sums$effect_var, ")"), class = "sigma")
  }
  if (family == "gamma") {
   priors <- priors + prior(gamma(0.01, 0.01), class = "shape")
  }
  priors
}

fit_model <- function(dat, form, priors, path) {
  environment(form) <- new.env()
  mod <- brm(form = form,
    data = dat,
    prior = priors,
    seed = 123,
    iter = 5000,
    warmup = 1000,
    thin = 5,
    chains = 4, cores = 4,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95, max_treedepth = 15)
    )
  path <- paste0(str_replace(path, "modelled/", "modelled/fit_"), ".rds")
  saveRDS(mod, file = path)
  path
}

## back_trans in this context is whether to back transform the response
## from positive to negative
get_posteriors <- function(data, model, response, back_trans = TRUE) {
  mod <- readRDS(model)
  predictor <- insight::find_predictors(mod)$conditional
  newdata <- modelr::data_grid(data,
    !!sym(predictor) :=  modelr::seq_range(!!sym(predictor), 100)) |>
    mutate(Reef = NA, Depth = NA)
  ## marginal partial posteriors
  partial <- add_epred_draws(newdata, mod,
    re_formula = NULL,
    allow_new_levels = TRUE)
  if (back_trans) {
    partial <-
      partial |>
      mutate(.epred = model_config[[response]]$trans(.epred))
  }
  
  ## summarised partial posteriors
  partial_sum <- 
    partial |>
    dplyr::select(-.chain, -.iteration, -.draw) |>
    tidybayes::summarise_draws(median, HDInterval::hdi)
  ## magnitude of effect
  eff <- mod |>
    as_draws_df(variable = paste0("^b_.*", predictor),
      regex = TRUE) |> 
    mod$family$linkinv()
  if (back_trans) {
    eff <-
      eff |>
      mutate(across(everything(),  ~ model_config[[response]]$trans(.x)))
  }
  ## summarised magnitude of effect
  eff_sum <-
    eff |> 
    tidybayes::summarise_draws(median, HDInterval::hdi,
      Pl = ~ mean(.x < ifelse(mod$family$link == "identity", 0, 1)),
      Pg = ~ mean(.x > ifelse(mod$family$link == "identity", 0, 1))
    )
  ## magnitude of effect over the range of the covariate
  eff_range <-
    partial |>
    ungroup() |> 
    filter(!!sym(predictor) == min(!!sym(predictor)) |
             !!sym(predictor) == max(!!sym(predictor))) |>
    group_by(.draw) |>
    summarise(min = min(!!sym(predictor)),
      max = max(!!sym(predictor)),
      abs_effect = diff(.epred),
      rel_effect = exp(diff(log(.epred)))  ## need to swap this out as it will not work with negatives
      ## rel_effect = 1 + diff(.epred) / .epred[1]
    )
  ## summarised magnitude of effect over the range of the covariate
  eff_range_sum <-
    eff_range |> 
    pivot_longer(c(abs_effect, rel_effect), names_to = "effect", values_to = "values") |>
    group_by(min, max, effect) |> 
    summarise(
      median = median(values),
      lower = HDInterval::hdi(values)[1],
      upper = HDInterval::hdi(values)[2],
      Pl = mean(values < ifelse(mod$family$link == "identity" | effect == "abs_effect", 0, 1)),
      Pg = mean(values > ifelse(mod$family$link == "identity" | effect == "abs_effect", 0, 1))
    )
  list(partial_sum = partial_sum, partial = partial,
    eff_sum = eff_sum, eff = eff,
    eff_range_sum = eff_range_sum, eff_range = eff_range)
}

make_partial_plots <- function(data, mod, partial_sum, response, path) {
  mod <- readRDS(mod)
  predictor <- insight::find_predictors(mod)$conditional
  gg <-
    partial_sum |> 
    ggplot(aes(y = median, x = !!sym(predictor))) +
    geom_point(data = data, aes(y = resp, x = !!sym(predictor)), alpha = 0.5) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.2) +
    coord_cartesian() +
    scale_x_continuous(name = variables$covariates[[predictor]]) +
    scale_y_continuous(name = variables$responses[[response]], expand = c(0,0)) + 
    ## scale_y_log10() +
    theme_classic()
  path <- paste0(str_replace(path,
    paste0(DATA_PATH, "modelled/"),
    paste0(FIGURES_PATH, "partial_plot_")),
    ".png")
  ggsave(filename = path, plot = gg, device = "png", width = 6, height = 4, units = "in", dpi = 300)
  path
}

compare_models <- function(models) {
  models_compare <-
    models |>
    mutate(effects = map2(.x = posteriors, .y = covariates,
      .f = ~ .x$eff_range
    )) |>
    dplyr::select(responses, covariates, path, effects) |> 
    unnest(effects) |>
    group_by(responses) |>
    nest() 
  models_compare <-
    models_compare |>
    mutate(effects_plot = pmap(.l = list(data, responses),
      .f = ~ make_effects_plot(..1, ..2)
    )) |>
    mutate(effects_plot_cap = pmap(.l = list(responses, data),
      .f =  ~ paste0("Posterior distributions of effect sizes (change in Y over the range in X) associated with each covariate on ", variables$responses[..1], ". Distributions are coloured according to whether there is evidence of a decline (red), increase (green) or neutral (blue). The x-axis expressess the effect size as a ", ifelse(any(is.na(..2$rel_effect)), "absolute", "percentage")," change in the response variable across the range of the covariate. Numbers inside the distributions express effect size as an absolute change in the response over the range of the covariate."))
    )
  models_compare
}

make_effects_plot <- function(data, responses) {
  path <- paste0(FIGURES_PATH, "effects_plot_", responses, ".png")
  response <- variables$responses[responses]
  plot_dat <- data |>
    ## filter(!covariates %in% c("cur_mean", "omega_ar.mean")) |> 
    group_by(covariates) |>
    mutate(
      Pl = mean(rel_effect < 1),
      Pg = mean(rel_effect > 1),
      flag = ifelse(Pl > 0.9, "decrease", ifelse(Pg > 0.9, "increase", "neutral")),
      Pl_a = mean(abs_effect < 0),
      Pg_a = mean(abs_effect > 0),
      flag_a = ifelse(Pl_a > 0.9, "decrease", ifelse(Pg_a > 0.9, "increase", "neutral"))
    ) |> 
    ungroup()  
  plot_dat_sum <- plot_dat |>
    group_by(covariates) |>
    summarise(abs_effect = median(abs_effect),
      rel_effect = median(rel_effect))
  if (any(is.na(plot_dat_sum$rel_effect))) { ## caused by relative change that goes negative
    gg <-
      plot_dat |> 
      ggplot(aes(x = abs_effect, y = covariates)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      stat_slab(aes(fill = flag_a, color = flag_a), linewidth = 0.5, alpha = 0.5, normalize = "groups", show.legend = FALSE) +
      geom_text(data = plot_dat_sum, aes(x = abs_effect, label = round(abs_effect, 2)), hjust = 0.5, nudge_y = 0.3) +
      scale_x_continuous("Effect size (absolute change over the range of the covariate)") +
        ## trans = scales::log2_trans(),
        ## breaks = c(0.05,0.1, 0.2, 0.5, 1, 2, 3, 6, 11),
        ## labels = \(x) 100 * (x -1),
        ## limits = c(0.01, 20,000)) +
      scale_y_discrete("", labels = function(nm) variables$covariates[nm]) +
      theme_bw() +
      ggtitle(paste0("Effect of various covariates on ", response))
    
  } else {
    gg <-
      plot_dat |> 
      ggplot(aes(x = rel_effect, y = covariates)) +
      geom_vline(xintercept = 1, linetype = "dashed") +
      stat_slab(aes(fill = flag, color = flag), linewidth = 0.5, alpha = 0.5, normalize = "groups", show.legend = FALSE) +
      geom_text(data = plot_dat_sum, aes(x = rel_effect, label = round(abs_effect, 2)), hjust = 0.5, nudge_y = 0.3) +
      scale_x_continuous("Effect size (% change over the range of the covariate)",
        trans = scales::log2_trans(),
        breaks = c(0.05,0.1, 0.2, 0.5, 1, 2, 3, 6, 11),
        labels = \(x) 100 * (x -1),
        limits = c(0.01, 20,000)) +
      scale_y_discrete("", labels = function(nm) variables$covariates[nm]) +
      theme_bw() +
      ggtitle(paste0("Effect of various covariates on ", response))
  }
  ggsave(filename = path, plot = gg, device = "png", width = 8, height = 4, units = "in", dpi = 300)
  path
}

make__sample_diagnostic <- function(mod, response, covariate, path, plotfun = "trace") {
  vars <- get_variables(mod)[1:10]
  plot <- mod$fit |> 
    ## bayesplot::mcmc_trace(pars = vars) + theme_bw() 
    rstan::plot(pars = vars, plotfun = plotfun) + theme_bw() 
  ppath <- paste0(path, "_", plotfun, ".png")
  ggsave(filename = ppath,
    plot = plot, device = "png",
    width = 8, height = 6, units = "in", dpi = 300)
  type <- switch(
    plotfun,
    "trace" = "Traceplot",
    "ac" = "Autocorrelation plot",
    "dens" = "Density plot",
    "rhat" = "Rhat plot",
    "n_eff" = "Effective sample size plot"
  )
  plot_cap <- paste0(type, " for the model of ",
    variables$responses[response], " against ",
    variables$covariates[covariate], ".")
  list(plot = ppath, plot_cap = plot_cap)
}

make__ppc_diagnostic <- function(mod, response, covariate, path, plotfun = "dens_overlay") {
  vars <- get_variables(mod)[1:10]
  plot <- mod|> 
    ## bayesplot::mcmc_trace(pars = vars) + theme_bw() 
    pp_check(type = plotfun, ndraws = 100) + theme_bw() 
  ppath <- paste0(path, "_", plotfun, ".png")
  ggsave(filename = ppath,
    plot = plot, device = "png",
    width = 8, height = 6, units = "in", dpi = 300)
  type <- switch(
    plotfun,
    "dens_overlay" = "Density overlay"
  )
  plot_cap <- paste0(type, " for the model of ",
    variables$responses[response], " against ",
    variables$covariates[covariate], ".")
  list(plot = ppath, plot_cap = plot_cap)
}

make_sampler_diagnostics <- function(model, response, covariate, path) {
  path <- str_replace(path, paste0(DATA_PATH, "modelled/"),
    paste0(FIGURES_PATH))
  mod <- readRDS(model)
  ## check the diagnostics
  trace <- make__sample_diagnostic(mod, response, covariate, path, plotfun = "trace")
  ac <- make__sample_diagnostic(mod, response, covariate, path, plotfun = "ac")
  rhat <- make__sample_diagnostic(mod, response, covariate, path, plotfun = "rhat")
  ess <- make__sample_diagnostic(mod, response, covariate, path, plotfun = "ess")
  dens_overlay <- make__ppc_diagnostic(mod, response, covariate, path, plotfun = "dens_overlay")
  
  list(trace = trace, ac = ac, rhat = rhat, ess = ess, dens_overlay = dens_overlay)
}
