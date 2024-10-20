## When there are a large number of combinations of EDA to perform, my
## preference is to create a nested tibble such that each row
## represents a single figure.  The columns would represent:
## - Response
## - Predictors
## - pointer to file containing the ggplot instructions
## - figure caption (as string)

## ---- eda_load_data
dat <- readRDS(paste0(DATA_PATH, "processed/dat.rds"))
## ----end

## ---- eda_plot
eda_plot <- function(dat, response, covariate, pth) {
  err <- try(
  {
    print(response)
    gg <-
      dat |>
      ggplot(aes(x = !!sym(covariate), y = !!sym(response))) +
      geom_point() +
      theme_classic()
    saveRDS(gg, file = pth)
  },
  silent = TRUE
  )
  if (inherits(err, "try-error")) {
    return(NA)
  } else {
    return(gg)
  }
}
## ----end

## ---- eda
eda <- tibble(responses = c("erosionGrazing", "totalErosion")) |>
  crossing(covariates = c("MA.Perc", "CCA.Perc"))
eda <- eda |>
  mutate(
    n = 1:n(),
    N = n()
  ) |>
  mutate(gg = pmap( ## generate gg plot instructions
    .l = list(
      response = responses,
      covariate = covariates,
      n = n,
      N = N
    ),
    .f = ~ {
      response <- ..1
      covariate <- ..2
      n <- ..3
      N <- ..4
      pth <- paste0(FIGURES_PATH, "eda_", response, "_by_", covariate, ".rds")
      cat(paste0(n, "/", N, "\tResponse=", response, ", Covariate=", covariate, "\n\t\t(", pth, ")\n"))
      eda_plot(dat, response = response, covariate = covariate, pth = pth)
    }
  )) |>
  mutate(fig = pmap(    ## save ggplot figures to disc
    .l = list(
      response = responses,
      covariate = covariates,
      gg = gg
    ),
    .f = ~ {
      response <- ..1
      covariate <- ..2
      gg <- ..3
      pth <- paste0(FIGURES_PATH, "eda_", response, "_by_", covariate, ".png")
      ggsave(filename = pth, plot = gg, width = 10, height = 7, units = "in", dpi = 300)
      ggsave(filename = str_replace(pth, ".png", ".pdf"), plot = gg, width = 10, height = 7, units = "in")
    }
  )) |> 
  mutate(cap = pmap( ## generate figure caption
    .l = list(
      response = responses,
      covariate = covariates,
      n = n,
      N = N
    ),
    .f = ~ {
      response <- ..1
      covariate <- ..2
      n <- ..3
      N <- ..4
      cat(paste0(n, "/", N, "\tResponse=", response, ", Covariate=", covariate, "\n"))
      cap <- paste0(response, " against ", covariate, " bla bla bla")
    }
  )) 
save(eda, file = paste0(DATA_PATH, "processed/eda.rds"))
## ----end

eda[1, "gg"][[1]][[1]]
eda[1, "cap"][[1]][[1]]
