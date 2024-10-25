## When there are a large number of combinations of EDA to perform, my
## preference is to create a nested tibble such that each row
## represents a single figure.  The columns would represent:
## - Response
## - Predictors
## - pointer to file containing the ggplot instructions
## - figure caption (as string)

## ---- eda_load_data
dat <- readRDS(paste0(DATA_PATH, "processed/dat.rds"))
variables <- readRDS(paste0(DATA_PATH, "processed/variables.rds"))
## ----end

## ---- eda_plot
### All data
eda_plot <- function(dat, response, covariate, pth) {
  err <- try(
  {
    gg <-
      dat |>
      ggplot(aes(x = !!sym(covariate), y = !!sym(response))) +
      geom_point() +
      theme_classic() +
      scale_x_continuous(name = variables$covariates[[covariate]]) +
      scale_y_continuous(name = variables$responses[[response]]) 
    saveRDS(gg, file = pth)
  },
  silent = TRUE
  )
  if (inherits(err, "try-error")) {
    print("error")
    return(NA)
  } else {
    return(gg)
  }
}

### By region
eda_plot_region <- function(dat, response, covariate, pth) {
  err <- try(
  {
    gg <-
      dat |>
      ggplot(aes(x = !!sym(covariate), y = !!sym(response))) +
      geom_point() +
      facet_wrap(~Region) +
      theme_classic() +
      scale_x_continuous(name = variables$covariates[[covariate]]) +
      scale_y_continuous(name = variables$responses[[response]]) 
    saveRDS(gg, file = pth)
  },
  silent = TRUE
  )
  if (inherits(err, "try-error")) {
    print("error")
    return(NA)
  } else {
    return(gg)
  }
}

### By Reef, Depth and Inshore
eda_plot_reef_depth <- function(dat, response, covariate, pth) {
  err <- try(
  {
    gg <-
      dat |>
      ggplot(aes(x = !!sym(covariate), y = !!sym(response), 
      colour = as.factor(Depth), fill = as.factor(Depth))) +
      geom_point() +
      facet_wrap(~Reef + Inshore) +
      theme_classic() +
      scale_x_continuous(name = variables$covariates[[covariate]]) +
      scale_y_continuous(name = variables$responses[[response]]) + 
      scale_color_manual(name = "Depth (m)",
                         values = c("#909800", "#00AD9A"),
                         aesthetics = c("colour", "fill"))
    saveRDS(gg, file = pth)
  },
  silent = TRUE
  )
  if (inherits(err, "try-error")) {
    print("error")
    return(NA)
  } else {
    return(gg)
  }
}
## ----end

## It might pay to parallelise the following with future

## ---- eda
## eda <- tibble(responses = c("erosionGrazing", "totalErosion")) |>
eda <- tibble(responses = names(variables$responses)) |>
  crossing(covariates = names(variables$covariates))

### All data 
eda_all <- eda |>
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
      pth <- paste0(FIGURES_PATH, "eda_all_", response, "_by_", covariate, ".rds")
      cat(paste0(n, "/", N, "\tResponse=", response, ", Covariate=", covariate, "\n\t\t(", pth, ")\n"))
      eda_plot(dat, response = response, covariate = covariate, pth = pth)
    }
  )) |>
  mutate(fig = pmap(    ## save ggplot figures to disc
    .l = list(
      response = responses,
      covariate = covariates,
      gg = gg,
      n = n,
      N = N
    ),
    .f = ~ {
      response <- ..1
      covariate <- ..2
      gg <- ..3
      n <- ..4
      N <- ..5
      cat(paste0(n, "/", N, "\tResponse=", response, ", Covariate=", covariate, "\n"))
      pth <- paste0(FIGURES_PATH, "eda_all_", response, "_by_", covariate, ".png")
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
      pth <- paste0(FIGURES_PATH, "eda_all_", response, "_by_", covariate, ".pdf")
      cap <- paste0(response, " against ", covariate, ". The figure is saved in", pth)
    }
  )) 
saveRDS(eda_all, file = paste0(DATA_PATH, "processed/eda_all.rds"))
## ----end

### By region 
eda_region <- eda |>
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
      pth <- paste0(FIGURES_PATH, "eda_region_", response, "_by_", covariate, ".rds")
      cat(paste0(n, "/", N, "\tResponse=", response, ", Covariate=", covariate, "\n\t\t(", pth, ")\n"))
      eda_plot_region(dat, response = response, covariate = covariate, pth = pth)
    }
  )) |>
  mutate(fig = pmap(    ## save ggplot figures to disc
    .l = list(
      response = responses,
      covariate = covariates,
      gg = gg,
      n = n,
      N = N
    ),
    .f = ~ {
      response <- ..1
      covariate <- ..2
      gg <- ..3
      n <- ..4
      N <- ..5
      cat(paste0(n, "/", N, "\tResponse=", response, ", Covariate=", covariate, "\n"))
      pth <- paste0(FIGURES_PATH, "eda_region_", response, "_by_", covariate, ".png")
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
      pth <- paste0(FIGURES_PATH, "eda_region_", response, "_by_", covariate, ".pdf")
      cap <- paste0(response, " against ", covariate, " per region. The figure is saved in", pth)
    }
  )) 
saveRDS(eda_region, file = paste0(DATA_PATH, "processed/eda_region.rds"))
## ----end

### By reef and depth
eda_reef_depth <- eda |>
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
      pth <- paste0(FIGURES_PATH, "eda_reef_depth_", response, "_by_", covariate, ".rds")
      cat(paste0(n, "/", N, "\tResponse=", response, ", Covariate=", covariate, "\n\t\t(", pth, ")\n"))
      eda_plot_reef_depth(dat, response = response, covariate = covariate, pth = pth)
    }
  )) |>
  mutate(fig = pmap(    ## save ggplot figures to disc
    .l = list(
      response = responses,
      covariate = covariates,
      gg = gg,
      n = n,
      N = N
    ),
    .f = ~ {
      response <- ..1
      covariate <- ..2
      gg <- ..3
      n <- ..4
      N <- ..5
      cat(paste0(n, "/", N, "\tResponse=", response, ", Covariate=", covariate, "\n"))
      pth <- paste0(FIGURES_PATH, "eda_reef_depth_", response, "_by_", covariate, ".png")
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
      pth <- paste0(FIGURES_PATH, "eda_reef_depth_", response, "_by_", covariate, ".pdf")
      cap <- paste0(response, " against ", covariate, "by reef and depth. TRUE and FALSE indicates if the reef is located on the inshore shelf (TRUE) or not (FALSE). The figure is saved in", pth)
    }
  )) 
saveRDS(eda_reef_depth, file = paste0(DATA_PATH, "processed/eda_reef_depth.rds"))
## ----end

### Correlation between potential covariates 

library(corrplot)
dat_cov <- dat |>
dplyr::select(names(variables$covariates)) |>
cor()

png(file = paste0(FIGURES_PATH, "correlation_covariates.png"),
        width = 200, height = 200, units='mm', res = 150)
cor_cov <- { corrplot(dat_cov, type = 'lower', diag = FALSE, order = 'hclust', 
        tl.col = 'black', cl.ratio = 0.2, tl.srt = 45, 
        col = COL2('PuOr', 10));
        recordPlot()
}
dev.off()

### Correlation between potential responses 

dat_resp <- dat |>
dplyr::select(names(variables$response)) |>
cor(use="complete.obs")

png(file = paste0(FIGURES_PATH, "correlation_responses.png"),
        width = 200, height = 200, units='mm', res = 150)
cor_resp <- { corrplot(dat_resp, type = 'lower', diag = FALSE, order = 'hclust', 
        tl.col = 'black', cl.ratio = 0.2, tl.srt = 45, 
        col = COL2('PRGn'));
        recordPlot()
}
dev.off()

### Distribution of responses 

dat_resp_summary <- dat |>
dplyr::select(names(variables$response)) #|>
#skim()

saveRDS(dat_resp_summary, file = paste0(DATA_PATH, "processed/data_summary.rds"))


