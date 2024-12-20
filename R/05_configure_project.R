## ---- load_libraries
library(tidyverse)
library(validate)
library(skimr)
library(brms)
library(rstan)
library(cmdstanr)
library(tidybayes)
library(insight)
library(bayesplot)
library(parameters)
library(HDInterval)
library(modelr)
source("../R/processing_functions.R")
source("../R/simple_modelling_functions.R")
## ----end

## ---- define global variables
assign("DATA_PATH", "../data/", envir = .GlobalEnv)
assign("RAW_DATA_PATH", paste0(DATA_PATH, "raw/"), envir = .GlobalEnv)
assign("OUTPUT_PATH", "../output/", envir = .GlobalEnv)
assign("FIGURES_PATH", paste0(OUTPUT_PATH, "figures/"), envir = .GlobalEnv)
assign("TABLES_PATH", paste0(OUTPUT_PATH, "tables/"), envir = .GlobalEnv)
## ----end

## ---- setup_directories
if (!dir.exists(paste0(DATA_PATH))) dir.create(paste0(DATA_PATH))
if (!dir.exists(paste0(DATA_PATH, "/primary"))) dir.create(paste0(DATA_PATH, "/primary"))
if (!dir.exists(paste0(DATA_PATH, "/processed"))) dir.create(paste0(DATA_PATH, "/processed"))
if (!dir.exists(paste0(DATA_PATH, "/modelled"))) dir.create(paste0(DATA_PATH, "/modelled"))

if (!dir.exists(paste0(OUTPUT_PATH))) dir.create(paste0(OUTPUT_PATH))
if (!dir.exists(paste0(FIGURES_PATH))) dir.create(paste0(FIGURES_PATH))
if (!dir.exists(paste0(TABLES_PATH))) dir.create(paste0(TABLES_PATH))
## ----end
