## ---- make rules
char_fields <- c("Reef", "Region")
rules_char <- make_rules(char_fields, type = "character")

num_fields <- c(
  "Reef.Nr",
  "Lat_degS",
  "Lon_degE",
  "WQ",
  "Depth",
  "ClaySilt.Percent",
  "cur_mean",
  "WQ.Ind",
  "Secchi.mean",
  "Kd_490.mean",
  "PAR_z.24h.mean",
  "DOR_C.mean",
  "Oxygen.mean",
  "pco2surf.mean",
  "PH.mean",
  "alk.mean",
  "Chl_a_sum.mean",
  "salt.mean",
  "temp.mean",
  "DIN.mean",
  "DIP.mean",
  "EPO.mean",
  "FineSed.mean",
  "FineSed.mg.L",
  "Dust.mean",
  "TN.mean",
  "TP.mean",
  "Turbidity.mean",
  "Fluorescence.mean",
  "omega_ar.mean",
  "AIMS_tag",
  "NOAA_tag",
  "durationDays",
  "durationYears",
  "MA.Perc",
  "TA.Perc",
  "CCA.Perc",
  "Abiotic.Perc",
  "Oth.calc.Perc",
  "SP.Perc",
  "Oth.Noncalc.Perc",
  "Calc.Perc",
  "NonCalc.Perc",
  "preweightBlock",
  "preRWDensity",
  "preSA",
  "preweightEpoxied",
  "preCTVolume",
  "preCTDensity",
  "postweightDirty",
  "postweightClean",
  "postCTVolume",
  "postCTDensity",
  "volDifference",
  "densityDifference",
  "erosionAnnelid",
  "erosionVermetid",
  "erosionWormlike",
  "erosionbivalve",
  "erosionSponge",
  "totalmacroErosion",
  "erosionGrazing",
  "totalErosion",
  "accretionBivalve",
  "accretionBryozoan",
  "accretionCCA",
  "accretionCoral",
  "accretionMollusk",
  "accretionVermetid",
  "accretionAnnelid",
  "accretionWormlike",
  "totalAccretion",
  "VolumeDiff.Perc",
  "MassDiff.Perc",
  "DensityDiff.Perc",
  "VolumeDiff.Perc.3Yr",
  "MassDiff.Perc.3Yr",
  "DensityDiff.Perc.3Yr",
  "Resp.ugO2.cm2.hr",
  "Dark_Calcification_umolCaCO3.cm2.hr",
  "Dark_Calcification_ugCaCO3.cm2.hr",
  "DIC0_umol.cm2.hr",
  "P200.ugO2.cm2.hr",
  "G200_umolCaCO3.cm2.hr",
  "G200_ugCaCO3.cm2.hr",
  "DIC200_umol.cm2.hr",
  "P500.ugO2.cm2.hr",
  "G500_umolCaCO3.cm2.hr",
  "G500_ugCaCO3.cm2.hr",
  "DIC500_umol.cm2.hr",
  "PR.G"
)
rules_num <- make_rules(num_fields, type = "numeric")

bool_fields <- c("Inshore")
rules_bool <- make_rules(bool_fields, type = "logical")

gt0_fields <- c("erosionGrazing", "totalErosion", "totalAccretion")
rules_gt0 <- make_arbitrary_rules(gt0_fields, rule = ". > 0")

lt0_fields <- c("VolumeDiff.Perc.3Yr")
rules_lt0 <- make_arbitrary_rules(lt0_fields, rule = ". < 0")

rules <- rbind(rules_char, rules_num, rules_bool, rules_gt0, rules_lt0)
## ----end

## ---- validate
rules <- validator(.data = rules)
## rules <- validator(.file = paste0(DATA_PATH, "raw/validation.yaml"))
validation_results <- validate_data(data = dat, rules = rules, on_error = "show")
saveRDS(validation_results, file = paste0(DATA_PATH, "primary/validation_results.rds"))
## ----end

if (any(validation_results$summary$fails > 0)) {
  validation_results$summary |>
    filter(fails > 0) |>
    knitr::kable() |>
    print()
  cat("\nPlease see validation_results$violating to explore the failing rows.\n")
  validation_results$violating
}
