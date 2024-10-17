make_arbitrary_rules <- function(fields, rule) {
  rules <- NULL 
  for (i in fields) {
    .rule <- str_replace(rule, "\\.", i)
    rules <- rules |>
      bind_rows(
        data.frame(
          rule = paste0(.rule),
          name = paste0(.rule),
          label = paste0("\"", .rule),
          description = paste0("Value of ", .rule)
        )
      )
  }
  return(rules)
}

make_rules <- function(fields, type = "character") {
  ## Present rules
  pres_rules <- make_presence_rules(fields)
  type_str <- switch(type,
    "character" = "is.character",
    "numeric" = "is.numeric",
    "logical" = "is.logical"
  )
  type_rules <- make_type_rules(fields, type_str)
  return(rbind(pres_rules, type_rules))
}

make_presence_rules <- function(fields) {
  rules <- NULL 
  for (i in fields) {
    rules <- rules |>
      bind_rows(
        data.frame(
          rule = paste0("\"", i, "\" %in% names(.)"),
          name = paste0("\"", i, "\""),
          label = paste0("\"", i, " absence\""),
          description = paste0("Field called \"", i, "\" must be present")
        )
      )
  }
  return(rules)
}
make_type_rules <- function(fields, type_str) {
  rules <- NULL
  type <- str_replace(type_str, "is.", "")
  for (i in fields) {
    rules <- rules |>
      bind_rows(
        data.frame(
          rule = paste0(type_str, "(", i, ")"),
          name = i,
          label = paste0(i, " ", type),
          description = paste0("Field called \"", i, "\" must be ", type)
        )
      )
  }
  return(rules)
} 

validate_data <- function(data, rules, on_error = "stop") {
  result <- confront(data, rules)
  invalid_rows <- violating_rows(result, data)
  val <- tidy_validation(result, rules)
  if (any(invalid_rows$wch_row)) {
    saveRDS(invalid_rows$violating,
      file = paste0(DATA_PATH, "primary/invalid_rows.rds")
    )
    if (on_error == "stop") {
      stop(paste0("\n",
        paste0("\tValidation failed: ",
          val$description[invalid_rows$wch_row],
          collapse = "\n"
        ), "\nWe strongly recommend to read in the ",
        paste0(DATA_PATH, "primary/invalid_rows.rds"),
        " file to explore the invalid rows."
      ))
    } else {
      return(list(violating = invalid_rows$violating, summary = val))
    }
  } else {
    return(val)
  }
}

violating_rows <- function(result, data) {
  s <- summary(result)
  wch <- which(s$items > 1 & s$fails > 1)
  if (length(wch) == 0) {
    return(NULL)
  } else {
    return(list(
      violating = violating(data, result[wch]),
      wch_row = wch
    ))
  }
}

tidy_validation <- function(result, rules) {
  summary(result) |>
    mutate(description = description(rules)) |>
    dplyr::select(description, everything(), -name, -expression)
  }

