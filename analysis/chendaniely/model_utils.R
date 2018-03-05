recode_never_someevery <- function(value) {
  if (is.na(value)) {
    return(NA)
  } else if (value == 'No, never') {
    return(value)
  } else if (value %in% c('Yes, some years', 'Yes, every year')) {
    return('Yes, some or every year')
  } else {
    return(NA)
  }
}

p_star <- function(pvalue) {
  if (pvalue <= .001) {
    return('***')
  } else if (pvalue <= .01) {
    return('**')
  } else if (pvalue <= .05) {
    return('*')
  } else if (pvalue <= .1) {
    return('.')
  } else {
    return('')
  }
}

logistic_or <- function(mod) {
  mod_res <- broom::tidy(mod)
  mod_res$or <- exp(mod_res$estimate)
  # mod_res$or_std_err <- exp(mod_res$std.error)
  mod_res$or_lower <- exp(mod_res$estimate - 1.96 * mod_res$std.error)
  mod_res$or_upper <- exp(mod_res$estimate + 1.96 * mod_res$std.error)
  mod_res$term <- stringr::str_replace(mod_res$term, 'as.factor', '')
  
  mod_res$sig <- sapply(mod_res$p.value, p_star)
  
  mod_res <- dplyr::select(mod_res, term, or, sig, or_lower:or_upper, estimate:p.value)
  return(mod_res)
}

fit_all_dat <- function(formula, ...) {
  mod_list <- list()
  for (d in list(...)) {
    mod <- survey::svyglm(formula,
                          design = d,
                          family = quasibinomial(link = "logit"))
    print(summary(mod))
    mod_res <- logistic_or(mod)
    print(mod_res, digits = 4)
    print('********************************************************************')
    mod_list <- rlist::list.append(mod_list, mod)
  }
  return(mod_list)
}

print_svy_mod <- function(mod) {
  print(summary(mod))
  mod_res <- logistic_or(mod)
  print(mod_res, digits = 4)
}
