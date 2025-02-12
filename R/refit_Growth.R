#' Re-fit growth data at putative breaks and return estimates for validation
#' @param dat data.frame with columns year, age, length, lat, long, sex (optional)
#' @param breakpoints data.frame with columns year and/or lat and long. can be output of {get_Breaks()}.
#' @param showPlot logical. do you want to see plots of the fitted curves?
#' @return Von B growth parameters at input breakpoints; plots with uncertainty of growth curves:
#'   \describe{
#'     \item{\code{$split_tables}}{list of tables of input data split by strata specified in breakpoints}
#'     \item{\code{$fits_df}}{input data, estimates and associated standard errors as single dataframe}
#'     \item{\code{$pars_df}}{Parameter estimates and associated standard errors}
#'     \item{\code{$fits_plot}}{input observations and fitted growth curves, by strata}
#'     \item{\code{$pars_plot}}{Parameter estimates and associated standard errors; red lines indicate statistically significant differences}
#'   }
#' @export

refit_Growth <- function(dat = simulated_data, breakpoints, selex = FALSE, showPlot = TRUE){
  if(selex) stop("Cannot currently handle selectivity, input must be FALSE.")
  if(any(is.na(breakpoints))) stop('Cannot handle NA in breakpoints. Did you mean to use +/-Inf?')
  # Apply the function to each row of df2
  split_tables <- purrr::map(1:nrow(breakpoints), function(i) {
    generate_conditions(df1=dat, row = breakpoints[i, ])
  })

  # Flatten the list of lists
  split_tables <- flatten(split_tables)

  # Combine all elements of split_tables into a single data frame
  combined_df <- bind_rows(
    purrr::map2(split_tables, names(split_tables), ~mutate(.x, DES = .y))
  )

  ## check dimensions and warn
  combined_df_dims <- combined_df %>% summarise(n=n(), .by = 'DES')
  if(any(combined_df_dims$n < 50)) warning("very few datapoints in some strata; consider dropping a break")

  combined_df$Sel <- 1 ## TODO include the penalty later for optional length selex
  combined_df$selType <- 1 ## TODO include the penalty later for optional length selex

  non_empty <- which(lapply(split_tables, nrow) >0)
  split_tables <- split_tables[non_empty]
  nStrata <- length(split_tables)
  strata_names <- names(split_tables)
  strata_factor <- as.numeric(as.factor(combined_df$DES))-1

  ## this will assign a unique DES depending on period X sex X region -- whatever is in DES
  data <-
    list(
      Length_cm = combined_df[,"length"],
      Age = combined_df[,"age"],
      DES = as.vector(strata_factor), ## keep this for master iterations
      selType = combined_df[,'selType'],
      Sel = combined_df[,'Sel'],
      nStrata = nStrata,
      a2 = 30
    )

  parameters <-
    list(
      log_Linf = rep(log(summary(combined_df$length)[5]), nStrata),
      log_k = rep(log(0.5), nStrata),
      t0 = rep(0.1, nStrata),
      log_Sigma = rep(log(2), nStrata)
    )

  # Now estimate everything
  map <- NULL

  # TMB::compile(here::here("src","growthbreaks.cpp"))
  # dyn.load(TMB::dynlib(here::here("src","growthbreaks")))
  model <- TMB::MakeADFun(data, parameters,  DLL="growthbreaks",silent=T,map=map)
  fit <- nlminb(
    model$par,
    model$fn,
    model$gr,
    control = list(
      rel.tol = 1e-12,
      eval.max = 100000,
      iter.max = 10000
    )
  )
  for (k in 1:3)  fit <- nlminb(model$env$last.par.best, model$fn, model$gr) ## start at last-best call, for stability
  model$report()$denominator ## if we only ran seltype 2 points, this should NOT be 1.0
  model$report()$AIC
  best <- model$env$last.par.best
  rep <- TMB::sdreport(model)

  ## data frame with observed and predicted values
  fits_df <-  bind_cols(ypred =  rep$value[names(rep$value)=='ypreds'],
                        ypred_sd = rep$sd[names(rep$value)=='ypreds'],
                        combined_df) %>%
    mutate(lower = ypred - 1.96*ypred_sd^2,
           upper = ypred + 1.96*ypred_sd^2)

  ## data frame with parameter estimates
  rep0 <-  bind_cols(variable =  names(rep$value)[names(rep$value)!='ypreds'],
                     value =  rep$value[names(rep$value)!='ypreds'],
                     value_sd =  rep$sd[names(rep$value)!='ypreds'],
                     strata = rep(strata_names, 6)) %>%
    mutate(lower = value - 1.96*value_sd^2,
           upper = value + 1.96*value_sd^2)
  pars_df <- rep0 %>%
    filter(!(variable %in% c('L1','L2'))) %>%
    dplyr::select(strata, variable, value, value_sd, lower, upper)

  ## check overlap across strata
  # Initialize the matchcol column with FALSE
  pars_df$matchcol <- FALSE

  # Loop through each row of the dataframe
  for (i in 1:nrow(pars_df)) {
    # Get the current row
    current_row <- pars_df[i, ]

    # Get the rows with the same variable but different strata
    other_rows <- pars_df[pars_df$variable == current_row$variable & pars_df$strata != current_row$strata, ]

    # Check if the value is within the lower and upper bounds of any other row
    if (any(current_row$value >= other_rows$lower & current_row$value <= other_rows$upper)) {
      pars_df$matchcol[i] <- TRUE
    }
  }

  ## panel plot of par ests and CIs
  p1 <- ggplot(pars_df, aes(x = strata, y = value, color = matchcol)) +
    geom_point()+
    geom_errorbar(width = 0, aes(ymin = lower, ymax = upper)) +
    scale_color_manual(values = c('red','black'))+
    theme_minimal()+
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 90))+
    labs(x = 'strata', y = '') +
    facet_wrap(variable~., scales = 'free_y')

  ## panel plot of obs and fits
  ## TODO include sigma around estimates
  p2 <- ggplot(fits_df, aes(x = age, y = length, color = DES)) +
    geom_point()+
    scale_color_manual(values = cols)+
    geom_line(aes(y = ypred), color = 'black')+
    # geom_ribbon(aes(ymin = lower, ymax = upper))+
    theme_minimal()+
    labs(x = 'strata', y = '') +theme(legend.position = 'none') +
    facet_wrap(~DES)
  if(showPlot){
    print(p1);print(p2)
  }

  return(list("split_tables" = split_tables,
              "fits_df" = fits_df,
              "pars_df" = pars_df,
              "AIC" =  model$report()$AIC,
              "pars_plot" = p1,
              "fits_plot" = p2))


}
