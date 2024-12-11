#' Function to re-fit growth data at putative breaks and return estimates for validation
#' @param dat data.frame with columns year, age, length, lat, long, sex (optional)
#' @param breakpoints data.frame with columns year and/or lat and long. can be output of {get_Breaks}.
#' @param break_by optional. specify a certain term (column) of breakpoints for re-fitting curves; otherwise it will use all columns.
#' @param showPlot logical. do you want to see plots of the fitted curves?

#' @return Von B growth parameters at input breakpoints; plots with uncertainty of growth curves
#' @export
#'
#'
refit_Growth <- function(dat = simulated_data, breakpoints, showPlot = TRUE){

  # if(mean(dat) > 1000) dat$length/1000; cat('divided input lengths by 1000 \n')


  # Apply the function to each row of df2
  split_tables <- map(1:nrow(breakpoints), function(i) {
    generate_conditions(df1=dat, row = breakpoints[i, ])
  })

  # Flatten the list of lists
  split_tables <- flatten(split_tables)

  ## estimate the VB pars
  par_est <- optim(par = c(2000,0.3,1,30),
                   method="L-BFGS-B",
        fn = vonB_optim,
        lower = c(1,0,0,0),
        # upper = c(max(dat$length)*2,Inf, max(dat$age),Inf),
        age = split_tables[[1]]$age,
        obs = split_tables[[1]]$length)

  output <- NULL
  output$group <- names(split_tables)[[1]]
  output$linf <-  par_est$par[1]
  output$k <-  par_est$par[2]
  output$t0 <-  par_est$par[3]
  output$sigma <-  par_est$par[4]

  growth_est <- vonB(linf = par_est$par[1],k=par_est$par[2],t0=par_est$par[3],
                     age = unique(dat$age))

  ## estimate parameters with CIs using optim/nls
  ## plot the resultant obs & estimates



}
