% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refit_Growth.R
\name{refit_Growth}
\alias{refit_Growth}
\title{Re-fit growth data at putative breaks and return estimates for validation}
\usage{
refit_Growth(dat = simulated_data, breakpoints, selex = FALSE, showPlot = TRUE)
}
\arguments{
\item{dat}{data.frame with columns year, age, length, lat, long, sex (optional)}

\item{breakpoints}{data.frame with columns year and/or lat and long. can be output of {get_Breaks()}.}

\item{showPlot}{logical. do you want to see plots of the fitted curves?}
}
\value{
Von B growth parameters at input breakpoints; plots with uncertainty of growth curves:
  \describe{
    \item{\code{$split_tables}}{list of tables of input data split by strata specified in breakpoints}
    \item{\code{$fits_df}}{input data, estimates and associated standard errors as single dataframe}
    \item{\code{$pars_df}}{Parameter estimates and associated standard errors}
    \item{\code{$fits_plot}}{input observations and fitted growth curves, by strata}
    \item{\code{$pars_plot}}{Parameter estimates and associated standard errors; red lines indicate statistically significant differences}
  }
}
\description{
Re-fit growth data at putative breaks and return estimates for validation
}
