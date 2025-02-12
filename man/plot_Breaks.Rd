% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_Breaks.R
\name{plot_Breaks}
\alias{plot_Breaks}
\title{Show breakpoints (and optionally data) on a map}
\usage{
plot_Breaks(dat, breakpoints, showData = TRUE)
}
\arguments{
\item{dat}{data.frame with columns year, age, length, lat, long, sex (optional)}

\item{breakpoints}{data.frame with columns year (optional) lat and long. can be output of {get_Breaks()}. Use -Inf to turn off a break.}

\item{showData}{logical. do you want to see your datapoints?}
}
\value{
Map
  \describe{
    \item{\code{p1}}{plot of Alaska with dashed breakpoints and (optionally) raw length observations}
  }
}
\description{
Show breakpoints (and optionally data) on a map
}
