#' Show breakpoints (and optionally data) on a map
#' @param dat data.frame with columns year, age, length, lat, long, sex (optional)
#' @param breakpoints data.frame with columns year (optional) lat and long. can be output of {get_Breaks()}. Use -Inf to turn off a break.
#' @param showData logical. do you want to see your datapoints?
#' @return Map
#'   \describe{
#'     \item{\code{p1}}{plot of Alaska with dashed breakpoints and (optionally) raw length observations}
#'   }
#' @export


plot_Breaks <- function(dat, breakpoints, showData = TRUE){
  data(us)

  if(!('year' %in% names(breakpoints))){
    p1 <- ggplot() +
      geom_sf(data = us, fill = NA, color = 'black') +
      {if(showData) geom_point(data = dat, aes(x = long, y= lat, size= length, color = length))} +
      geom_hline(data = subset(breakpoints, lat != -Inf), aes(yintercept = lat), lty = 'dashed')+
      geom_vline(data = subset(breakpoints, long != -Inf), aes(xintercept = long), lty = 'dashed')+
      scale_y_continuous(limits = c(50,71)) +
      scale_x_continuous(limits = c(-185,-130))+
      guides(size = 'none', alpha = 'none')+
      theme_minimal() +
      scale_color_gradient2(low = "blue", mid = "grey90", high = "red", midpoint = mean(dat$length)) +
      labs(color = '', x= '', y = '', title = 'Length Observations & Detected Break(s)' ) +
      theme(legend.position = 'top')

  }
 ## TODO add year func

  return(p1)
}
