#' Load, check and plot input data for use in breakpoint detection
#' @param dat data.frame with columns year, age, length, sex (optional)
#' @param sex logical. does your data frame include a sex column?
#' @param showPlot logical. do you want to visualize your input data?
#' @return plots
#' @export



check_data <- function(dat, sex = FALSE, showPlot = TRUE) {
  if(sex) stop("Cannot currently handle sex structure, set to FALSE")

  ## header checks
  colheads <- tolower(names(dat));names(dat) <- colheads
  colneeds <- c("year","age","length",  "long" ,  "lat" )
  if(sex){  colneeds <- c(colneeds,'sex')}
  if(!all(colneeds %in% colheads)) stop(paste0("missing one of ",colneeds))
  if(!sex & !all( apply(dat[,colneeds],2,is.numeric))) stop("all columns should be numeric")
  if(sex){
    if(!all(apply(dat[,colneeds] %>% select(-"sex"),2,is.numeric))){
      stop("all columns besides sex should be numeric")
    }
  }
  ## spatial extent check
  if(min(dat$long) < 180){
    showPlot <-  FALSE
    warning("longitude should have a minimum of -180 for plotting to work. disabling plots.")
  }

  ## dimension checks
  if(!sex){
    agedims <- summarise(dat, n = n(), .by = c('age'))
    yeardims <- summarise(dat, n = n(), .by = c('year'))
    bothdims <- summarise(dat, n = n(), .by = c('age','year'))
  }else{
    agedims <- summarise(dat, n = n(), .by = c('age','sex'))
    yeardims <- summarise(dat, n = n(), .by = c('year','sex'))
    bothdims <- summarise(dat, n = n(), .by = c('age','year','sex'))
  }

  if(any(agedims$n< 10)) warning(paste0('low sample sizes for ages ',  unique(agedims$age[agedims$n < 10])))
  if(any(yeardims$n< 10)) warning(paste0('low sample sizes for years ',  unique(yeardims$year[yeardims$n < 10])))
  if(any(bothdims$n < 5)) warning(paste0('low sample sizes for years ',  unique(bothdims$year[bothdims$n < 10])))

  dat_plot <- dat %>%
    mutate(meanL = mean(length), .by = ifelse(sex, c('sex','age'), 'age')) %>%
    mutate(resid = length-meanL)




  ## plot raw length-age obs

  p1 <- ggplot(dat_plot, aes(x = age, y = length, color = factor(year))) +
    geom_point() +
    theme_minimal()+ theme(legend.position = 'none')+
    labs(x = 'Age', y = 'Length', color = 'year', title = 'Raw length-at-age observations')

  ## plot raw length-age map
  data(us)

  p2 <- ggplot() +
    geom_sf(data = us, fill = NA, color = 'black') +
    geom_point(data = dat_plot, aes(x = long, y= lat, size= length, color = length))+
    # scale_y_continuous(limits = 2+c(floor(min(dat$lat)),ceiling(max(dat$lat)))) +
    # scale_x_continuous(limits = 2+c(floor(min(dat$long)),ceiling(max(dat$long)))) +
    scale_y_continuous(limits = c(50,71)) +
    scale_x_continuous(limits = c(-185,-130))+
    guides(size = 'none')+
    theme_minimal() +
    scale_color_gradient2(low = "blue", mid = "grey90", high = "red", midpoint = mean(dat$length)) +
    labs(color = '', x= '', y = '', title = 'Raw Length Observations') +
    theme(legend.position = 'top')


  ## plot residual map
  p3 <- ggplot() +
    geom_sf(data = us, fill = NA, color = 'black') +
    geom_point(data = dat_plot, aes(x = long, y= lat, size= resid, color = resid))+
    # scale_y_continuous(limits = 2+c(floor(min(dat$lat)),ceiling(max(dat$lat)))) +
    # scale_x_continuous(limits = 2+c(floor(min(dat$long)),ceiling(max(dat$long)))) +
    scale_y_continuous(limits = c(50,71)) +
    scale_x_continuous(limits = c(-185,-130))+
    guides(size = 'none')+
    theme_minimal() +
    scale_color_gradient2(low = "blue", mid = "grey90", high = "red", midpoint = mean(dat_plot$resid)) +
    labs(color = '', x= '', y = '', title = 'Length Residuals') +
    theme(legend.position = 'top')


  if(showPlot){
    print(p1); print(p2);print(p3)
  }
  return(list(p1,p2,p3))
}


# check_data(dat)
