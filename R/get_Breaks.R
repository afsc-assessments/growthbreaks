#' Wrapper function to to fit gams & evaluate first derivatives
#' @param dat data.frame with columns year, age, length, lat, long, sex (optional)
#' @param ages_to_use optional. vector of age(s) to evaluate for breakpoints.
#' @param sex logical.
#' @param axes do you want to evaluate axes for space only (0, default), time only (1), or both simultaneously (2)?
#' @param showPlot logical. do you want to see the detected break(s) on a map? Applicable only for axes options 0 or 2.
#' @import mgcv
#' @return matrix of detected breakpoints and plots
#' @examples  \dontrun{ data(simulated_data) ;get_Breaks(dat = simulated_data, ages_to_use = c(15:20), axes = 0)
#' }
#' @export


get_Breaks <- function(dat, ages_to_use=c(5,10),
                       sex = FALSE,
                       axes = 0,
                       showPlot = TRUE){

  if(is.null(ages_to_use)) ages_to_use = unique(dat$ages)

  if(axes == 0){
    Terms <- c('lat','long')
  } else if(axes == 1){
    Terms <- 'year'
  } else{
    Terms <- c('lat','long','year')
  }
  ## parse data at defined breaks
  if(!sex){
    age_split <-   dat %>%
      group_by(age) %>%
      group_split()

  } else{
    age_split <-   dat %>%
      group_by(age,sex) %>%
      group_split()
    ## TODO sort how to index when split by sex
  }

  newD <- data.frame( year = seq(min(dat$year),max(dat$year),length = 100),
                      long = seq(min(dat$long),max(dat$long), length = 100),
                      lat = seq(min(dat$lat),max(dat$lat),length = 100))[Terms] %>%
    mutate(detected_break = NA, count = 0)

  for(iage in seq_along(ages_to_use)){ ## loop over key ages

    ##TODO fill in breakpoints$sex here breakpoints$sex <- ifelse(sex)

    dat_use <- age_split[ages_to_use][[iage]]
    ##TODO split by sex here breakpoints$sex <- ifelse(sex)

    if(axes == 0)  mod <- mgcv::gam(length ~ s(lat) + s(long), data = dat_use)
    if(axes == 1)  mod <- mgcv::gam(length ~ s(year, bs = "cc"), data = dat_use)
    if(axes == 2)  mod <- mgcv::gam(length ~ s(year, bs = "cc") + s(lat) + s(long),data = dat_use)

    ## get & eval derivatives ----

    pTerm <- predict(mod, newdata = dat_use, type = "terms", se.fit = TRUE) ## predict on original data
    p2 <- predict(mod, newdata = dat_use) ## raw predicts
    df.res <- df.residual(mod)
    crit.t <- qt(0.025, df.res, lower.tail = FALSE)
    ## make dataframe with CIs given by additive term-specific variances
    pdat <-  dat_use %>%
      mutate(predLen = p2,
             variance_total = rowSums(pTerm$se.fit),
             lower = predLen - (crit.t * (variance_total)),
             upper = predLen + (crit.t * (variance_total)))

    for(t in 1:length(Terms)){
      Term <- Terms[t]

      m2.d <- Deriv(mod, newdata = newD)
      m2.dci <-  confint_Deriv(m2.d, term = Term)

      crit.eval = quantile(probs = c(0.025, 0.975), x =  m2.d[[Term]]$deriv) ## use tails
      crit.eval.se = quantile(probs = c(0.025, 0.975), x =  m2.d[[Term]]$se.deriv) ## use tails

      ## identify where mean crosses zero or falls out of bounds (NAs where it does)
      m2.dsig.zeros <- check_Deriv(x = m2.d$eval[[Term]],
                                   d = m2.d[[Term]]$deriv,
                                   upper = m2.dci[[Term]]$upper,
                                   lower = m2.dci[[Term]]$lower,
                                   crit.eval = crit.eval,
                                   eval = 0)

      # pix <- !is.na(m2.dsig.zeros)
      vals <- m2.d$eval[[Term]][!is.na(m2.dsig.zeros)]
      newD$detected_break[newD[,Term] %in% vals] <- TRUE ## flag the rows of detected breaks
      newD$detected_break[newD[,Term] == min(newD[,Term])] <- NA ## overwrite edge cases
      newD$detected_break[newD[,Term] == max(newD[,Term])] <- NA ## overwrite edge cases
      newD$count[newD[,Term] %in% vals] <- newD$count[newD[,Term] %in% vals]+1 ## add how many combos flagged

    } ## end terms
    ## TODO end sexes

  } ## end key ages

  breakpoints <- newD[!is.na(newD$detected_break),]
  breakpoints$count <-  breakpoints$count/length(ages_to_use)

  if(axes!=1){
    data(us)
    p1 <- ggplot() +

      # geom_hline(data = breakpoints, aes(yintercept = lat, alpha = count), lty = 'dashed')+
      # geom_vline(data = breakpoints, aes(xintercept = long, alpha = count), lty = 'dashed')+
      geom_sf(data = us, fill = NA, color = 'black') +
      geom_point(data = dat, aes(x = long, y= lat, size= length, color = length))+
      geom_hline(data = breakpoints, aes(yintercept = lat), lty = 'dashed')+
      geom_vline(data = breakpoints, aes(xintercept = long), lty = 'dashed')+
      # scale_y_continuous(limits = 2+c(floor(min(dat$lat)),ceiling(max(dat$lat)))) +
      # scale_x_continuous(limits = 2+c(floor(min(dat$long)),ceiling(max(dat$long)))) +
      scale_y_continuous(limits = c(50,71)) +
      scale_x_continuous(limits = c(-185,-130))+
      guides(size = 'none', alpha = 'none')+
      theme_minimal() +
      scale_color_gradient2(low = "blue", mid = "grey90", high = "red", midpoint = mean(dat$length)) +
      labs(color = '', x= '', y = '', title = 'Length Observations & Detected Break(s)' ) +
      theme(legend.position = 'top')

  }
  if(showPlot & axes != 1){
    print(p1)
    print(p2)
  }

  return(breakpoints)

}

# breakpoints <- get_Breaks(dat = simulated_data, axes = 0, ages_to_use = 15:20, showPlot = TRUE)
