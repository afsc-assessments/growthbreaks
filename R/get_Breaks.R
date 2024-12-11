#' Wrapper function to to fit gams & evaluate first derivatives
#' @param dat data.frame with columns year, age, length, lat, long, sex (optional)
#' @param ages_to_use optional. vector of age(s) to evaluate for breakpoints.
#' @param sex logical.
#' @param axes do you want to evaluate axes for space only (0), time only (1), or both simultaneously (2, default)?
#' @param eval the value that you would like to examine. default 0

#' @return matrix of detected breakpoints and plots
#' @export


get_Breaks <- function(dat, ages_to_use=NULL, sex = FALSE, axes = 2){

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

  newD <- data.frame( year = seq(min(dat_use$year),max(dat_use$year),length = 100),
                      long = seq(min(dat_use$long),max(dat_use$long), length = 100),
                      lat = seq(min(dat_use$lat),max(dat_use$lat),length = 100))[Terms] %>%
    mutate(detected_break = NA, count = 0)

  for(iage in seq_along(ages_to_use)){ ## loop over key ages

    ##TODO fill in breakpoints$sex here breakpoints$sex <- ifelse(sex)

    dat_use <- age_split[ages_to_use][[iage]]
    ##TODO split by sex here breakpoints$sex <- ifelse(sex)

    if(axes == 0)  mod <- gam(length ~ s(lat) + s(long), data = dat_use)
    if(axes == 1)  mod <- gam(length ~ s(year, bs = "cc"), data = dat_use)
    if(axes == 2)  mod <- gam(length ~ s(year, bs = "cc") + s(lat) + s(long),data = dat_use)

    # layout(matrix(1:4, ncol = 2))
    # gam.check(mod)
    # dat_use %>% mutate(predlen = predict(mod)) %>%
    #   select(-lat, -long) %>%
    #   melt(id = c('year', 'age')) %>%
    #   ggplot(data =., aes(x = age, y = value, color = variable)) +geom_point()

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

    # if(axes %in% c(0,2)){
    #
    #   p2$se2_lat <- pTerm$se.fit[,1]
    #   p2$se2_long <- pTerm$se.fit[,2]
    # } else if(axes %in% c(1,2)){
    #   p2$se2_yr <- pTerm$se.fit[,1]
    # }
    # p2 %>%
    #   mutate(se2_lat = ifelse(axes %in% c(0,1)), , NA)
    #
    # # pdat <- transform(dat_use,
    # #                   predLen = p2,
    # #                   se2_lat = pTerm$se.fit[,2],
    # #                   se2_lon = pTerm$se.fit[,3],
    # #                   se2_yr = pTerm$se.fit[,1])


    ## calculate additive variances
    # if(axes == 0){
    #   pdat <- transform(pdat,
    #                     upper = predLen + (crit.t * (se2_lat+se2_lon)),
    #                     lower = predLen - (crit.t * (se2_lat+se2_lon)))
    # }else if (axes == 1){
    #   pdat <- transform(pdat,
    #                     upper = predLen + (crit.t * (se2_yr)),
    #                     lower = predLen - (crit.t * (se2_yr)))
    # }else if(axes == 2){
    #   pdat <- transform(pdat,
    #                     upper = predLen + (crit.t * (se2_lat+se2_lon+se2_yr)),
    #                     lower = predLen - (crit.t * (se2_lat+se2_lon+se2_yr)))
    # }

    ## predict over parameter space
    # breaksdf <- list(); idx = 1## storage for breakpoints

    for(t in 1:length(Terms)){
      Term <- Terms[t]

      m2.d <- Deriv(mod, newdata = newD)
      m2.dci <- confint(m2.d, term = Term)

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
      newD$count[newD[,Term] %in% vals] <- newD$count[newD[,Term] %in% vals]+1 ## add how many combos flagged
      # breaksdf[[idx]] <- sort(c(unique(vals))) ## get rounded unique

      ## fill NAs in bdf for binding
      # for(i in 1:length(breaksdf)){
      #   if (length(breaksdf[[i]]) == 0){ ## fill NA for empty
      #     breaksdf[[i]] <- NA
      #   }
      # }## end breaksdf
      # cat(breaksdf)


      # breakpoints$age[idx] <- ages_to_use[iage];

    } ## end terms
     ## TODO end sexes

  } ## end key ages

  breakpoints <- newD[!is.na(newD$detected_break),]
  return(breakpoints)

}

get_Breaks(dat = simulated_data, axes = 0)
