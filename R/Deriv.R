#' Evaluate first derivative of GAM smooth(s), adapted from
#' \href{https://gist.githubusercontent.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R}{Gavin Simpson}.
#' @param mod the output of {mgcv::gam()}
#' @param n number of intervals over which to evaluate each smooth. default 200.
#' @param eps tolerance threshold. default 1e-4.
#' @param newdata optional; data.frame of new smooth parameters to evaluate
#' @param term string smooth name, i.e. "year". must match values in mod and newdata
#' @return a vector of derivative values of length n
#' @export

Deriv <- function(mod, n = 200, eps = 1e-4, newdata, term) {
  if(inherits(mod, "gamm"))
    mod <- mod$gam
  m.terms <- attr(terms(mod), "term.labels")
  if(missing(newdata)) {
    newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                   function(x) seq(min(x), max(x), length = n))
    names(newD) <- m.terms
  } else {
    newD <- newdata
  }
  p <-proc.time()
  X0 <- predict(mod, data.frame(newD), type = "lpmatrix")
  icol  <- NA; for(i in 1:ncol(newD))  if(class(newD[,i]) == 'numeric') icol[i] <- i; icol <- icol[!is.na(icol)] ## return numeric vectors

  # newD[,1:ncol(newD)] <- newD[,1:ncol(newD)] + eps ## only update smooths
  newD[,icol] <- newD[,icol] + eps ## only update smooths
  X1 <- predict(mod, data.frame(newD), type = "lpmatrix")
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  ## dims of bs
  bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
  ## number of smooth terms
  t.labs <- attr(mod$terms, "term.labels")
  proc.time() - p
  ## match the term with the the terms in the model
  if(!missing(term)) {
    want <- grep(term, t.labs)
    if(!identical(length(want), length(term)))
      stop("One or more 'term's not found in model!")
    t.labs <- t.labs[want]
  }
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(mod)
    df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  class(lD) <- "Deriv"
  lD$gamModel <- mod
  lD$eps <- eps
  # lD$eval <- newD[,1:2] - eps
  lD$eval <- newD[,icol] - eps
  lD ##return
}

