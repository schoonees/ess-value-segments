# set.seed(1)
# wts <- rep(1, 50)
# tar <- c(0.3, 0.2, 0.5)
# samp <- sample.int(n = 3, size = 50, replace = TRUE)
# prop <- tapply(wts, samp, sum) / sum(wts)
myrake <- function(w, f, target, tol = .Machine$double.eps, maxit = 50L) {
  
  target <- na.omit(target)
  worig <- w
  forig <- f
  if (!is.null(names(target))) {
    levs <- names(target)
    f <- factor(f, levels = levs)
  } else {
    f <- factor(f)
    levs <- levels(f)
  }
  nlev <- length(levs)
  origtot <- sum(w)
  
  ## Deal with NA's in f
  isNA <- is.na(f)
  f <- f[!isNA]
  w <- w[!isNA]
  
  ## Iteration
  iter <- 0L
  err <- 1
  coef <- rep(1, nlev)
  while (err > tol & iter < maxit) {
    iter <- iter + 1L
    for (i in seq_len(nlev)) {
      tot <- sum(w)
      prop <- sum(w[f == levs[i]], na.rm = TRUE) / tot
      w[f == levs[i]] <- w[f == levs[i]] * target[i] / prop
    }
    w <- w * origtot / sum(w)
    sums <- tapply(w, f, sum)
    sums[is.na(sums)] <- 0
    props <- prop.table(sums)
    err <- sum((target - props)^2)
  }
  ## Zero weight for observed values not in target
  wout <- rep(0, length(worig))
  wout[!isNA] <- w
  return(list(w = wout, err = err, iter = iter, fitted = props, target = target))
}

# wts2 <- myrake(wts, samp, tar)
# prop.table(tapply(wts2$w, samp, sum))