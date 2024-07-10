###################
find.ice <- function(aic)
  {
    aic.min <- apply(aic, MARGIN = 1, FUN = min)
    aic.ind <- matrix(aic.min, nrow = nrow(aic), ncol = ncol(aic)) == aic
    aic.ind <- aic.ind & 1 == t(apply(aic.ind, MARGIN = 1, cumsum))
    ice <- t(col(aic))[t(aic.ind)] - 1
    ice
  }

###################
peacf.plot <- function(r)
  {
    scale <- 0.5
    type <- attr(r, "type")
    if(is.null(type))
      stop("Error: invalid input. Input must be output from peacf or pepacf"
      )
    if(type == "acf") {
      acf <- scale * r$acf
      sdl <- scale * 1.96 * r$benchmark.sd
      ts.title <- r$title
      ylabel <- "acf"
    }
    else if(type == "pacf") {
      acf <- scale * r$pacf
      sdl <- scale * 1.96 * r$acf.out$benchmark.sd
      ts.title <- r$acf.out$title
      ylabel <- "pacf"
    }
    if(is.null(acf)) {
      stop("error: input argument must be generated from peacf() or pepacf()"
      )
    }
    p <- dim(acf)[[1]]
    lag.max <- dim(acf)[[2]]
    plot(c(1:p, rep(0, lag.max)), c(rep(0, p), 1:lag.max), type = "n", xlab
         = "period", ylab = "")
    for(imonth in 1:p) {
      segments((imonth - sdl), 0, (imonth - sdl), lag.max)
      segments((imonth + sdl), 0, (imonth + sdl), lag.max)
      for(ilag in 1:lag.max) {
        segments(imonth, ilag, (imonth + acf[imonth, ilag]), 
                 ilag)
      }
    }
    title(main = ts.title, ylab = ylabel)
  }


################
peacf <- function(z, lag.max, plot = TRUE)
  {
    if(!is.ts(z)) {
      stop("Error: input is not a time series.\nUse ts() to fix-up.")
    }
    p <- attr(z, "tsp")[3]
    if(missing(lag.max))
      lag.max <- ceiling(length(z)/(p * 4))
    z.names <- attr(z, "period.abb")
    zc <- z
    zmean <- numeric(p)
    zsd <- numeric(p)
    nyrs <- numeric(p)
    nyrs.max <- ceiling(length(z)/p)
    for(imonth in 1:p) {
      k <- cycle(z) == imonth
      zmean[imonth] <- mean(z[k])
      zc[k] <- zmean[imonth]
      nyrs[imonth] <- length(z[k])
      zsd[imonth] <- sqrt(sum((z[k] - zc[k])^2)/nyrs.max)
    }
    zc <- z - zc
    if(lag.max > min(nyrs)) {
      cat("\nwarning: lag.max exceeds data span so reset to a smaller value"
      )
      lag.max <- min(nyrs) - 1
    }
    periodic.acf <- matrix(numeric(1), nrow = p, ncol = lag.max)
    for(ilag in 1:lag.max) {
      zac1 <- ts.intersect(zc, lag(zc,  - ilag))
      zac <- ts(apply(zac1, MARGIN = 1, FUN = prod), frequency = p, 
                start = attr(zac1, "tsp")[1])
      for(imonth in 1:p) {
        imonthlag <- (imonth - ilag - 1) %% p + 1
        periodic.acf[imonth, ilag] <- sum(zac[cycle(zac) == 
                                                imonth])/(nyrs.max * zsd[imonth] * zsd[
                                                  imonthlag])
      }
    }
    if(is.null(z.names))
      dimnames(periodic.acf) <- list(periods = paste("period", 1:p), 
                                     lags = paste("lag", 1:lag.max))
    else dimnames(periodic.acf) <- list(periods = z.names, lags = paste(
      "lag", 1:lag.max))
    bsd <- 1/sqrt(max(nyrs))
    title.z <- attr(z, "title")
    if(is.null(title.z)) {
      title.z <- " "
    }
    Q1 <- nyrs.max * sum(periodic.acf[, 1]^2)
    Q1.sl <- 1 - pchisq(Q1, p)
    periodicity.test <- list(Q1 = Q1, Q1.sl = Q1.sl)
    null.var <- matrix(numeric(1), nrow = p, ncol = lag.max)
    for(i in 1:p) {
      for(j in 1:lag.max) {
        null.var[i, j] <- var.periodic.correlation(j, i, nyrs[i
        ], p)
      }
    }
    QM <- matrix(t(apply((periodic.acf^2)/null.var, 1, cumsum)), nrow = p, 
                 ncol = ncol(periodic.acf))
    QM.df <- col(QM)
    if(lag.max >= 5)
      QM.lags <- if(0 == (lag.max %% 5)) 5 * (1:(lag.max/5)) else c(5 *
                                                                      (1:trunc(lag.max/5)), lag.max)
    else QM.lags <- lag.max
    QM <- matrix(QM[, QM.lags], nrow = p, ncol = length(QM.lags))
    QM.df <- matrix(QM.df[, QM.lags], nrow = p, ncol = length(QM.lags))
    portmanteau.test <- list(QM = QM, QM.df = QM.df)
    r <- list(means = zmean, standard.deviations = zsd, acf = periodic.acf, 
              benchmark.sd = bsd, sub.lengths = nyrs, period = p, title = 
                title.z, periodicity.test = periodicity.test, portmanteau.test
              = portmanteau.test)
    attr(r, "type") <- "acf"
    if(plot) {
      peacf.plot(r)
    }
    r
  }


##############
peboxplot <-  function(z, ...)
  {
    ztitle <- attr(z, "title")
    z.names <- attr(z, "period.abb")
    if(is.null(z.names))
      z.names <- as.character(unique(cycle(z)))
    boxplot(split(z, cycle(z)), names = z.names, ...)
    if(!is.null(ztitle))
      title(main = ztitle)
  }


############
pear <- function(z, m, ic = "none")
  {
    if(ic == "none") {
      if(missing(m)) {
        stop("Error: m required. Vector of model orders")
      }
      lag.max <- max(m)
      acf.out <- peacf(z, lag.max, plot = FALSE)
    }
    else if(!(ic == "none")) {
      p <- attr(z, "tsp")[3]
      N <- length(z)
      lag.max <- ceiling((0.25 * N)/p)
      pacf.out <- pepacf(z, lag.max, plot = FALSE)
      if(ic == "aic") {
        m <- pacf.out$maice
      }
      else if(ic == "bic") {
        m <- pacf.out$mbice
      }
      lag.max <- max(m)
      acf.out <- pacf.out$acf.out
    }
    p <- acf.out$period
    if((ic == "none") && (length(m) != p))
      m <- rep(m, length = p)
    res <- numeric(length(z))
    means <- acf.out$means
    for(imonth in 1:p) {
      k <- cycle(z) == imonth
      res[k] <- means[imonth]
    }
    res <- z - res
    attr(res, "tsp") <- attr(z, "tsp")
    z <- res
    Qm <- 0
    Qm.sl <- 0
    nyrs <- acf.out$sub.lengths
    bsd <- acf.out$benchmark.sd
    phi <- matrix(numeric(1), nrow = p, ncol = lag.max)
    se.phi <- matrix(numeric(1), nrow = p, ncol = lag.max)
    resvar <- numeric(p)
    sd <- acf.out$standard.deviations
    acvf <- cbind(rep(1, p), acf.out$acf)
    cov <- vector("list", p)
    for(i in 1:p) {
      for(j in 1:(lag.max + 1)) {
        acvf[i, j] <- acvf[i, j] * sd[i] * sd[((i - j) %% p) + 
                                                1]
      }
    }
    for(imonth in 1:p) {
      pm <- m[imonth]
      if(pm == 0) {
        resvar[imonth] <- acvf[imonth, 1]
        phim <- 0
        sephim <- 0
      }
      else {
        a <- matrix(numeric(1), nrow = pm, ncol = pm)
        for(i in 1:pm) {
          for(j in 1:pm) {
            k <- i - j
            kmonth <- (imonth - j - 1) %% p + 1
            if(k < 0) {
              k <-  - k
              kmonth <- (imonth - i - 1) %% p + 1
            }
            a[i, j] <- acvf[kmonth, k + 1]
          }
        }
        b <- acvf[imonth, 1 + (1:pm)]
        phim <- solve(a, b)
        resvar[imonth] <- acvf[imonth, 1] - phim %*% acvf[
          imonth, 1 + (1:pm)]
        cov[[imonth]] <- (solve(a) * resvar[imonth])/nyrs[
          imonth]
        sephim <- sqrt(diag(cov[[imonth]]))
        if(pm < lag.max) {
          phim <- c(phim, rep(0, lag.max - pm))
          sephim <- c(sephim, rep(0, lag.max - pm))
        }
      }
      phi[imonth,  ] <- phim
      se.phi[imonth,  ] <- sephim
    }
    for(i in 1:length(z)) {
      res[i] <- 0
      imonth <- cycle(z)[i]
      pm <- m[imonth]
      if(((i - pm) > 0) && (pm > 0))
        res[i] <- z[i] - z[i - (1:pm)] %*% phi[imonth, 1:pm]
      else if(pm == 0)
        res[i] <- z[i]
    }
    ra <- peacf(res, plot = FALSE)
    residual.acf <- ra$acf
    dimnames(residual.acf)[[1]] <- dimnames(acf.out$acf)[[1]]
    L.racf <- ncol(residual.acf)
    residual.acf.sd <- matrix(numeric(1), nrow = p, ncol = L.racf, dimnames
                              = dimnames(residual.acf))
    psi <- cbind(rep(1, p), pepsi(phi, L.racf - 1))
    for(imonth in 1:p) {
      pm <- m[imonth]
      if(pm > 0) {
        X <- matrix(numeric(1), nrow = L.racf, ncol = pm)
        for(i in 1:L.racf) {
          for(j in 1:pm) {
            if((i - j) >= 0) {
              imonthmi <- (imonth - i - 1) %% p + 1
              jmonth <- (imonth - j - 1) %% p + 1
              rsigmas <- sqrt(resvar[imonthmi]/resvar[
                imonth])
              X[i, j] <- psi[jmonth, i - j + 1] * rsigmas
            }
          }
        }
        covest <- cov[[imonth]]
        residual.acf.sd[imonth,  ] <- sqrt(abs(1/nyrs[imonth] - 
                                                 diag(X %*% covest %*% t(X))))
      }
      else if(pm == 0) {
        residual.acf.sd[imonth,  ] <- sqrt(1/nyrs[imonth])
      }
    }
    dimnames(phi) <- list(dimnames(acf.out$acf)[[1]], paste("lag", 1:ncol(
      phi)))
    dimnames(se.phi) <- dimnames(phi)
    names(m) <- dimnames(phi)[[1]]
    names(resvar) <- names(m)
    names(cov) <- names(m)
    QM <- ra$portmanteau.test$QM
    QM.df <- ra$portmanteau.test$QM.df - matrix(m, nrow = p, ncol = ncol(QM
    ))
    QM.sl <- matrix((1 - pchisq(QM, QM.df)), nrow = nrow(QM), ncol = ncol(
      QM), dimnames = dimnames(QM))
    portmanteau.test <- list(QM = QM, QM.df = QM.df, QM.sl = QM.sl)
    out <- list(model.orders = m, phi = phi, se.phi = se.phi, resvar = 
                  resvar, residuals = res, portmanteau.test = portmanteau.test, 
                residual.acf = residual.acf, residual.acf.sd = residual.acf.sd, 
                cov = cov)
    out
  }


############
pepacf <-function(z, lag.max, plot = TRUE, acf.out)
  {
    if(missing(acf.out)) {
      if(missing(lag.max))
        acf.out <- peacf(z, plot = FALSE)
      else acf.out <- peacf(z, lag.max, plot = FALSE)
    }
    p <- acf.out$period
    if(missing(lag.max))
      lag.max <- ceiling(length(z)/(p * 4))
    nyrs <- acf.out$sub.lengths
    bsd <- acf.out$benchmark.sd
    w1 <- numeric(p)
    w2 <- numeric(p)
    w3 <- numeric(p)
    pacf <- matrix(numeric(1), nrow = p, ncol = lag.max)
    phi <- matrix(numeric(1), nrow = p, ncol = lag.max)
    phiF <- matrix(numeric(1), nrow = p, ncol = lag.max)
    resvar <- matrix(numeric(1), nrow = p, ncol = lag.max + 1)
    resvarF <- matrix(numeric(1), nrow = p, ncol = lag.max + 1)
    sd <- acf.out$standard.deviations
    acvf <- cbind(rep(1, p), acf.out$acf)
    for(i in 1:p) {
      for(j in 1:(lag.max + 1)) {
        acvf[i, j] <- acvf[i, j] * sd[i] * sd[((i - j) %% p) + 
                                                1]
      }
    }
    resvar[, 1] <- acvf[, 1]
    resvarF[, 1] <- acvf[, 1]
    for(ilag in 1:lag.max) {
      SFPREV <- resvarF[p, ilag]
      for(imonth in 1:p) {
        D <- acvf[imonth, ilag + 1]
        if(ilag > 1) {
          for(i in 1:(ilag - 1)) {
            mmi <- ((imonth - i - 1) %% p) + 1
            D <- D - acvf[mmi, (ilag - i + 1)] * phi[
              imonth, i]
          }
        }
        phi[imonth, ilag] <- D/SFPREV
        phiF[imonth, ilag] <- D/resvar[imonth, ilag]
        pacf[imonth, ilag] <- D/sqrt(SFPREV * resvar[imonth, 
                                                     ilag])
        TEMP <- 1 - phi[imonth, ilag] * phiF[imonth, ilag]
        resvar[imonth, ilag + 1] <- resvar[imonth, ilag] * TEMP
        SFKEEP <- resvarF[imonth, ilag]
        resvarF[imonth, ilag + 1] <- SFPREV * TEMP
        SFPREV <- SFKEEP
        if(ilag > 1) {
          for(i in 1:(ilag - 1)) {
            w2[i] <- w1[i] - phi[imonth, ilag - i] * phiF[
              imonth, ilag]
            w3[i] <- phi[imonth, i] - phi[imonth, ilag] * 
              w1[ilag - i]
          }
          for(i in 1:(ilag - 1)) {
            w1[i] <- phiF[imonth, i]
            phiF[imonth, i] <- w2[i]
            phi[imonth, i] <- w3[i]
          }
        }
      }
      w1 <- phiF[p,  ]
    }
    nyrs.matrix <- matrix(nyrs, ncol = lag.max + 1, nrow = p)
    aic <- nyrs.matrix * log(resvar)
    bic <- aic
    k.matrix <- matrix(0:lag.max, byrow = TRUE, nrow = p, ncol = lag.max + 1)
    aic <- aic + 2 * k.matrix
    bic <- bic + log(nyrs.matrix) * k.matrix
    maice <- find.ice(aic)
    mbice <- find.ice(bic)
    r <- list(acf.out = acf.out, pacf = pacf, residual.sd = sqrt(resvar), 
              phi = phi, aic = aic, bic = bic, maice = maice, mbice = mbice)
    attr(r, "type") <- "pacf"
    if(plot) {
      peacf.plot(r)
    }
    r
  }



##############
pepsi <- function(phi, lag.max)
  {
    phi[is.na(phi)] <- 0
    p <- nrow(phi)
    m.max <- ncol(phi)
    if(m.max > 1)
      m <- t(apply(t(apply(t(apply(phi, 1, "rev")) != 0, 1, "cumsum")
      ) != 0, 1, "cumsum"))[, ncol(phi)]
    else m <- rep(1, p)
    psi <- matrix(numeric(1), nrow = p, ncol = lag.max + 1)
    psi[, 1] <- 1
    for(ilag in 1:lag.max) {
      for(imonth in 1:p) {
        TEMP <- numeric(1)
        for(jlag in 1:m[imonth]) {
          kmonth <- (imonth - jlag - 1) %% p + 1
          klag <- ilag - jlag
          if(klag >= 0)
            TEMP <- TEMP + phi[imonth, jlag] * psi[kmonth,
                                                   klag + 1]
        }
        psi[imonth, ilag + 1] <- TEMP
      }
    }
    psi <- psi[, -1]
    dimnames(psi) <- list(periods = paste("period", 1:p), lags = paste(
      "lag", 1:lag.max))
    psi
  }



###############
var.periodic.correlation <-  function(l, m, n, p)
  {
    if((l %% p) == 0)
      (n - l/p)/(n * (n + 2))
    else (n - trunc((l - m + p)/p))/n^2
  }


######################
peplot <- function(z, lag = 1, label = FALSE, mfrow = c(2, 2))
  {
    if(2 != length(mfrow)) {
      stop("\nError: mfrow must be vector of length 2. \nExamples:\n     mfrow=c(1,1) produces 1 plot at a time\n     mfrow=c(2,2) produces 4 plots at a time."
      )
    }
    ztitle <- attr(z, "title")
    par(mfrow = mfrow)
    plot.count <- apply(matrix(mfrow, ncol = 2, nrow = 1), MARGIN = 1, FUN
                        = "prod")
    z.title <- attr(z, "title")
    if(is.null(z.title))
      z.title <- " "
    z.names <- attr(z, "period.abb")
    if(is.null(z.names)) {
      z.names <- paste("period", unique(cycle(z)))
    }
    p <- attr(z, "tsp")[3]
    start.month <- cycle(z)[1]
    icount <- 0
    for(imonth in 1:p) {
      icount <- icount + 1
      jmonth <- (imonth - lag - 1) %% p + 1
      y <- z[cycle(z) == imonth]
      x <- z[cycle(z) == jmonth]
      u <- 1:length(y)
      if(start.month != 1) {
        if((imonth >= start.month) && (jmonth < start.month)) 
        {
          y <- y[-1]
          u <- u[-1]
        }
        if((imonth < start.month) && (imonth - lag < 1)) {
          x <- x[ - length(x)]
        }
      }
      if((imonth - lag) < 1) {
        ndrop <- 1 + trunc((lag - imonth)/p)
        n <- length(y)
        y <- y[ - (1:ndrop)]
        u <- u[ - (1:ndrop)]
        x <- x[ - ((n - ndrop + 1):n)]
      }
      plot(x, y, xlab = z.names[jmonth], ylab = z.names[imonth])
      if(label) {
        identify(x, y, labels = u)
      }
      if((icount != p) && (icount %% plot.count) == 0) {
        if(!is.null(ztitle)) {
          mtext(ztitle, side = 3, outer = TRUE, line = -2, cex = 1.2)
          #par(mfrow = c(1, 1))
          #title(main = ztitle)
          #par(mfrow = mfrow)
        }
        cat("\n Press Enter key for next plot")
        junk <- (as.character(readline()))[1]
      }
      else if((icount %% plot.count) == 0) {
        if(!is.null(ztitle)) {
          mtext(ztitle, side = 3, outer = TRUE, line = -2, cex = 1.2)
          #par(mfrow = c(1, 1))
          #title(main = ztitle)
          #par(mfrow = mfrow)
        }
      }
    }
    par(mfrow = c(1, 1))
  }
