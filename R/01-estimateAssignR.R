# testData <- data.frame(location = c("Japan", "Japan", "Italy", "Italy", "USA", "USA", "Italy", "Japan"),
#                        O18  = c(120, 125,68,65,300, 60, 90, 20), SR86_87 = c(2,3,6,7,5,4,7,2),
#                        MobType = c("A", "A", "B", "B", "B", "A", "B", "A"))
#
# y <- testData$location
# cats <- unique(y)
# refCat <- cats[1]
#
# numVars <- c("O18", "SR86_87")
# catVars <- c("MobType")
#
# XNUM <- model.matrix(as.formula(paste0("~ ", paste(numVars, collapse = "+")," - 1")), data = testData)
# XCAT <- model.matrix(as.formula(paste0("~ ", paste(catVars, collapse = "+")," - 1")), data = testData)
#
# y <- c(0,1,0,1,1, 0,1,0)
#
# xUncNUM <- matrix(0.1, ncol = ncol(XNUM), nrow = nrow(XCAT))
# xUncCAT <- matrix(0.1, ncol = ncol(XCAT), nrow = nrow(XCAT))


fitModelAssignR <- function(XNUM, XCAT, y, yUnc = NULL, xUncNUM = NULL, xUncCAT = NULL, iter = 1000, nChains = 4, burnin = 0.4*iter, thinning = 10, cat = ""){
  X <- cbind(rep(1, length(y)))

  if(!is.null(XNUM)){
    XNUM <- scale(XNUM)
    mRe <- attr(XNUM, "scaled:center")
    sRe <- attr(XNUM, "scaled:scale")
    if(!is.null(xUncNUM)){
      xUncNUM <- sweep(xUncNUM, 2, sRe, "/")
    }
    X <- cbind(X, XNUM)

  }
  if(!is.null(XCAT)){
    X <- cbind(X, XCAT)

  }

  nY <- length(y)
  XOrig <- X
  XOrigNUM <- XNUM
  XOrigCAT <- XCAT

  burnInProp <- 0.4

  #####################################
  ###Starting Values
  #####################################
  #Chain 1
  startPar <- rep(0, NCOL(X))
  pars <- startPar
  ######################################
  ###Tuningparameter der a-priori Verteilungen:
  ######################################
  a.eps <- 1E-5
  b.eps <- 1E-5
  a.mu <- 1E-5
  b.mu <- 1E-5
  priorB <- 1

  #######################################
  ###Parametermatrizen zur Speicherung der MCMC Iterationen
  #######################################
  betamc <- matrix(ncol = length(pars), nrow = iter)
  yPredmc <- matrix(ncol = length(y), nrow = iter)
  acceptMC <- matrix(ncol = length(pars), nrow = iter)
  ########################################
  #MCMC-Algorithmus
  ########################################
  #changeX <- which(data$Uncertainty2 > 0)

  MHPar <- rep(0.1, length(pars))

  #rescale

  MCMC_AssignR <- function(start, iter){
    for (m in start:iter) {
      #Betas

      if(m %% 100 == 0){
        accRates <- colMeans(acceptMC[(m - 99):(m-1), , drop = FALSE])
        MHPar[accRates < 0.234] <<- MHPar[accRates < 0.234] * 0.8
        MHPar[accRates > 0.234] <<- MHPar[accRates > 0.234] * 1.25
      }

      #MH-step for parameters
      parsNew <- pars
      for(j in 1:length(pars)){
        parsNew[j] <- pars[j] + rnorm(1, 0, MHPar[j])
        if(all(!is.na(y))){
          acc <- pmin(1, exp((sum(log(resp(X %*% parsNew))[y==1]) + sum(log(1 - resp(X %*% parsNew))[y==0]) + sum((parsNew - 0) ^ 2 / (-2 * priorB))) -
                               ((sum(log(resp(X %*% pars))[y==1]) + sum(log(1 - resp(X %*% pars))[y==0])) + sum((pars - 0) ^ 2 / (-2 * priorB)))))

          if(!is.nan(acc)){
            randomAlpha <- runif(1)
            accept <- (acc > runif(1))
          } else {
            accept <- FALSE
          }
        } else {
          accept <- FALSE
        }
        if(accept){
          pars[j] <- parsNew[j]
        }
        acceptMC[m, j] <<- accept
      }
      pars <<- pars
      # #MH-step for time
      if(m %% 10 == 0 && !is.null(XNUM) && !is.null(xUncNUM)  && (NCOL(xUncNUM) == NCOL(XNUM))){
        for(l in 1:NCOL(xUncNUM)){
          changeX <- which(xUncNUM[, l] > 0)

          if (length(changeX) > 0){
            random <- rnorm(nY, sd = xUncNUM[, l])
            XNumNew <- XNUM
            XNumNew[, l] <- XNUM[, l] + random
            XNew <- cbind(rep(1, length(y)), XNumNew, XCAT)
            muX <- mean(XNumNew[, l])
            sdX <- sd(XNumNew[,l])
            acc <- pmin(1, exp((sum(log(resp(XNew %*% pars))[y==1]) + sum(log(1 - resp(XNew %*% pars))[y==0]) +
                                  (XNumNew[, l] - XOrigNUM[, l])^2 / (-2 * xUncNUM[, l]^2) +
                                  (XNumNew[, l] - muX)^2 / (-2 * sdX^2) -
                                  sum(log(resp(X %*% pars))[y==1]) + sum(log(1 - resp(X %*% pars))[y==0]) -
                                  (XNUM[, l] - XOrigNUM[, l])^2 / (-2 * xUncNUM[, l]^2) -
                                  (XNUM[, l] - muX)^2 / (-2 * sdX^2))))
            acc[is.na(acc)] <- 0
            randomAlpha <- runif(nY)
            updated <- which(randomAlpha < acc)
            if(length(updated) > 0){
              XNUM[updated, l] <- XNumNew[updated, l]
            }
          }
        }
        if(!is.null(XCAT)){
          X <- cbind(rep(1, length(y)), XNUM, XCAT)
        } else {
          X <- cbind(rep(1, length(y)), XNUM)
        }
      }
      #categorical vars
      if(m %% 10 == 0 && !is.null(XCAT) && !is.null(xUncCAT) && (NCOL(xUncCAT) == NCOL(XCAT))){
        for(l in 1:NCOL(xUncCAT)){
          changeX <- which(xUncCAT[, l] > 0)

          if (length(changeX) > 0){
            random <- rnorm(nY, sd = xUncCAT[, l])
            XCatNew <- XCAT
            XCatNew[, l] <- XCAT[, l] + random
            XNew <- cbind(rep(1, length(y)), XNUM, XCatNew)
            muX <- mean(XCatNew[, l])
            sdX <- sd(XCatNew[,l])
            acc <- pmin(1, exp((sum(log(resp(XNew %*% pars))[y==1]) + sum(log(1 - resp(XNew %*% pars))[y==0]) +
                                  (XCatNew[, l] - XOrigCAT[, l])^2 / (-2 * xUncCAT[, l]^2) +
                                  (XCatNew[, l] - muX)^2 / (-2 * sdX^2) -
                                  sum(log(resp(X %*% pars))[y==1]) + sum(log(1 - resp(X %*% pars))[y==0]) -
                                  (XCAT[, l] - XOrigCAT[, l])^2 / (-2 * xUncCAT[, l]^2) -
                                  (XCAT[, l] - muX)^2 / (-2 * sdX^2))))
            acc[is.na(acc)] <- 0
            randomAlpha <- runif(nY)
            updated <- which(randomAlpha < acc)
            if(length(updated) > 0){
              XCAT[updated, l] <- XCatNew[updated, l]
            }
          }
        }
        if(!is.null(XNUM)){
          X <- cbind(rep(1, length(y)), XNUM, XCAT)
        } else {
          X <- cbind(rep(1, length(y)), XCAT)
        }
      }


      #Sigma
      #Smoothing Parameter lambda

      # nolint start
      #conditional posterioris:

      # nolint end

      #Werte in Parametermatrizen einsetzen
      betamc[m, ] <<- pars
    }
    return(betamc)
  }


  for ( k in 1:5) {
    log_memory_usage()
    j <- seq(1, iter, iter / 5)[k]
    showMessage(
      MCMC_AssignR,
      msg = "Calculating AssignR",
      detail = paste0("Chain ", nChains, ", Cat: ", cat),
      value = k / 5)(
        start = j, iter = j + iter / 5 - 1
      )
    pars <- startPar
    MHPar <- rep(0.1, length(pars))
  }

  burnin <- round(burnInProp * iter / nChains +  seq(0, iter, iter / nChains))
  every <- thinning  #nur die x-te MCMC-Iteration soll genutzt werden
  #Vektor der tatsaechlich benutzten Beobachtungen
  usedsamples <- unlist(sapply(1:nChains, function(k) seq(from = burnin[k], to = iter / nChains * k, by = every)))
  log_memory_usage()
  return(list(beta = betamc[usedsamples, , drop = F],
              mRe = mRe, sRe = sRe,
              nChains = nChains))
}

resp <- function(x){
  exp(x) / (1 + exp(x))
}
normalizePredictions <- function(predictions){
  lapply(1:length(predictions), function(x) {
    predictions[[x]] / Reduce("+", predictions)
  })
}

modelAssignRMC <- function(XNUM, XCAT, y, yUnc = NULL, xUncNUM = NULL, xUncCAT = NULL, iter = 1000, nChains = 4, burnin = 0.4*iter, thinning = 10, cat = ""){
  ret <- lapply(1:nChains, function(x){
    fitModelAssignR(XNUM, XCAT, y, yUnc = yUnc,
                    xUncNUM = xUncNUM, xUncCAT = xUncCAT,
                    iter = iter, nChains = x,
                    burnin = 0.4*iter, thinning = thinning, cat = cat)
  })
  res <- ret[[1]]
  res$beta <- as.matrix(do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$beta)), ncol = NCOL(res$beta))
  return(res)
}

# res <- fitModelAssignR(XNUM, XCAT, y, yUnc = NULL, xUncNUM = xUncNUM, xUncCAT = xUncCAT, iter = 200000, chains = 8, burnin = 0.4*iter)
# plot(res$beta[,1])
