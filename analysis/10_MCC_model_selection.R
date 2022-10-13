#===============================================================================
# Script: Trajectory_modelling_MCC
# Objective: Cluster trajectory of healthcare service usage using MCC Clustering
# Last Edited by : Karthik Mohan, 8 Oct 2022
#===============================================================================

rm(list=ls())

# ====================  Packages ===============================================
pkgs <- c("dplyr", "ggplot2", "tidyr", "tidyverse")
# install.packages(pkgs)
lapply(pkgs, require, character.only = TRUE)

getwd()
setwd('C:/Projects/SLICK/SLICK_R')
# ================== Path ======================================================
# Insert appropriate data path
data_path = '../Data/output/data/'
myOutfilesDir <- paste0(data_path, 'Test_run/') # This is where the MCClust function will save the output file
workDir <- paste0(data_path, 'Model_Selection/') # This is where model selection output will be saved
#===============================================================================

# Load data
resource    <- readRDS(paste0(data_path, 'data_resource.rds'))

# ----------------- MCClust --------------------------
# Need to install package from source due to R version compatibility (requires RTools)
# Need to install dependencies : gplot, gtools, bayesm
# install.packages("bayesMCClust_1.0.tar.gz", repos = NULL, type = 'source')
# -------------------

# library(bayesMCClust)
#===============================================================================
# Function definitions - MC Clusts
#===============================================================================
dataListToNjki <- function (dataList) 
{
  stopifnot(class(dataList) == "list")
  N <- length(dataList)
  minCat <- range(dataList)[1]
  K <- range(dataList)[2] - minCat
  Njk.i <- array(0, c(K + 1, K + 1, N), dimnames = list(as.numeric(names(table(unlist(dataList)))), 
                                                        as.numeric(names(table(unlist(dataList)))), NULL))
  counttr.i <- function(tr1, tr2) {
    Njk.i[tr1 + 1, tr2 + 1, i] <<- Njk.i[tr1 + 1, tr2 + 
                                           1, i] + 1
  }
  transDataList <- dataList
  lengthTS <- sapply(dataList, length)
  for (i in 1:N) {
    mapply(counttr.i, transDataList[[i]][1:(lengthTS[i] - 
                                              1)], transDataList[[i]][2:lengthTS[i]])
  }
  return(invisible(Njk.i))
}

ddirichlet <- function (x, alpha) 
{
  dirichlet1 <- function(x, alpha) {
    logD <- sum(lgamma(alpha)) - lgamma(sum(alpha))
    s <- (alpha - 1) * log(x)
    s <- ifelse(alpha == 1 & x == 0, -Inf, s)
    exp(sum(s) - logD)
  }
  if (!is.matrix(x)) {
    if (is.data.frame(x)) {
      x <- as.matrix(x)
    }
    else {
      x <- t(x)
    }
  }
  if (!is.matrix(alpha)) {
    alpha <- matrix(alpha, ncol = length(alpha), nrow = nrow(x), 
                    byrow = TRUE)
  }
  if (any(dim(x) != dim(alpha))) {
    stop("Mismatch between dimensions of 'x' and 'alpha'.")
  }
  pd <- vector(length = nrow(x))
  for (i in 1:nrow(x)) {
    pd[i] <- dirichlet1(x[i, ], alpha[i, ])
  }
  pd[apply(x, 1, function(z) any(z < 0 | z > 1))] <- 0
  pd[apply(x, 1, function(z) all.equal(sum(z), 1) != TRUE)] <- 0
  pd
}


MCClust <- function (Data = list(dataFile = stop("'dataFile' (=> Njk.i) must be specified: either 'filename' (path) or data"), 
                                         storeDir = "try01", priorFile = NULL), Prior = list(H = 4, 
                                                                                             e0 = 4, c = 1, cOff = 1, usePriorFile = FALSE, xiPooled = FALSE, 
                                                                                             N0 = 5), Initial = list(xi.start.ind = 3, pers = 0.7, S.i.start = NULL), 
                             Mcmc = list(M = 50, M0 = 20, mOut = 5, mSave = 10, seed = 12345)) 
{
  startDate <- format(Sys.time(), "%Y%m%d_%H%M%S")
  startTime <- proc.time()[3]
  H <- Prior$H
  M <- Mcmc$M
  M0 <- Mcmc$M0
  set.seed(Mcmc$seed)
  mOut <- Mcmc$mOut
  mSave <- Mcmc$mSave
  xi.start.ind <- Initial$xi.start.ind
  pers <- Initial$pers
  e0 <- Prior$e0
  c <- Prior$c
  cOff <- Prior$cOff
  dirName <- Data$storeDir
  if (!file.exists(dirName)) 
    dir.create(dirName)
  fileName <- paste("MCC_H", H, "_M", M, "_", startDate, ".RData", 
                    sep = "")
  workspaceFile <- file.path(dirName, fileName)
  logLike <- numeric(M)
  logClassLike <- numeric(M)
  entropy <- numeric(M)
  logXiPrior <- numeric(M)
  logEtaPrior <- numeric(M)
  NhSfunPat <- function(S, H, freq) {
    sapply(1:H, function(h) sum((S == h) * freq))
  }
  freqMat <- function(i) S.i.counts[i, S.i.temp[i]] <<- S.i.counts[i, 
                                                                   S.i.temp[i]] + 1
  countMat <- function(h) Njk.h.temp[, , h] <<- apply(Njk.i.ind[, 
                                                                , S.i.temp.ind == h], c(1, 2), sum)
  dirDensFun <- function(h, j) log(ddirichlet(xi.m[m, j, , 
                                                   h], c0[j, , h]))
  if (is.character(Data$dataFile)) {
    tp <- load(Data$dataFile)
    stopifnot(tp == "Njk.i")
  }
  else {
    Njk.i <- Data$dataFile
  }
  N <- dim(Njk.i)[3]
  K <- dim(Njk.i)[1] - 1
  if (Prior$usePriorFile | Initial$xi.start.ind == 4) {
    if (is.character(Data$priorFile)) {
      tp2 <- load(Data$priorFile)
      stopifnot(tp2 == "mcc")
    }
    else {
      mcc <- Data$priorFile
    }
  }
  else {
    mcc <- NULL
  }
  Njk <- matrix(0, K + 1, K + 1, dimnames = dimnames(Njk.i)[1:2])
  Njk <- apply(Njk.i, c(1, 2), sum)
  xi.hat <- Njk/apply(Njk, 1, sum)
  pasteNjk <- apply(Njk.i, c(3), paste, collapse = " ")
  names(pasteNjk) <- 1:N
  freq <- as.numeric(table(pasteNjk))
  indizes <- as.numeric(names(sort(pasteNjk[!duplicated(pasteNjk)])))
  R <- length(freq)
  Njk.i.ind <- array(0, c(K + 1, K + 1, R), dimnames = dimnames(Njk.i))
  for (asd in 1:R) Njk.i.ind[, , asd] <- Njk.i[, , indizes[asd]] * 
    freq[asd]
  c0 <- if (Prior$usePriorFile) {
    if (Prior$xiPooled) {
      array((Prior$N0 * mcc[[1]]$xi), c(K + 1, K + 1, 
                                        H))
    }
    else {
      array((Prior$N0 * mcc[[H]]$xi), c(K + 1, K + 1, 
                                        H))
    }
  }
  else {
    array(diag(c, K + 1) + cOff, c(K + 1, K + 1, H))
  }
  eta.start <- 1/H
  if (xi.start.ind == 1) {
    xi.h <- array(1/(K + 1), c(K + 1, K + 1, H))
  }
  if (xi.start.ind == 2) {
    xi.h <- array(0, c(K + 1, K + 1, H))
    invisible(sapply(1:H, function(h) xi.h[, , h] <<- xi.hat))
  }
  if (xi.start.ind == 3) {
    trans <- (1 - pers)/K
    xi.start <- (pers - trans) * diag(1, K + 1, K + 1) + 
      trans * matrix(1, K + 1, K + 1)
    xi.h <- array(0, c(K + 1, K + 1, H))
    invisible(sapply(1:H, function(h) xi.h[, , h] <<- xi.start))
  }
  if (xi.start.ind == 4) {
    xi.h <- array(mcc[[H]]$xi, c(K + 1, K + 1, H))
    eta.start <- mcc[[H]]$eta
  }
  eta.h <- array(eta.start, H)
  S.i.counts <- matrix(0, N, H)
  ptm <- proc.time()[3]
  m <- 1
  trickmat <- outer(seq_len(H), seq_len(H), "<=")
  sProbs <- matrix(mapply(function(i, h) prod(xi.h[, , h]^Njk.i[, 
                                                                , i]), rep(1:N, each = H), 1:H), N, H, byrow = TRUE) * 
    matrix(eta.h, N, H, byrow = TRUE)
  if (is.null(Initial$S.i.start)) {
    if (H > 1) {
      vertfkt <- sProbs %*% trickmat
      S.i.temp <- rowSums(vertfkt[, H] * runif(N) > vertfkt) + 
        1
    }
    else {
      S.i.temp <- rep(1, N)
    }
  }
  else {
    S.i.temp <- if (H > 1) 
      Initial$S.i.start
    else rep(1, N)
  }
  if (M0 <= 1) 
    invisible(sapply(1:N, function(i) S.i.counts[i, S.i.temp[i]] <<- S.i.counts[i, 
                                                                                S.i.temp[i]] + 1))
  eta.m <- matrix(0, H, M)
  S.i.temp.ind <- S.i.temp[indizes]
  eta.m[, m] <- if (H > 1) 
    gtools::rdirichlet(n = 1, alpha = NhSfunPat(S.i.temp.ind, 
                                                H, freq) + e0)
  else 1
  xi.m <- array(0, c(M, K + 1, K + 1, H))
  Njk.h.temp <- array(0, c(K + 1, K + 1, H))
  invisible(sapply(1:H, function(h) Njk.h.temp[, , h] <<- apply(Njk.i.ind[, 
                                                                          , S.i.temp.ind == h], c(1, 2), sum)))
  hj.grid <- as.matrix(expand.grid(h = 1:H, j = 1:(K + 1)))
  transMat <- function(h, j) xi.m[m, j, , h] <<- gtools::rdirichlet(n = 1, 
                                                                    alpha = Njk.h.temp[j, , h] + c0[j, , h])
  invisible(mapply(transMat, hj.grid[, 1], hj.grid[, 2]))
  logXiPrior[m] <- sum(mapply(dirDensFun, hj.grid[, 1], hj.grid[, 
                                                                2]))
  logEtaPrior[m] <- log(ddirichlet(eta.m[, m], rep(e0, H)))
  cat("workspaceFile: ", workspaceFile, "  (within current working directory!) \n")
  cat("m =", m, "; duration of iter proc so far: ", round(proc.time()[3] - 
                                                            ptm, 2), "sec. \n")
  flush.console()
  for (m in 2:M) {
    sProbsPart1 <- matrix(mapply(function(i, h) prod(xi.m[m - 
                                                            1, , , h]^Njk.i[, , i]), rep(1:N, each = H), 1:H), 
                          N, H, byrow = TRUE)
    sProbs <- sProbsPart1 * matrix(eta.m[, m - 1], N, H, 
                                   byrow = TRUE)
    tik <- sProbs/rowSums(sProbs)
    sProbsMax <- max.col(sProbs)
    if (H > 1) {
      vertfkt <- sProbs %*% trickmat
      S.i.temp <- rowSums(vertfkt[, H] * runif(N) > vertfkt) + 
        1
    }
    else {
      S.i.temp <- rep(1, N)
    }
    logLike[m - 1] <- sum(log(rowSums(sProbs)))
    logClassLike[m - 1] <- sum(log((sProbsPart1)[cbind(1:N, 
                                                       sProbsMax)] * eta.m[sProbsMax, m - 1]))
    print(paste0('min tik:',min(tik)))
    # entropy[m - 1] <- if (min(tik) > 9.99988867182683e-321) 
    entropy[m - 1] <- -sum(tik * log(tik + 9.99988867182683e-324))
    if (m > M0) 
      sapply(1:N, freqMat)
    S.i.temp.ind <- S.i.temp[indizes]
    eta.m[, m] <- if (H > 1) 
      gtools::rdirichlet(n = 1, alpha = NhSfunPat(S.i.temp.ind, 
                                                  H, freq) + e0)
    else 1
    Njk.h.temp <- array(0, c(K + 1, K + 1, H))
    sapply(1:H, countMat)
    mapply(transMat, hj.grid[, 1], hj.grid[, 2])
    logXiPrior[m] <- sum(mapply(dirDensFun, hj.grid[, 1], 
                                hj.grid[, 2]))
    logEtaPrior[m] <- log(ddirichlet(eta.m[, m], rep(e0, 
                                                     H)))
    if (identical(all.equal(m%%mOut, 0), TRUE) || is.element(m, 
                                                             c(1:5, 10, 20, 50, 100, 200, 500))) {
      cat("m =", m, "; duration of iter proc so far:", 
          round(diff <- proc.time()[3] - ptm, 2), "sec.,  exp time to end:", 
          round((diff/(m - 1) * M - diff)/60, 2), " min. \n")
      flush.console()
    }
    if (identical(all.equal(m%%mSave, 0), TRUE)) 
      save(list = c("Data", "Prior", "Initial", "Mcmc", 
                    "c0", "eta.start", "eta.m", "fileName", "freq", 
                    "indizes", "K", "mcc", "N", "Njk.i", "Njk.i.ind", 
                    "R", "S.i.counts", "workspaceFile", "xi.hat", 
                    "xi.m", if (exists("xi.start")) "xi.start", 
                    "xi.start.ind", "logLike", "logClassLike", 
                    "logXiPrior", "logEtaPrior"), file = file.path(dirName, 
                                                                   paste(startDate, "_temp.RData", sep = "")))
  }
  estGroupSizeMat <- matrix(eta.m[, m], N, Prior$H, byrow = TRUE)
  sProbsPart1 <- matrix(mapply(function(i, h) prod(xi.m[m, 
                                                        , , h]^Njk.i[, , i]), rep(1:N, each = Prior$H), 1:Prior$H), 
                        N, Prior$H, byrow = TRUE)
  tempLK <- estGroupSizeMat * sProbsPart1
  tik <- tempLK/rowSums(tempLK)
  sProbsMax <- max.col(tempLK)
  logLike[m] <- sum(log(rowSums(tempLK)))
  logClassLike[m] <- sum(log((sProbsPart1)[cbind(1:N, sProbsMax)] * 
                               eta.m[sProbsMax, m]))
  print(paste0('min tik2:',min(tik)))
  # entropy[m] <- if (min(tik) > 9.99988867182683e-321) 
  entropy[m] <- -sum(tik * log(tik + 9.99988867182683e-324))
  estTransProb <- if (H > 1) 
    apply(xi.m[M0:M, , , ], c(2, 3, 4), mean)
  else array(apply(xi.m[M0:M, , , ], c(2, 3), mean), c(K + 
                                                         1, K + 1, H))
  dimnames(estTransProb) <- dimnames(Njk.i)
  dimnames(estTransProb)[[3]] <- paste("Group", 1:H)
  estGroupSize <- if (H > 1) 
    apply(eta.m[, M0:M], 1, mean)
  else mean(eta.m[, M0:M])
  logPostDens <- logLike + logXiPrior + logEtaPrior
  mMax <- which.max(logPostDens)
  totalTime <- proc.time()[3] - startTime
  save(list = c("Data", "Prior", "Initial", "Mcmc", "c0", 
                "eta.start", "estGroupSize", "estTransProb", "eta.m", "entropy",
                "fileName", "freq", "indizes", "K", "mcc", "N", "Njk.i", 
                "Njk.i.ind", "R", "S.i.counts", "totalTime", "workspaceFile", 
                "xi.hat", "xi.m", if (exists("xi.start")) "xi.start", 
                "xi.start.ind", "logLike", "logClassLike", 
                "logXiPrior", "logEtaPrior", "logPostDens", "mMax"), 
       file = workspaceFile)
  unlink(file.path(dirName, paste(startDate, "_temp.RData", 
                                  sep = "")))
  cat("Total time:", totalTime%/%3600, "hours", (totalTime%%3600)%/%60, 
      "min \n")
  resultsList <- list(workspaceFile = workspaceFile, Data = Data, 
                      Prior = Prior, Initial = Initial, Mcmc = Mcmc, c0 = c0, 
                      eta.start = eta.start, estGroupSize = estGroupSize, entropy = entropy,
                      estTransProb = estTransProb, eta.m = eta.m, fileName = fileName, 
                      freq = freq, indizes = indizes, K = K, mcc = mcc, N = N, 
                      Njk.i = Njk.i, Njk.i.ind = Njk.i.ind, R = R, S.i.counts = S.i.counts, 
                      totalTime = totalTime, xi.hat = xi.hat, xi.m = xi.m, 
                      xi.start = if (exists("xi.start")) xi.start else NULL, 
                      xi.start.ind = xi.start.ind, logLike = logLike, logClassLike = logClassLike, logXiPrior = logXiPrior, logEtaPrior = logEtaPrior, 
                      logPostDens = logPostDens, mMax = mMax)
  return(invisible(resultsList))
}

modelSel <- function (workDir, myLabel = "model choice for ...", H0 = 3, 
          whatToDoList = c("approxMCL", "approxML", "postMode")) 
{
  prevwd <- getwd()
  setwd(workDir)
  outFileNamesMCC <- list.files(pattern = "[0123456789].RData")
  xval <- numeric(length(outFileNamesMCC))
  Hmax <- Hnr <- length(xval)
  logLike <- logXiPrior <- logEtaPrior <- logClassLike <- xi.m <- K <- Prior <- eta.m <- N <- Njk.i <- Mcmc <- entropy <- c0 <- Initial <- NULL
  toDoIsPostMode <- toDoIsApproxML <- toDoIsApproxMCL <- FALSE
  MSCritList <- list()
  for (whatToDo in whatToDoList) {
    print("MCC")
    print(whatToDo)
    toDoIsPostMode <- if (whatToDo == "postMode") 
      TRUE
    else FALSE
    toDoIsApproxML <- if (whatToDo == "approxML") 
      TRUE
    else FALSE
    toDoIsApproxMCL <- if (whatToDo == "approxMCL") 
      TRUE
    else FALSE
    stopifnot(setequal(c(toDoIsPostMode, toDoIsApproxML, 
                         toDoIsApproxMCL), c(FALSE, TRUE, FALSE)))
    BicMCC <- numeric(Hmax)
    adjBicMCC <- numeric(Hmax)
    AicMCC <- numeric(Hmax)
    AweMCC <- numeric(Hmax)
    IclBicMCC <- numeric(Hmax)
    ClcMCC <- numeric(Hmax)
    Dic2MCC <- numeric(Hmax)
    Dic4MCC <- numeric(Hmax)
    logICLMCC <- numeric(Hmax)
    corrClassMCC <- numeric(Hmax)
    mMaxMCC <- numeric(Hmax)
    if (toDoIsPostMode) 
      maxLogPostDens <- numeric(Hmax)
    if (toDoIsApproxML) 
      maxLogLike <- numeric(Hmax)
    if (toDoIsApproxMCL) 
      maxLogClassLike <- numeric(Hmax)
    for (HH in 1:Hnr) {
      results <- load(outFileNamesMCC[HH])
      logPostDens <- logLike + logXiPrior + logEtaPrior
      if (toDoIsPostMode) {
        mMax <- which.max(logPostDens)
        maxLogPostDens[HH] <- max(logPostDens)
      }
      if (toDoIsApproxML) {
        mMax <- which.max(logLike)
        maxLogLike[HH] <- max(logLike)
      }
      if (toDoIsApproxMCL) {
        mMax <- which.max(logClassLike)
        maxLogClassLike[HH] <- max(logClassLike)
      }
      mMaxMCC[HH] <- mMax
      xiPostMode <- array(xi.m[mMax, , , ], c(K + 1, K + 
                                                1, Prior$H))
      etaPostMode <- eta.m[, mMax]
      estGroupSizeMat <- matrix(etaPostMode, N, Prior$H, 
                                byrow = TRUE)
      tempLK <- estGroupSizeMat * matrix(mapply(function(i, 
                                                         h) prod(xiPostMode[, , h]^Njk.i[, , i]), rep(1:N, 
                                                                                                      each = Prior$H), 1:Prior$H), N, Prior$H, byrow = TRUE)
      LK <- sum(log(rowSums(tempLK)))
      Nobs <- N
      dh <- (K + 1) * K * Prior$H + (Prior$H - 1)
      BicMCC[HH] <- bick <- -2 * LK + dh * log(Nobs)
      adjBicMCC[HH] <- adjbick <- bick - 2 * Prior$H * 
        log(H0) + 2 * lgamma(Prior$H + 1) + 2 * H0
      AicMCC[HH] <- aick <- -2 * LK + 2 * dh
      sProbs <- tempLK
      sProbsNorm <- sProbs/rowSums(sProbs)
      class <- max.col(sProbsNorm)
      CLK <- sum(log((matrix(mapply(function(i, h) prod(xiPostMode[, 
                                                                   , h]^Njk.i[, , i]), rep(1:N, each = Prior$H), 
                                    1:Prior$H), N, Prior$H, byrow = TRUE))[cbind(1:N, 
                                                                                 class)] * etaPostMode[class]))
      AweMCC[HH] <- awek <- -2 * CLK + 2 * dh * (3/2 + 
                                                   log(Nobs))
      tik <- sProbsNorm
      # stopifnot(min(tik) > 9.99988867182683e-321)
      EK <- -sum(tik * log(tik + 9.99988867182683e-324))
      IclBicMCC[HH] <- Iclbic <- -2 * LK + log(Nobs) * 
        dh + 2 * EK
      ClcMCC[HH] <- clc <- -2 * LK + 2 * EK
      Dic2MCC[HH] <- dic2 <- -4 * mean(logLike[Mcmc$M0:Mcmc$M]) + 
        2 * LK
      Dic4MCC[HH] <- dic4 <- dic2 + 2 * mean(entropy[Mcmc$M0:Mcmc$M])
      xval[HH] <- Prior$H
      N_h <- table(class)
      e0jkh <- c0
      alpha0h <- rep(Prior$e0, Prior$H)
      Njkh <- array(0, dim(e0jkh))
      for (h in 1:Prior$H) {
        Njkh[, , h] <- apply(Njk.i[, , class == h], 
                             c(1, 2), sum)
      }
      logICLMCC[HH] <- logicl <- -2 * (sum(colSums(lgamma(apply(e0jkh, 
                                                                c(3), colSums))) - colSums(apply(lgamma(e0jkh), 
                                                                                                 c(3), colSums)) + colSums(apply(lgamma(e0jkh + 
                                                                                                                                          Njkh), c(3), colSums)) - colSums(lgamma(apply(e0jkh + 
                                                                                                                                                                                          Njkh, c(3), colSums)))) + lgamma(sum(alpha0h)) - 
                                         sum(lgamma(alpha0h)) + sum(lgamma(alpha0h + 
                                                                             N_h)) - lgamma(sum(alpha0h + N_h)))
      
      # Causing issues
      # corrClassMCC[HH] <- corrClass <- if (Prior$H > 1) 
      #   if (is.null(Initial$S.i.start)) 
      #     NA
      # else suppressWarnings(round(e1071::classAgreement(table(Initial$S.i.start, 
      #                                                         class))$crand * 100, 5))
      
      print(paste(HH, ". MCC: H =", Prior$H, "; BIC =", 
                  round(bick, 2), "; adjBIC =", round(adjbick, 
                                                      2), "; AIC =", round(aick, 2), "; AWE =", 
                  round(awek, 2), "; CLC =", round(clc, 2), "; IclBic =", 
                  round(Iclbic, 2), "; DIC2 =", round(dic2, 2), 
                  "; DIC4a =", round(dic4, 2), "; ICL =", round(logicl, 
                                                                2), ";"))# adj Rand:", corrClass, "%"))
      flush.console()
      rm(list = setdiff(ls(all = TRUE), c("outFileNamesMCC", 
                                          "Hmax", "whatToDo", "BicMCC", "adjBicMCC", "AicMCC", 
                                          "AweMCC", "H0", "IclBicMCC", "ClcMCC", "Dic2MCC", 
                                          "Dic4MCC",
                                          # "corrClassMCC",
                                          "mMaxMCC", "HH", 
                                          "Hnr", "xval", "myLabel", "logICLMCC", "MSCritList", 
                                          "toDoIsPostMode", "toDoIsApproxML", "toDoIsApproxMCL", 
                                          "prevwd", if (toDoIsPostMode) "maxLogPostDens", 
                                          if (toDoIsApproxML) "maxLogLike", if (toDoIsApproxMCL) "maxLogClassLike")))
    }
    indi <- 1:Hnr
    postscript(paste("MCCModSelCrits_", whatToDo, ".eps", 
                     sep = ""), width = 10, height = 7)
    par(mfrow = c(2, 4), omi = c(0, 0, 0.25, 0), mai = c(0.6, 
                                                         0.3, 0.5, 0.1))
    plot(xval, BicMCC[indi], main = "BIC", xlab = "H", ylab = "")
    plot(xval, adjBicMCC[indi], main = "adjBIC", xlab = "H", 
         ylab = "")
    plot(xval, Dic2MCC[indi], main = "DIC2", xlab = "H", 
         ylab = "")
    plot(xval, AweMCC[indi], main = "AWE", xlab = "H", ylab = "")
    plot(xval, ClcMCC[indi], main = "CLC", xlab = "H", ylab = "")
    plot(xval, IclBicMCC[indi], main = "ICL-BIC", xlab = "H", 
         ylab = "")
    plot(xval, logICLMCC[indi], main = "ICL", xlab = "H", 
         ylab = "")
    plot(xval, Dic4MCC[indi], main = "DIC4a", xlab = "H", 
         ylab = "")
    mtext(paste(myLabel, "MCC", whatToDo, format(Sys.time(), 
                                                 "%d %b %Y"), sep = "   ***   "), outer = TRUE, cex = 1.2)
    dev.off()
    pdf(paste("MCCModSelCrits_", whatToDo, ".pdf", sep = ""), 
        width = 10, height = 7)
    par(mfrow = c(2, 4), omi = c(0, 0, 0.25, 0), mai = c(0.6, 
                                                         0.3, 0.5, 0.1))
    plot(xval, BicMCC[indi], main = "BIC", xlab = "H", ylab = "")
    plot(xval, adjBicMCC[indi], main = "adjBIC", xlab = "H", 
         ylab = "")
    plot(xval, Dic2MCC[indi], main = "DIC2", xlab = "H", 
         ylab = "")
    plot(xval, AweMCC[indi], main = "AWE", xlab = "H", ylab = "")
    plot(xval, ClcMCC[indi], main = "CLC", xlab = "H", ylab = "")
    plot(xval, IclBicMCC[indi], main = "ICL-BIC", xlab = "H", 
         ylab = "")
    plot(xval, logICLMCC[indi], main = "ICL", xlab = "H", 
         ylab = "")
    plot(xval, Dic4MCC[indi], main = "DIC4a", xlab = "H", 
         ylab = "")
    mtext(paste(myLabel, "MCC", whatToDo, format(Sys.time(), 
                                                 "%d %b %Y"), sep = "   ***   "), outer = TRUE, cex = 1.2)
    dev.off()
    print(getwd())
    print(whatToDo)
    MSCritTable <- cbind(H = xval, mMax = mMaxMCC[indi], 
                         if (toDoIsPostMode) 
                           maxLPD = maxLogPostDens[indi], if (toDoIsApproxML) 
                             maxLL = maxLogLike[indi], if (toDoIsApproxMCL) 
                               maxLCL = maxLogClassLike[indi], BIC = BicMCC[indi], 
                         adjBIC = adjBicMCC[indi], AIC = AicMCC[indi], AWE = AweMCC[indi], 
                         CLC = ClcMCC[indi], IclBic = IclBicMCC[indi], DIC2 = Dic2MCC[indi], 
                         DIC4a = Dic4MCC[indi], logICL = logICLMCC[indi]
                         #,adjRand = corrClassMCC[indi] #causes issues
                         )
    print(MSCritTable)
    cat("\n\n")
    if (toDoIsPostMode) 
      MSCritList$postMode <- MSCritTable
    if (toDoIsApproxML) 
      MSCritList$approxML <- MSCritTable
    if (toDoIsApproxMCL) 
      MSCritList$approxMCL <- MSCritTable
    save(list = c("outFileNamesMCC", "Hmax", "whatToDo", 
                  "BicMCC", "adjBicMCC", "AicMCC", "AweMCC", "H0", 
                  "IclBicMCC", "ClcMCC", "Dic2MCC", "Dic4MCC", 
                  # "corrClassMCC", #causes issues
                  "mMaxMCC", "HH", "Hnr", "xval", "myLabel", "logICLMCC", 
                  "MSCritList", "toDoIsPostMode", "toDoIsApproxML", 
                  "toDoIsApproxMCL", "prevwd", "indi", if (toDoIsPostMode) "maxLogPostDens", 
                  if (toDoIsApproxML) "maxLogLike", if (toDoIsApproxMCL) "maxLogClassLike"), 
         file = paste("MCCModSelCrits_", whatToDo, ".RData", 
                      sep = ""))
  }
  print(cbind(outFileNames = outFileNamesMCC), quote = FALSE)
  MSCritList$outFileNames <- outFileNamesMCC
  setwd(prevwd)
  return(invisible(MSCritList))
}

#===============================================================================

# ------- Save entropy_fix.R function script in same folder as this script, or edit below path
# source('entropy_fix.R')  


# Current priority Crit > Bed > OP > Contact; but does this make sense? Trajectory can change in one day
# Critical Care never occurs in isolation (in this dataset)
# Create label for service used in day based on priority outlined above
resource <- resource %>%
  mutate(service = case_when(n_critical_care > 0 ~ 'CC',
                             n_beddays > 0 & n_critical_care == 0 ~ 'BD',
                             n_outpatient > 0 & (n_beddays == 0 & n_critical_care == 0) ~ 'OP',
                             n_gp > 0 & (n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0) ~ 'Contact',
                             n_gp == 0 & n_critical_care == 0 & n_outpatient == 0 & n_beddays == 0 ~ 'None')
  )

# Label encoding 
# 1 - None 
# 2 - GP
# 3 - OP
# 4 - Bed
# 5 - Critical Care
levels = c('None', 'Contact', 'OP', 'BD', 'CC')
resource <- resource %>% mutate(service_idx = as.numeric(factor(service, levels=levels)))


# Create lagged service column to calculate transition frequency matrix
resource <- resource %>% 
  group_by(patient_id) %>% 
  mutate(prev_service_idx = lag(service_idx, n = 1, default = NA)) %>% 
  mutate(prev_service_idx = tidyr::replace_na(prev_service_idx, 1))

n = length(unique(resource$patient_id))
  
# Initial State Vector
initial_state_vector <- resource$service_idx[which(resource$date_indexed==1)]

# Transitions vectors
tr_list <- vector("list", length = n)
patient_list <- unique(resource$patient_id)

for (i in 1:n){
  tr_list[[i]] <- as.integer(resource$service_idx[which(resource$patient_id == patient_list[i])]-1) #added -1
}

njki <- dataListToNjki(tr_list)


# ==============================================================================
max_groupNr <- 8
results <- list()

# Loop through for each cluster number k
for (groupNr in 1:max_groupNr){ 
  results[[groupNr]] <- MCClust(
    Data = list(
      dataFile = njki, ## Transition frequency matrix
      storeDir = myOutfilesDir, # Outpath
      priorFile = NULL), # whether priors are supplied in external file
    Prior = list(H = groupNr, # number of clusters
                 e0 = 4, # Determines the value of the prior parameter of the Dirichlet-prior for the group sizes h
                 c = 1, # used for calculating prior parameter mx  ---- ONLY WHEN PRIORFILE = FALSE
                 cOff = 1, # used for calculating prior parameter mx ---- ONLY WHEN PRIORFILE = FALSE 
                 usePriorFile = FALSE, # prior info for transition probabilities provided as priorfile
                 xiPooled = FALSE, # set true only if priorfile = True
                 N0 = 5), # Determines a parameter for use in calculating the prior parameter matrix
    Initial = list(xi.start.ind = 3, 
                   pers = 0.7,
                   S.i.start = as.integer(initial_state_vector)),
    Mcmc = list(M = 150,
                M0 = 30,
                mOut = 50,
                mSave = 10,
                seed = 1234))
}


modelSel(workDir=myOutfilesDir, myLabel = "model choice for ...",
                 H0 = 3,
                 whatToDoList =c("approxMCL", "approxML", "postMode"))


