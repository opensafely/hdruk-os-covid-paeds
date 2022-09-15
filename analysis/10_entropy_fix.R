test_wo_entrpy <- function (Data = list(dataFile = stop("'dataFile' (=> Njk.i) must be specified: either 'filename' (path) or data"), 
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
  # entropy <- numeric(M)
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
    # entropy[m - 1] <- if (min(tik) > 9.99988867182683e-321) 
      -sum(tik * log(tik))
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
  # entropy[m] <- if (min(tik) > 9.99988867182683e-321) 
    -sum(tik * log(tik))
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
                "eta.start", "estGroupSize", "estTransProb", "eta.m", 
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
                      eta.start = eta.start, estGroupSize = estGroupSize, 
                      estTransProb = estTransProb, eta.m = eta.m, fileName = fileName, 
                      freq = freq, indizes = indizes, K = K, mcc = mcc, N = N, 
                      Njk.i = Njk.i, Njk.i.ind = Njk.i.ind, R = R, S.i.counts = S.i.counts, 
                      totalTime = totalTime, xi.hat = xi.hat, xi.m = xi.m, 
                      xi.start = if (exists("xi.start")) xi.start else NULL, 
                      xi.start.ind = xi.start.ind, logLike = logLike, logClassLike = logClassLike, logXiPrior = logXiPrior, logEtaPrior = logEtaPrior, 
                      logPostDens = logPostDens, mMax = mMax)
  return(invisible(resultsList))
}
