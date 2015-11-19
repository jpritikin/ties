library(rpf)
library(plyr)
library(OpenMx)
source("measures.R")

mkspec <- function(espt, items, factors=1) {
  spec <- list()
  for (n in 1:length(items)) {
    lev <- length(levels(espt[[ items[n] ]]))
    if (lev == 2) {
      spec[n] <- rpf.grm(outcomes=2, factors)
    } else {
        if (lev < 3) stop(paste(items[n], "has only", lev, "levels"))
      spec[n] <- rpf.nrm(lev, factors)
    }
  }
  names(spec) <- items
  spec
}

set.nominal.rank <- function(spec, ip.mat, name, a, c) {
  spec1 <- spec[[name]]
  if (is.null(spec1)) {
    return(ip.mat)
  }
  if (!is(spec1, 'rpf.mdim.nrm')) stop(paste("spec is",class(spec),"not nominal"))
  thresh <- spec1$outcomes-1
  free <- rep(FALSE, spec1@factors + 2 * thresh)
  free[1] <- TRUE
  base <- spec1$factors+1
  if (thresh * a >= 1) {
    free[(1+base):(base + thresh * a - 1)] <- TRUE
  }
  base <- base + thresh
  numGamma <- max(thresh * c, 2L)
  free[base:(base + numGamma - 1)] <- TRUE
  ip.mat$free[1:length(free),name] <- free
  sv <- c(rep(1,spec1@factors), rep(1, thresh), rep(0, thresh))
  ip.mat$values[1:length(free),name] <- free * sv
  ip.mat$values[c(1,spec1@factors+1),name] <- 1
  ip.mat
}

num2cat <- function(num) {
    num <- as.numeric(num)
  zero <- num <= 0
  high <- num >= 1000
  level.coef <- 2
  kat <- round(level.coef * log10(num)) + 1
  kat[zero] <- 0
  kat[high] <- round(level.coef * log10(1000))+1
  kat
}

# "msNotion" -- same question better measured by other items
# msDescarte -- poor fit
# msEmo -- like a combination of msLife, msId

cms.testlets <- function(df) {
    if (TRUE) {
      nscol <- intersect(colnames(df), c("msEffort", "msAfraid", "msFast",
                               "msLife", "msIdentity", "msPreoccu", "msDescarte",
                               "msEmo"))
        df2 <- sapply(df[,nscol], unclass)
        mask <- apply(df2, 1, function(x) sum(x==3, na.rm=TRUE)) >= (length(nscol)-1)
        df[mask,nscol] <- NA
    }

  if (!is.null(df$msFast)) {
    df$msFast1 <- df$msFast
    df$msFast1[df$msFast1 == "disagree somewhat"] <- "not sure"
    df$msFast1 <- mxFactor(df$msFast1, levels(df$msFast1)[-4])
    #table(df$msFast1)
  }
  
  if (!is.null(df$wantLearn)) {
    df$msIdAfraid <- ordered(unclass(df$msAfraid) + unclass(df$msIdentity) - 10, levels=seq(-8,0,1))
    df$msIdAfraidLearn <- ordered(unclass(df$wantLearn) + unclass(df$msAfraid) + unclass(df$msIdentity) - 10,
                                  levels=seq(-7,5,1))
    df$msFastEffort <- ordered(unclass(df$msFast) + unclass(df$msEffort) - 10, levels=seq(-8,0,1))
    df$msFastEffortLife <- ordered(unclass(df$msFast) + unclass(df$msEffort) + unclass(df$msLife) - 15,
                                   levels=seq(-12,0,1))
  }
    if (is.null(df[['msEffortX']])) {
        MSAgreementItemX = tolower(c('Not sure','Agree','Agree somewhat','Disagree somewhat','Disagree'))
        df$msEffortX <- mxFactor(df$msEffort, MSAgreementItemX)
    }

  for (col in c('msMet', 'msShared', 'msTeach', 'msTrainTeach')) {
    ncol <- paste0(col, "Num")
    if (is.null(df[[ncol]])) {
        df[[ncol]] <- ordered(NA)
        next
    }
    if (is.factor(df[[ncol]])) next
    df[[ncol]] <- mxFactor(num2cat(df[[ncol]]), levels=0:7)
  }
  
  if (!is.null(df$msCause)) {
    df$trainSkill <- ordered(unclass(df$msMet) + unclass(df$msShared) +
                               unclass(df$msTeach) + unclass(df$msTrainTeach) - 4, levels=0:16)
    df$allowCause <- ordered(mean.or.na(df[,c('msAllow','msCause'),drop=FALSE], 1), levels=seq(1,5,.5))
    df$yearnEnv <- ordered(mean.or.na(df[,c("msYearn", "msEnv"),drop=FALSE], 1), levels=seq(1,5,.5))
    
    if (!is.null(df$msMetNum)) {
        df$trainNum <- ordered(unclass(df$msMetNum) + unclass(df$msSharedNum) +
                                 unclass(df$msTeachNum) + unclass(df$msTrainTeachNum) - 4, levels=0:(4*4))
    } else {
        df$trainNum <- ordered(NA, levels=0:(4*4))
    }
  }
  if (is.null(df$freqCause)) {
      df$freqCause <- ordered(NA, levels=0:4)   # data from old version
  }
  if (!is.null(df$pctSuccess)) {
    df$pctSuccess[nchar(df$pctSuccess)==0] <- NA
    pct <- round(as.numeric(df$pctSuccess))
    pct[pct < 0] <- 0
    pct[pct > 100] <- 100
    # no need to isolate 100 from (80,99] ? TODO
    df$successCat <- cut(pct, breaks=c(-1,seq(1,99,length.out=6),100),
                         ordered_result=TRUE)
    }
    if (!is.null(df$pctAsleep)) {
        df$pctAsleep[nchar(df$pctAsleep)==0] <- NA
        pct <- round(as.numeric(df$pctAsleep))
        pct[pct < 0] <- 0
        pct[pct > 100] <- 100
        # probably not a log scale? TODO
        df$alseepCat <- cut(pct, breaks=seq(-.1,100,length.out=6), ordered_result=TRUE)
        # reverse order
        df$asleepCat <- mxFactor(df$alseepCat, levels=rev(levels(df$alseepCat)))
    }
  if (!is.null(df$pctRelaxed)) {
    df$pctRelaxed[nchar(df$pctRelaxed)==0] <- NA
    pct <- round(as.numeric(df$pctRelaxed))
    pct[pct < 0] <- 0
    pct[pct > 100] <- 100
    # increase to 7 or 9 breaks? TODO
    df$relaxedCat <- cut(pct, breaks=seq(-.1,100,length.out=6), ordered_result=TRUE)
  }
  for (col in c('msPreoccu', 'msTimeAlloc', 'maxDurationOut', 'msTaught',
                  'thinkTime', 'thinkFuture', 'thinkPast', 'physicalLoc', 'phyLocDepth',
                  'spon1', 'spon2', 'sponDepth', 'relaxedCat', 'asleepCat', 'waveAfter', 'waveBefore')) {
    if (is.null(df[[col]])) df[[col]] <- factor(NA)
  }
  df
}

ties.spam.score <- function(cube) {
  # cube should be a person by item by time array
  
  successConfusion <- aaply(cube, 1, function(vec) {
    confused <- (vec['successCat',] == 1 & vec['maxDuration',] > 1) |
      (vec['successCat',] > 1 & vec['maxDuration',] == 1)
    confused <- !is.na(confused) & confused
    out <- rep(0, ncol(vec))
    if (any(confused)) {
      penalty <- abs(vec['successCat',] - vec['maxDuration',])
      out[confused] <- penalty[confused]
    }
    out
  })

  oneWayItems <- c('msMetNum','msSharedNum', 'msTeachNum','msTrainTeachNum',
                   'msEffort', 'msIdentity', 'msAfraid')
  backward <- apply(cube[,oneWayItems,], 1:2, function(vec) {
    vec <- vec[!is.na(vec)]
    if (length(vec) < 2) return(NA)
    dv <- diff(vec)
    dv[dv > 0] <- 0
    sum(dv)
  })
  
  spammer <- apply(backward, 1, sum, na.rm=TRUE) - rowSums(successConfusion)
  names(spammer) <- rownames(cube)
  spammer
}
