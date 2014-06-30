library(rpf)
library(OpenMx)
source("cms-score-lib.R")

load("cms-fit.rda")

ifa.score <- function(grp, df) {
  grp$data <- df
  EAPscores(grp, naAction = 'pass')[,1]
}

cms.score <- function(df) {
  df <- cms.testlets(df)
  cms <- cbind(barrier=-ifa.score(ba.grp, df),
               training=ifa.score(tr.grp, df),
               event=ifa.score(ev.grp, df))

  barrier.names <- c("wantLearn", "msEffort", "msEmo", "msDescarte", "msAfraid",
                     "msFast", "msLife",  "msIdentity")
  lim <- df[1,barrier.names]
  for (c in 1:ncol(lim)) {
    lev <- levels(lim[1,c])
    lim[1,c] <- lev[length(lev)]
  }
  lim <- cms.testlets(lim)
  cms[df$skipInt==TRUE,'barrier'] <- -ifa.score(ba.grp, lim)
  
  training.names <- c("msYearn", "msEnv", "msAllow", "msCause",
                      "msMet", "msMetNum", "msShared", "msSharedNum",
                      "msTeach", "msTeachNum", "msTrainTeach", "msTrainTeachNum")
  lim <- df[1,training.names]
  for (c in 1:ncol(lim)) {
    if (is.factor(lim[1,c])) lim[1,c] <- levels(lim[1,c])[1]
    else lim[1,c] <- 0
  }
  lim <- cms.testlets(lim)
  cms[df$skipExp==TRUE,'training'] <- ifa.score(tr.grp, lim)
  
  eventNames <- c('freqCause', 'successCat', 'maxDuration')
  lim <- df[1,eventNames]
  for (c in 1:ncol(lim)) {
    if (is.factor(lim[1,c])) lim[1,c] <- levels(lim[1,c])[1]
    else lim[1,c] <- 0
  }
  lim <- cms.testlets(lim)
  lowest <- apply(df[,eventNames], 1, function(row) {
    if (all(is.na(row))) return(TRUE)
    all(row[!is.na(row)] == lim[!is.na(row)])
  })
  # df[which(lowest), eventNames]
  cms[lowest, 'event'] <- ifa.score(ev.grp, lim)
  cms
}

if (0) {
  load("espt.rda")
  cms <- cms.score(espt)
  apply(cms, 2, function(c) sum(is.na(c))) / nrow(cms)
  
  cor(cms, use="pairwise.complete.obs")
#   barrier training event
#   barrier     1.00    -0.27 -0.38
#   training   -0.27     1.00  0.70
#   event      -0.38     0.70  1.00
  if (0) {
    require(ggplot2)
    qplot(cms[,'barrier'], cms[,'event'])
    qplot(cms[,'training'], cms[,'event'])
  }
  
  summary(lm(event ~ ., data=as.data.frame(cms)))
}
