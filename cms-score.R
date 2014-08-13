library(rpf)
library(OpenMx)
source("cms-score-lib.R")

load("cms-fit.rda")

ifa.score <- function(grp, df) {
  grp$data <- df
  EAPscores(grp)[,1]
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
  
  training.names <- c("msNotion", "msYearn", "msEnv", "msAllow", "msCause",
                      "msMet", "msMetNum", "msShared", "msSharedNum",
                      "msTeach", "msTeachNum", "msTrainTeach", "msTrainTeachNum")
  if (0) {
    # It is not clear whether participants can follow direction consistently
    # enough to enable this assumption. Some of the longitudinal participants
    # skipped the training items for the middle measurement leading to strange
    # trajectories.
    lim <- df[1,training.names]
    for (c in 1:ncol(lim)) {
      if (is.factor(lim[1,c])) lim[1,c] <- levels(lim[1,c])[1]
      else lim[1,c] <- 0
    }
    lim <- cms.testlets(lim)
    cms[df$skipExp==TRUE,'training'] <- ifa.score(tr.grp, lim)
  }
  
  eventNames <- c('freqCause', 'successCat', 'maxDuration')
  lim <- df[1,eventNames]
  for (c in 1:ncol(lim)) {
    if (is.factor(lim[1,c])) lim[1,c] <- levels(lim[1,c])[1]
    else lim[1,c] <- 0
  }
  lim <- cms.testlets(lim)
  lowest <- apply(df[,eventNames], 1, function(row) {
    # It would be nice if we could assume that all missing meant the lowest
    # event score but longitudinal trajectories look strange if the assumption
    # is made.
    if (all(is.na(row))) return(FALSE)
    all(row[!is.na(row)] == lim[!is.na(row)])
  })
  # df[which(lowest), eventNames]
  cms[lowest, 'event'] <- ifa.score(ev.grp, lim)
  
  notSure.names <- c("msEffort", "msEmo", "msAfraid", "msFast", "msLife",  "msIdentity")
  mask <- apply(df, 1, function(r) all(r == "not sure"))
  mask <- !is.na(mask) & mask
  cms[mask,] <- NA
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
