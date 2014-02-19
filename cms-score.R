library(rpf)
library(OpenMx)
source("cms-score-lib.R")

load("cms-fit.rda")

ifa.score <- function(grp, df) {
  ip.mat <- mxMatrix(name="ItemParam", values=grp$param)
  m.mat <- mxMatrix(name="mean", nrow=1, ncol=length(grp$mean), values=grp$mean)
  cov.mat <- mxMatrix("Symm", name="cov", values=grp$cov)
  
  items <- colnames(grp$param)
  ba.data <- df[,items]
  ba.mask <- apply(ba.data, 1, function(r) any(!is.na(r)))
  
  ba <- mxModel(model="score",
                m.mat, cov.mat, ip.mat,
                mxData(observed=ba.data[ba.mask,], type="raw"),
                mxExpectationBA81(mean="mean", cov="cov",
                                  ItemSpec=grp$spec, ItemParam="ItemParam", scores="full"),
                mxComputeOnce('expectation'))
  ba.est <- mxRun(ba, silent=TRUE)

  sc <- ba.est@expectation@output$scores
  got <- matrix(NA, nrow(df), ncol(sc))
  got[ba.mask,] <- sc
  got
}

cms.score <- function(df) {
  df <- cms.testlets(df)
  cms <- cbind(barrier=-ifa.score(ba.grp, df)[,1],
               training=ifa.score(tr.grp, df)[,1],
               event=ifa.score(ev.grp, df)[,1])

  barrier.names <- c("wantLearn", "msEffort", "msEmo", "msDescarte", "msAfraid",
                     "msFast", "msLife",  "msIdentity")
  lim <- df[1,barrier.names]
  for (c in 1:ncol(lim)) {
    lev <- levels(lim[1,c])
    lim[1,c] <- lev[length(lev)]
  }
  lim <- cms.testlets(lim)
  cms[df$skipInt==TRUE,'barrier'] <- -ifa.score(ba.grp, lim)[,1]
  
  training.names <- c("msYearn", "msEnv", "msAllow", "msCause",
                      "msMet", "msMetNum", "msShared", "msSharedNum",
                      "msTeach", "msTeachNum", "msTrainTeach", "msTrainTeachNum")
  lim <- df[1,training.names]
  for (c in 1:ncol(lim)) {
    if (is.factor(lim[1,c])) lim[1,c] <- levels(lim[1,c])[1]
    else lim[1,c] <- 0
  }
  lim <- cms.testlets(lim)
  cms[df$skipExp==TRUE,'training'] <- ifa.score(tr.grp, lim)[,1]
  cms
}

if (0) {
  load("espt.rda")
  cms <- cms.score(espt)
  apply(cms, 2, function(c) sum(is.na(c))) / nrow(cms)
  
  cor(cms, use="pairwise.complete.obs")
#             barrier   training      event
#   barrier   1.0000000 -0.2704082 -0.4168304
#   training -0.2704082  1.0000000  0.4755085
#   event    -0.4168304  0.4755085  1.0000000
  if (0) {
    require(ggplot2)
    qplot(cms[,'barrier'], cms[,'event'])
    qplot(cms[,'training'], cms[,'event'])
  }
  
  summary(lm(event ~ ., data=as.data.frame(cms)))
}
