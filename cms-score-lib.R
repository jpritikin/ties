library(rpf)
library(OpenMx)
source("measures.R")

mkspec <- function(espt, items) {
  spec <- list()
  for (n in 1:length(items)) {
    lev <- length(levels(espt[[ items[n] ]]))
    if (lev == 2) {
      spec[n] <- rpf.grm(outcomes=2, factors=1)
    } else {
      spec[n] <- rpf.nrm(lev, factors=1)
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
  thresh <- spec1@outcomes-1
  free <- rep(FALSE, spec1@factors + 2 * thresh)
  base <- spec1@factors+1
  free[base:(base + thresh * a - 1)] <- TRUE
  base <- base + thresh
  free[base:(base + thresh * c - 1)] <- TRUE
  ip.mat@free[1:length(free),name] <- free
  sv <- c(1, rep(1, thresh), rep(0, thresh))
  ip.mat@values[1:length(free),name] <- free * sv
  ip.mat@values[1,name] <- 1
  ip.mat
}

num2cat <- function(num) {
  zero <- num <= 0
  high <- num >= 1001
  level.coef <- 1  # try 2 once we have more data
  kat <- round(level.coef * log10(num)) + 1
  kat[zero] <- 0
  kat[high] <- round(level.coef * log10(1000))+1
  kat
}

# "msNotion" -- same question better measured by other items
# msDescarte -- poor fit
# msEmo -- like a combination of msLife, msId

cms.testlets <- function(df) {
  if (!is.null(df$wantLearn)) {
    df$msIdAfraid <- ordered(unclass(df$msAfraid) + unclass(df$msIdentity) - 10, levels=seq(-8,0,1))
    df$msIdAfraidLearn <- ordered(unclass(df$wantLearn) + unclass(df$msAfraid) + unclass(df$msIdentity) - 10,
                                  levels=seq(-7,5,1))
    df$msFastEffort <- ordered(unclass(df$msFast) + unclass(df$msEffort) - 10, levels=seq(-8,0,1))
    df$msFastEffortLife <- ordered(unclass(df$msFast) + unclass(df$msEffort) + unclass(df$msLife) - 15,
                                   levels=seq(-12,0,1))
  }
  if (!is.null(df$msCause)) {
    df$trainSkill <- ordered(unclass(df$msMet) + unclass(df$msShared) +
                               unclass(df$msTeach) + unclass(df$msTrainTeach) - 4, levels=0:16)
    df$allowCause <- ordered(mean.or.na(df[,c('msAllow','msCause'),drop=FALSE], 1), levels=seq(1,5,.5))
    df$yearnEnv <- ordered(mean.or.na(df[,c("msYearn", "msEnv"),drop=FALSE], 1), levels=seq(1,5,.5))
    
    df$trainNum <- ordered(num2cat(df$msMetNum) + num2cat(df$msSharedNum) +
                             num2cat(df$msTeachNum) + num2cat(df$msTrainTeachNum), levels=0:(4*4))
  }
  if (!is.null(df$pctSuccess)) {
    df$pctSuccess[nchar(df$pctSuccess)==0] <- NA
    # TODO try 10 categories when we have more data
    df$successCat <- cut(as.numeric(df$pctSuccess),
                         breaks=seq(0,100,length.out=8), ordered_result=TRUE)
  }
  df
}
