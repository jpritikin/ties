library(rpf)
library(OpenMx)
source('irtplot.R')

load("espt.rda")

gpcm <- function(outcomes) {
  rpf.nrm(outcomes, factors=2)
  #   rpf.nrm(outcomes, T.c=diag(outcomes-1))
}

# 'msEvery'  possibly too vague, better measured by other items
#     # too much missingness, revisit later
# 'msAllow',                  # drop this or msCause, need larger sample
# 'msDescarte'               # poor performer, revisit later
# 'msCause0'    # redundent
# 'msCause', 'msTeach', 'msTrainTeach' -- replace with causeTeach testlet
# 'causeTeach'                # testlet item, too much missingness
# 'durationCharacter' ??
# 'msAny'  # weak item
# 'skipExp'   # redundent

item.names <- c('skipInt', 'msFreqTestlet','msDescarte',
                   'msIdAfraidLearn', 'msFastEffortLife',
                'msFreqDur', 'msYEC', 'msMetSharedTeach')

# ensure we have the data we think we have
missing <- item.names[is.na(match(item.names, colnames(espt)))]
if (length(missing)) stop(paste("Columns missing:", missing))

sapply(espt[item.names], function (c) sum(!is.na(c)))  # per item sample size

spec <- list()
for (n in 1:length(item.names)) {
  lev <- length(levels(espt[[ item.names[n] ]]))
  if (lev == 2) {
    spec[n] <- rpf.grm(outcomes=2, factors=2)
  } else {
    spec[n] <- gpcm(lev)
  }
}
names(spec) <- item.names

#sapply(spec, function(m) slot(m,'numOutcomes'))

when <- strptime(espt[['instrument']], "%Y-%m-%d")
revision1 <- strptime("2013-02-11", "%Y-%m-%d")
ver.mask <- difftime(when, revision1) > 0

manocha2013.mask <- (espt$time == 2 | (espt$time == 1 & !(espt$id %in% espt[espt$time == 2, 'id'])))
ver.mask[espt$wave == "manocha2013"] <- manocha2013.mask[espt$wave == "manocha2013"]

data <- espt[ver.mask, item.names]

sapply(data, function(r) sum(!is.na(r)))  # per item sample size

numItems <- length(item.names)
maxParam <- max(sapply(spec, rpf.numParam))

ip.mat <- mxMatrix(name="ItemParam", nrow=maxParam, ncol=numItems, free=FALSE)
ip.mat@free[,] <- FALSE
colnames(ip.mat@free) <- item.names
colnames(ip.mat@values) <- item.names
rownames(ip.mat@free) <- c("interest", "experience", rep(NA, maxParam-2))
interest <- c('wantLearn', "msAfraid", "msEmo","msLife", "msFastEffort", "msFastEffortLife",
            "msDescarte", "msIdentity", 'msIdAfraidLearn', 'msFreqTestlet')
experience <- c('skipInt', 'msNotion', "msAny", "maxDuration", 'skipExp', "msYearn", "msCause0","msCause",
                "msAllow", "msMetShared", "msMet", "msEnv","msShared","msTeach","msTrainTeach", "causeTeach",
                "msFreqDur", "msYearnEnv", "msMetSharedTeach", "msYEC")
ip.mat@free["interest", setdiff(item.names, experience)] <- TRUE
ip.mat@free["experience", setdiff(item.names, interest)] <- TRUE
#ip.mat@free[4,] <- TRUE  # just for fun
for (ix in 1:numItems) {
  i <- rpf.paramInfo(spec[[ix]])
  ptype <- unlist(i)[seq(match('type', rownames(i)),length(i),length(rownames(i)))]
  ip.mat@values[which(ptype==1), ix] <- 1
  ip.mat@values[which(ptype==2), ix] <- 0
  ip.mat@free[which(ptype==2), ix] <- TRUE
}
ip.mat@values[!ip.mat@free] <- 0
ip.mat@values[3,] <- 1

set.nominal.rank <- function(ip.mat, name, a, c) {
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
  ip.mat
}
ip.mat <- set.nominal.rank(ip.mat, 'causeTeach', .27, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msIdAfraid', .27, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msFastEffort', .27, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msFastEffortLife', .27, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msIdAfraidLearn', .27, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msFreqTestlet', .27, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msIdAfraidLearn', .4, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msLife', .27, 1)
ip.mat <- set.nominal.rank(ip.mat, 'msMetShared', .27, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msFreqDur', .27, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msYearnEnv', .27, .5)
ip.mat <- set.nominal.rank(ip.mat, 'msMetSharedTeach', .27, .4)
ip.mat <- set.nominal.rank(ip.mat, 'msYEC', .27, .4)

m.mat <- mxMatrix(name="mean", nrow=1, ncol=2, values=c(0,0), free=FALSE)
est.cov <- ip.mat@values[1,] %*% ip.mat@values[2,] != 0
cov.mat <- mxMatrix("Symm", name="cov", nrow=2, ncol=2, values=diag(2),
                    free=c(FALSE,est.cov,FALSE), labels=c("v1","cov12","v2"))

m2 <- mxModel(model="2d",
              m.mat, cov.mat, ip.mat,
              mxData(observed=data, type="raw"),
              mxExpectationBA81(mean="mean", cov="cov",
                                ItemSpec=spec, ItemParam="ItemParam",
                                qpoints=21, qwidth=5, scores="full"),
              mxFitFunctionML(),
              mxComputeEM('expectation',
                          mxComputeNewtonRaphson(free.set='ItemParam'),
                          mxComputeOnce('fitfunction', free.set=c("mean", "cov"), fit=TRUE),
                          verbose=2L))
  #  m2 <- mxOption(m2, "Number of Threads", 1)
m2.est <- mxRun(m2)
print(m2.est@fitfunction@result)
rownames(m2.est@matrices$ItemParam@values) <- c("interest", "experience", rep(NA, maxParam-2))
print(m2.est@matrices$ItemParam@values[1:2,])
print(m2.est@matrices$cov@values)

if (1) {
  fixed.ip <- m2.est@matrices$ItemParam
  fixed.ip@free[,] <- FALSE
  eap <- mxModel(m2, fixed.ip, mxData(observed=espt[espt$wave == "manocha2013", item.names], type="raw"),
                 mxComputeOnce('expectation', context='EM'))
  eap.est <- mxRun(eap)
  manocha2013sc <- eap.est@expectation@scores.out[,1:2]
  save(manocha2013sc, file="manocha2013sc.rda")
}

latentVars <- c("interest", "experience")
grp <- list(spec=spec,
            param=m2.est@matrices$ItemParam@values,
            free=apply(ip.mat@free, 2, sum),
            mean=m2.est@matrices$mean@values,
            cov=m2.est@matrices$cov@values,
            scores=m2.est@expectation@scores.out,
            data=data)

print(cor(grp$scores[,1], grp$scores[,2]))

# design needed for two-tier? TODO
colnames(grp$mean) <- latentVars
dimnames(grp$cov) <- list(latentVars, latentVars)

if (0) {
  # doesn't make sense
  got <- chen.thissen.1997(grp, data)
  max(got$pval[!is.na(got$pval)])
}

#p <- rpf.plot(grp, "causeTeach", width=4, data.bins=13, basis=c(0,1), factor=2)
#print(p)

booklet(function(item) {
  rpf.plot(grp, item, width=4, data.bins=12, basis=c(1,0), factor=1)
}, item.names[grp$param[1,] > 0], output="interest.pdf")

booklet(function(item) {
  rpf.plot(grp, item, width=4, data.bins=12, basis=c(0,1), factor=2)
}, item.names[grp$param[2,] > 0], output="experience.pdf")

if (0) {
  map1 <- item.map(grp, 1)
  ggplot(map1, aes(x=score, y=item.name, label=outcome)) + geom_text()
  map2 <- item.map(grp, 2)
  ggplot(map2, aes(x=score, y=item.name, label=outcome)) + geom_text()
}

if (0) {
  got1 <- chen.thissen.1997(grp, data, item.names[m2.est@matrices$ItemParam@values[1,] != 0])
  got2 <- chen.thissen.1997(grp, data, item.names[m2.est@matrices$ItemParam@values[2,] != 0])
  
  c(max(got1$pval[!is.na(got1$pval)]), max(got2$pval[!is.na(got2$pval)]))
}

if (0) {
  # try to figure out ethical items
  scores <- m2.est@expectation@scores.out
  espt$interest <- scores[,1]
  espt$experience <- scores[,2]
  got <- list()
  for (item in c("ethical")) {
    for (l in levels(espt[[item]])) {
      got[[paste(item, l, sep="=")]] <- c(mean(espt[espt[[item]] == l, 'interest'], na.rm=TRUE),
                                          mean(espt[espt[[item]] == l, 'experience'], na.rm=TRUE))
    }
  }
  ethical <- do.call(rbind, got)
  colnames(ethical) <- c("interest","experience")
  ethical[order(ethical[,"experience"]),]
}

if (0) {
  library(ggplot2)
  library(stringr)
  scores <- m2.est@expectation@scores.out
  espt$interest <- scores[,1]
  espt$experience <- scores[,2]
  ggplot(subset(espt, !is.na(str_match(espt$wave, "^germano"))),
         aes(interest, experience, color=wave, group=email)) +
    geom_point(size=1) + geom_line(size=.1) + coord_fixed()
}

#items <- t(m2@matrices$ItemParam@values)
#rownames(items) <- item.names
#espt[mask, "score"] <- m2@expectation@scores.out[,1]
#espt[mask, "se"] <- m2@expectation@scores.out[,2]

if (0) {
  sc <- m2.est@expectation@scores.out
  for (l in 1:3) {
    mask <- espt$durationCharacter==levels(espt$durationCharacter)[l]
    mask <- !is.na(mask) & mask
    print(paste(l, mean(sc[mask,1]), mean(sc[mask,2])))
  }
}

if (0) {
  v1 <- got$detail$"msTrainTeach:msTeach"
  chisq(v1$observed, v1$expected)
  ptw2011.gof.test(v1$observed, v1$expected)
}
