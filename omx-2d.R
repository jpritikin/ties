library(rpf)
library(OpenMx)
source('irtplot.R')

load("espt.rda")

gpcm <- function(outcomes) {
  rpf.nrm(outcomes, T.c=lower.tri(diag(outcomes-1),TRUE) * -1, factors=2)
  #   rpf.nrm(outcomes, T.c=diag(outcomes-1))
}

item.names <- c('msNotion','msFreq', 'msAny', 'msEvery', 'msCause0', 'wantLearn',
                   'msAfraid', 'msEmo', 'msLife', 'msFast', 'msDescarte', 'msIdentity',
                   'freqCause', 'maxDuration', 'msYearn', 'msMet', 'msEnv', 'msCause', 'msAllow',
                   'msShared',  'msTeach', 'msTrainTeach')

# ensure we have the data we think we have
missing <- item.names[is.na(match(item.names, colnames(espt)))]
if (length(missing)) stop(paste("Columns missing:", missing))

spec <- list()
spec[1:length(item.names)] <- gpcm(5)
names(spec) <- item.names
spec["msFreq"] <- gpcm(4)
spec["msCause0"] <- gpcm(3)
spec["wantLearn"] <- gpcm(4)
spec["freqCause"] <- gpcm(4)
spec["maxDuration"] <- gpcm(4)
#spec["ethical"] <- rpf.nrm(25, factors=2)

#sapply(spec, function(m) slot(m,'numOutcomes'))

data <- espt[,item.names]
for (c in colnames(data)) { attr(data[,c], 'mxFactor') <- attr(espt[,c], 'mxFactor') }

if (0) {
  g.wave1 <- espt[,'wave'] == 'germano2013-1'
  g.wave2 <- espt[,'wave'] == 'germano2013-2'
  g.wave3 <- espt[,'wave'] == 'germano2013-3'
  other <- !(g.wave1 | g.wave2 | g.wave3)
  col.mask <- apply(!is.na(data[g.wave1[mask],]), 2, any)
  which(apply(!is.na(data[g.wave1[mask],]), 2, any))
  
  fm <- sapply(data, unclass) - 1
  fm[is.na(fm)] <- -9
  write.table(fm[g.wave1[mask],col.mask], file="fm-ms-g1.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  write.table(fm[g.wave2[mask],col.mask], file="fm-ms-g2.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  write.table(fm[g.wave3[mask],col.mask], file="fm-ms-g3.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  write.table(fm[other[mask],col.mask], file="fm-ms-main.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  
  #  write.table(fm, file="ms-data.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  
}

numItems <- length(item.names)
maxParam <- max(sapply(spec, rpf.numParam))

ip.mat <- mxMatrix(name="ItemParam", nrow=maxParam, ncol=numItems, free=FALSE)
ip.mat@free[,] <- FALSE
colnames(ip.mat@free) <- item.names
colnames(ip.mat@values) <- item.names
rownames(ip.mat@free) <- c("interest", "experience", rep(NA, maxParam-2))
# ip.mat@free["know",c("msFreq", "msAny", "msEvery", "msCause0",    #fm 1
#                      "maxDuration", "msYearn", "msMet", "msEnv",
#                      "msCause", "msAllow", "msShared", "msTeach",
#                      "msTrainTeach", "ethical")] <- TRUE
# ip.mat@free["desire",c("msNotion","msEvery", "msCause0","wantLearn",    #fm 2
#                        "msAfraid","msEmo","msLife", "msFast",
#                        "msDescarte", "msIdentity", "freqCause",
#                        "maxDuration", "msYearn", "msMet", "msEnv",
#                        "msAllow", "ethical")] <- TRUE
interest <- c("msNotion","msAfraid","msEmo","msLife", "msFast",
            "msDescarte", "msIdentity")
experience <- c("msAny", "maxDuration", "msCause0","msCause","msAllow","msShared","msTeach","msTrainTeach")
ip.mat@free["interest", setdiff(item.names, experience)] <- TRUE
ip.mat@free["experience", setdiff(item.names, interest)] <- TRUE
#ip.mat@free[4,] <- TRUE  # just for fun
for (ix in 1:numItems) {
  i <- rpf.paramInfo(spec[[ix]])
  ptype <- unlist(i)[seq(match('type', rownames(i)),length(i),length(rownames(i)))]
  ip.mat@values[which(ptype==0), ix] <- 1
  ip.mat@values[which(ptype==1), ix] <- 0
  ip.mat@free[which(ptype==1), ix] <- TRUE
}
#ip.mat@free[4,"ethical"] <- TRUE
#ip.mat@free[48:50,"ethical"] <- FALSE
ip.mat@values[!ip.mat@free] <- 0
ip.mat@values[3,] <- 1

#  fmfit <- read.flexmirt("~/2012/sy/fm/ms-prm.txt")
#  ip.mat@values <- fmfit$G1$param

m.mat <- mxMatrix(name="mean", nrow=1, ncol=2, values=c(0,0), free=FALSE)
cov.mat <- mxMatrix("Symm", name="cov", nrow=2, ncol=2, values=diag(2),
                    free=c(FALSE,TRUE,FALSE), labels=c("v1","cov12","v2"))

m2 <- mxModel(model="2d",
              m.mat, cov.mat, ip.mat,
              mxData(observed=data, type="raw"),
              mxExpectationBA81(mean="mean", cov="cov",
                                ItemSpec=spec, ItemParam="ItemParam",
                                qpoints=21, qwidth=5, scores="full"),
              mxFitFunctionML(),
              mxComputeSequence(steps=list(
                mxComputeIterate(verbose=1L, steps=list(
                  mxComputeOnce('expectation', context='EM'),
                  #                  mxComputeGradientDescent(free.set='ItemParam', useGradient=TRUE),
                  mxComputeNewtonRaphson(free.set='ItemParam'),
                  mxComputeOnce('expectation'),
                  mxComputeOnce('fitfunction', free.set=c("mean", "cov"), maxAbsChange=TRUE)
                )),
               mxComputeOnce('fitfunction', free.set=c("mean", "cov"), fit=TRUE))))
  #  m2 <- mxOption(m2, "Number of Threads", 1)
m2.est <- mxRun(m2)
print(m2.est@fitfunction@result)
print(m2.est@matrices$ItemParam@values)
print(m2.est@matrices$cov@values)

latentVars <- c("interest", "experience")
grp <- list(spec=spec, param=m2.est@matrices$ItemParam@values,
            mean=m2.est@matrices$mean@values, cov=m2.est@matrices$cov@values)
# design needed for two-tier TODO
colnames(grp$mean) <- latentVars
dimnames(grp$cov) <- list(latentVars, latentVars)

thetaComb <- function(theta, nfact)
{
  if (nfact == 1L){
    Theta <- matrix(theta)
  } else {
    thetalist <- vector('list', nfact)
    for(i in 1L:nfact)
      thetalist[[i]] <- theta
    Theta <- as.matrix(expand.grid(thetalist))
  }	
  return(Theta)
}
chen.thissen.1997 <- function(grp, data, qwidth=6, qpoints=49) {
  theta <- thetaComb(seq(-qwidth,qwidth,length.out=qpoints), length(grp$mean))
  prior <- mvtnorm::dmvnorm(theta, grp$mean, grp$cov)
  prior <- prior/sum(prior)
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
