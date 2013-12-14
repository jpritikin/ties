library(rpf)
library(OpenMx)
source('irtplot.R')

load("espt.rda")

check.item.names <- function(item.names) {  
  missing <- item.names[is.na(match(item.names, colnames(espt)))]
  if (length(missing)) stop(paste("Columns missing:", missing))
  item.names
}

create.spec <- function(item.names, data) {
  spec <- list()
  for (n in 1:length(item.names)) {
    lev <- length(levels(data[[ item.names[n] ]]))
    spec[n] <- rpf.grm(outcomes=lev)
  }
  names(spec) <- item.names
  spec
}

set.starting <- function(ispec, ip.mat) {
  colnames(ip.mat@free) <- names(ispec)
  colnames(ip.mat@values) <- names(ispec)
  colnames(ip.mat@labels) <- names(ispec)
  for (ix in 1:length(ispec)) {
    i <- rpf.paramInfo(ispec[[ix]])
    ptype <- unlist(i)[seq(match('type', rownames(i)),length(i),length(rownames(i)))]
    ip.mat@values[which(ptype==1), ix] <- 1
    ip.mat@values[which(ptype==2), ix] <- seq(1,-1,length.out=sum(ptype==2))
    ip.mat@free[which(ptype==2), ix] <- TRUE
  }
  ip.mat
}

is.true <- function(v) v & !is.na(v)


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

# msFreq vs freqCause ?
inames <- check.item.names(c('wantLearn', "msIdAfraid",
                             "msFastEffortLife", "msDescarte"))
enames <- check.item.names(c('msNotion', "msAny", 'skipInt', "maxDuration",
                             "msYearn", "msMet", "msEnv", "msShared",
                             "msCause", "msAllow", "msTeach","msTrainTeach"))

when <- strptime(espt[['instrument']], "%Y-%m-%d")
revision1 <- strptime("2013-02-11", "%Y-%m-%d")
ver.mask <- difftime(when, revision1) > 0
manocha2013.mask <- (espt$time == 2 | (espt$time == 1 & !(espt$id %in% espt[espt$time == 2, 'id'])))
ver.mask[espt$wave == "manocha2013"] <- manocha2013.mask[espt$wave == "manocha2013"]

idata <- espt[ver.mask, inames]
sapply(idata, function(r) sum(!is.na(r)))  # per item sample size

edata <- espt[ver.mask, enames]
sapply(edata, function(r) sum(!is.na(r)))  # per item sample size

ispec <- create.spec(inames, idata)
espec <- create.spec(enames, edata)

ip.mat <- mxMatrix(name="ItemParam", nrow=max(sapply(ispec, rpf.numParam)),
                   ncol=length(ispec), free=FALSE)
ip.mat <- set.starting(ispec, ip.mat)
#ip.mat@labels[1,] <- "slope"
ip.mat@free[1,] <- TRUE
ip.mat@values[!ip.mat@free] <- 0

latents <- 1
m.mat <- mxMatrix(name="mean", nrow=1, ncol=latents, values=rep(0, latents), free=FALSE)
cov.mat <- mxMatrix("Symm", name="cov", nrow=latents, ncol=latents, values=diag(latents),
                    free=FALSE)

im <- mxModel(model="interest",
              m.mat, cov.mat, ip.mat,
              mxData(observed=idata, type="raw"),
              mxExpectationBA81(mean="mean", cov="cov",
                                ItemSpec=ispec, ItemParam="ItemParam",
                                scores="full"),
              mxFitFunctionML(),
              mxComputeEM('expectation',
                          mxComputeNewtonRaphson(free.set='ItemParam'),
                          mxComputeOnce('fitfunction', free.set=c("mean", "cov"), fit=TRUE),
                          verbose=2L))

im.est <- mxRun(im)
print(im.est@matrices$ItemParam@values[1,])

igrp <- list(spec=ispec,
             param=im.est@matrices$ItemParam@values,
             mean=im.est@matrices$mean@values,
             cov=im.est@matrices$cov@values,
             scores=im.est@expectation@scores.out,
             data=idata)

got <- chen.thissen.1997(igrp)
got$pval

#source("~/ifa/rpf/R/diagnose.R")
#options(warn=2)
#options(error = utils::recover)
rpf.1dim.fit(group=igrp, margin=2, wh.exact=FALSE)

booklet(function(item) {
  rpf.plot(igrp, item, width=4, data.bins=12, basis=1, factor=1)
}, inames, output="interest.pdf")

#ggplot(item.map(igrp), aes(x=score, y=item.name, label=outcome)) + geom_text()

plot.info(igrp, width=6, show.total=FALSE)

if (0) {
  booklet(function(item) {
    rpf.plot(grp, item, width=4, data.bins=12, basis=c(0,1), factor=2)
  }, item.names[grp$param[2,] > 0], output="experience.pdf")
}

if (0) {
  map2 <- item.map(grp, 2)
  ggplot(map2, aes(x=score, y=item.name, label=outcome)) + geom_text()
}
