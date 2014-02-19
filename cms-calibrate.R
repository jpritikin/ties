library(rpf)
library(OpenMx)
source('irtplot.R')
source("cms-score-lib.R")

load("espt.rda")
espt <- cms.testlets(espt)

barrier.items <- c('msIdAfraidLearn', 'msFastEffortLife')
training.items <- c("yearnEnv", "allowCause", "trainSkill", "trainNum")
event.items <- c("msAny", "msEvery", "freqCause", "successCat", "maxDuration")

all.items <- c(barrier.items, training.items, event.items)

# ensure that we have the data we think we have
missing <- all.items[is.na(match(all.items, colnames(espt)))]
if (length(missing)) stop(paste("Columns missing:", missing))

sapply(espt[all.items], function (c) sum(!is.na(c)))  # approx per item sample size

if (0) {
  # exclude later when we have more data TODO
  manocha2013.mask <- (espt$time == 2 | (espt$time == 1 & !(espt$id %in% espt[espt$time == 2, 'id'])))
  ver.mask[espt$wave == "manocha2013"] <- manocha2013.mask[espt$wave == "manocha2013"]
  data <- espt[ver.mask, item.names]
}

plot.width=3

#-------------------------------------------------------------------------

spec <- mkspec(espt, barrier.items)
numItems <- length(barrier.items)
maxParam <- max(sapply(spec, rpf.numParam))

ip.mat <- mxMatrix(name="ItemParam", nrow=maxParam, ncol=numItems, free=FALSE)
colnames(ip.mat@free) <- barrier.items
colnames(ip.mat@values) <- barrier.items

ip.mat <- set.nominal.rank(spec, ip.mat, 'msIdAfraid', .27, .5)
ip.mat <- set.nominal.rank(spec, ip.mat, 'msIdAfraidLearn', .27, .5)
ip.mat <- set.nominal.rank(spec, ip.mat, 'msFastEffort', .27, .5)
ip.mat <- set.nominal.rank(spec, ip.mat, 'msFastEffortLife', .27, .5)
ip.mat <- set.nominal.rank(spec, ip.mat, 'msDescarte', .5, .5)

m.mat <- mxMatrix(name="mean", nrow=1, ncol=1, values=0)
cov.mat <- mxMatrix("Symm", name="cov", nrow=1, ncol=1, values=diag(1))

ba.data <- espt[,barrier.items]
ba.mask <- apply(ba.data[,barrier.items], 1, function(r) any(!is.na(r)))

ba <- mxModel(model="barrier",
              m.mat, cov.mat, ip.mat,
              mxData(observed=ba.data[ba.mask,], type="raw"),
              mxExpectationBA81(mean="mean", cov="cov",
                                ItemSpec=spec, ItemParam="ItemParam", scores="full"),
              mxFitFunctionML(),
              mxComputeSequence(steps=list(
                mxComputeEM('expectation',
                            mxComputeNewtonRaphson(free.set='ItemParam'),
                            mxComputeOnce('fitfunction', free.set=c("mean", "cov"), fit=TRUE)),
                mxComputeOnce('expectation'),
                mxComputeOnce('fitfunction', information=TRUE, info.method="meat"),
                mxComputeStandardError(),
                mxComputeHessianQuality())
                ))
ba.est <- mxRun(ba, silent=TRUE)
ba.est@output$conditionNumber

ba.grp <- list(spec=spec,
            param=ba.est@matrices$ItemParam@values,
            free=apply(ip.mat@free, 2, sum),
            mean=ba.est@matrices$mean@values,
            cov=ba.est@matrices$cov@values,
            scores=ba.est@expectation@output$scores,
            data=ba.data[ba.mask,])

if (0) {
  got <- chen.thissen.1997(ba.grp)
  max(got$pval[!is.na(got$pval)])  # -2.14
}

booklet(function(item) {
  rpf.plot(ba.grp, item, width=plot.width, data.bins=12, basis=c(1), factor=1)
}, barrier.items, output="barrier.pdf")

#-------------------------------------------------------------------------

spec <- mkspec(espt, training.items)
numItems <- length(training.items)
maxParam <- max(sapply(spec, rpf.numParam))

ip.mat <- mxMatrix(name="ItemParam", nrow=maxParam, ncol=numItems, free=FALSE)
colnames(ip.mat@free) <- training.items
colnames(ip.mat@values) <- training.items

ip.mat <- set.nominal.rank(spec, ip.mat, 'msNotion', .51, 1)
ip.mat <- set.nominal.rank(spec, ip.mat, 'msYearn', .51, 1)
ip.mat <- set.nominal.rank(spec, ip.mat, 'msEnv', .51, 1)
ip.mat <- set.nominal.rank(spec, ip.mat, 'yearnEnv', .27, .27)
ip.mat <- set.nominal.rank(spec, ip.mat, 'msAllow', .51, 1)
ip.mat <- set.nominal.rank(spec, ip.mat, 'msCause', .51, 1)
ip.mat <- set.nominal.rank(spec, ip.mat, 'allowCause', .27, .27)
ip.mat <- set.nominal.rank(spec, ip.mat, 'trainSkill', .2, .2)
ip.mat <- set.nominal.rank(spec, ip.mat, 'trainNum', .1, .1)

tr.data <- espt[,training.items]
tr.mask <- apply(tr.data[,training.items], 1, function(r) any(!is.na(r)))

train <- mxModel(model="training",
              m.mat, cov.mat, ip.mat,
              mxData(observed=tr.data[tr.mask,], type="raw"),
              mxExpectationBA81(mean="mean", cov="cov",
                                ItemSpec=spec, ItemParam="ItemParam", scores="full"),
              mxFitFunctionML(),
              mxComputeSequence(steps=list(
                mxComputeEM('expectation',
                            mxComputeNewtonRaphson(free.set='ItemParam'),
                            mxComputeOnce('fitfunction', free.set=c("mean", "cov"), fit=TRUE)),
                mxComputeOnce('expectation'),
                mxComputeOnce('fitfunction', information=TRUE, info.method="meat"),
                mxComputeStandardError(),
                mxComputeHessianQuality())
              ))
tr.est <- mxRun(train, silent=TRUE)
tr.est@output$conditionNumber

tr.grp <- list(spec=spec,
            param=tr.est@matrices$ItemParam@values,
            free=apply(ip.mat@free, 2, sum),
            mean=tr.est@matrices$mean@values,
            cov=tr.est@matrices$cov@values,
            scores=tr.est@expectation@output$scores,
            data=tr.data[tr.mask,])

if (0) {
  got <- chen.thissen.1997(tr.grp)
  max(got$pval[!is.na(got$pval)])  # 35
}

booklet(function(item) {
  rpf.plot(tr.grp, item, width=plot.width, data.bins=12, basis=c(1), factor=1)
}, training.items, output="training.pdf")

#-------------------------------------------------------------------------

spec <- mkspec(espt, event.items)
numItems <- length(event.items)
maxParam <- max(sapply(spec, rpf.numParam))

ip.mat <- mxMatrix(name="ItemParam", nrow=maxParam, ncol=numItems, free=FALSE)
colnames(ip.mat@free) <- event.items
colnames(ip.mat@values) <- event.items

ip.mat <- set.nominal.rank(spec, ip.mat, 'msAny', .51, 1)
ip.mat <- set.nominal.rank(spec, ip.mat, 'msEvery', .51, 1)
ip.mat <- set.nominal.rank(spec, ip.mat, 'freqCause', .51, 1)
ip.mat <- set.nominal.rank(spec, ip.mat, 'successCat', .35, .5)
ip.mat <- set.nominal.rank(spec, ip.mat, 'maxDuration', .51, 1)

ev.data <- espt[,event.items]
ev.mask <- apply(ev.data[,event.items], 1, function(r) any(!is.na(r)))

ev <- mxModel(model="event",
                 m.mat, cov.mat, ip.mat,
                 mxData(observed=ev.data[ev.mask,], type="raw"),
                 mxExpectationBA81(mean="mean", cov="cov",
                                   ItemSpec=spec, ItemParam="ItemParam", scores="full"),
                 mxFitFunctionML(),
                 mxComputeSequence(steps=list(
                   mxComputeEM('expectation',
                               mxComputeNewtonRaphson(free.set='ItemParam'),
                               mxComputeOnce('fitfunction', free.set=c("mean", "cov"), fit=TRUE)),
                   mxComputeOnce('expectation'),
                   mxComputeOnce('fitfunction', information=TRUE, info.method="meat"),
                   mxComputeStandardError(),
                   mxComputeHessianQuality())
                 ))
ev.est <- mxRun(ev, silent=TRUE)
ev.est@output$conditionNumber

ev.grp <- list(spec=spec,
            param=ev.est@matrices$ItemParam@values,
            free=apply(ip.mat@free, 2, sum),
            mean=ev.est@matrices$mean@values,
            cov=ev.est@matrices$cov@values,
            scores=ev.est@expectation@output$scores,
            data=ev.data[ev.mask,])

booklet(function(item) {
  rpf.plot(ev.grp, item, width=plot.width, data.bins=12, basis=c(1), factor=1)
}, event.items, output="event.pdf")

if (0) {
  got <- chen.thissen.1997(ev.grp)
  max(got$pval[!is.na(got$pval)])  # 25.9
}

#-------------------------------------------------------------------------

save(ba.grp, tr.grp, ev.grp, file="cms-fit.rda")

if (0) {
  map1 <- item.map(grp, 1)
  ggplot(map1, aes(x=score, y=item.name, label=outcome)) + geom_text()
  map2 <- item.map(grp, 2)
  ggplot(map2, aes(x=score, y=item.name, label=outcome)) + geom_text()
}

if (0) {
  got1 <- chen.thissen.1997(grp)
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
