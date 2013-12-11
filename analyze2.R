library(stringr)
library(ggplot2)
library(rpf)
library(reshape2)
library(plyr)
library(gridExtra)
library(gtools)
source("jrs.R")
options(jrsCacheDir='.cache')
options(error = utils::recover)

source("prepare.R")
raw <- read.csv("raw.csv", stringsAsFactors=FALSE)
manocha2013 <- read.csv("au/2013combined.csv", stringsAsFactors=FALSE)
manocha2013$wave <- 'manocha2013'
manocha2013$instrument <- '2013-09-12'
combined <- smartbind(raw, manocha2013, fill="")
espt <- prepare.espt(combined)
save(espt, file="espt.rda")

source('irtplot.R')

#######################################################
# demographics

# do participants accurately self report their location? looks good:
# espt[(tolower(as.character(espt$ip.country)) != tolower(espt$country)),c('ip.country', 'country')]

#table(espt$wave, grepl("\\bstudent\\b", espt$work, ignore.case=TRUE))
if(0) {
  table(espt$wave)
  table(espt$wave, espt$rel)
  table(espt$edu)
  table(espt$wave, espt$m.training)
  
  table(espt[espt$ppool=='Web Surfers','edu'])
  sort(table(espt[espt$ppool=='Web Surfers','ip.country']))
}

########################################################
# Check for crazy stuff. The vast majority of the data looks reasonable.
# No need to exclude anything.
# WARNING: THIS CODE IS STALE. Some items are reversed scored now. DANGER

if (0) {
dis.logic <- cbind(
  msOpposite1=unclass(espt$msEvery) + unclass(espt$msNotAny)-6 < -2,
  msOpposite2=unclass(espt$msNotSelf) + unclass(espt$msCause)-6 < -2,
  msCrazy1=pmax(unclass(espt$msTeach) - unclass(espt$msTrainTeach),0)>1,
  boreCrazy=espt$boreFidget=='No' & espt$boreCheer=='Yes' & espt$boreLone=='False',
  msCrazy2=((espt$wantLearn=='Not sure' | espt$wantLearn=='No') &
    (espt$freqCause=='Daily' | espt$freqCause=='Weekly' | espt$freqCause=='Infrequently')),
  msCrazy3=xor(espt$maxDuration=='I have not experienced complete mental silence',
      espt$durationCharacter=='I have not experienced complete mental silence')
  )
table(apply(dis.logic, 1, sum, na.rm=TRUE))

# msCause -- It is not clear who is causing mental silence.
# table(pmax(unclass(espt$msCause) - unclass(espt$msTeach),0))

dis.flow <- cbind(
  pmax(4-unclass(espt$fl.b.pf),0),
  pmax(unclass(espt$fl.subjTime)-3,0),
  pmax(3-unclass(espt$fl.b.gi),0))
table(apply((dis.flow), 1, sum, na.rm=TRUE))
}

################################################################

m2.item.names <- c('msNotion','msFreq', 'msAny', 'msEvery', 'msCause0', 'wantLearn',
                   'msAfraid', 'msEmo', 'msLife', 'msFast', 'msDescarte', 'msIdentity',
                   'freqCause', 'maxDuration', 'msYearn', 'msMet', 'msEnv', 'msCause', 'msAllow',
                   'msShared',  'msTeach', 'msTrainTeach')

gpcm <- function(outcomes) {
  rpf.nrm(outcomes, T.c=lower.tri(diag(outcomes-1),TRUE) * -1)
#   rpf.nrm(outcomes, T.c=diag(outcomes-1))
}

m2.spec <- list()
m2.spec[1:22] <- gpcm(5)
m2.spec[2] <- gpcm(4)
m2.spec[5] <- gpcm(3)
m2.spec[6] <- gpcm(4)
m2.spec[13:14] <- gpcm(4)

#sapply(m2.spec, function(m) slot(m,'numOutcomes'))

m2.missing <- m2.item.names[is.na(match(m2.item.names, colnames(espt)))]
if (length(m2.missing)) stop(paste("Columns missing:", m2.missing))
m2.data <- espt[,m2.item.names]
m2.mask <- apply(m2.data, 1, function (r) !all(is.na(r)))
m2.data <- m2.data[m2.mask,]
for (c in colnames(m2.data)) { attr(m2.data[,c], 'mxFactor') <- attr(espt[,c], 'mxFactor') }

if (0) {
  g.wave1 <- espt[,'wave'] == 'germano2013-1'
  g.wave2 <- espt[,'wave'] == 'germano2013-2'
  g.wave3 <- espt[,'wave'] == 'germano2013-3'
  other <- !(g.wave1 | g.wave2 | g.wave3)
  col.mask <- apply(!is.na(m2.data[g.wave1[m2.mask],]), 2, any)
  which(apply(!is.na(m2.data[g.wave1[m2.mask],]), 2, any))
  
  m2.fm <- sapply(m2.data, unclass) - 1
  m2.fm[is.na(m2.fm)] <- -9
  write.table(m2.fm[g.wave1[m2.mask],col.mask], file="fm-ms-g1.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  write.table(m2.fm[g.wave2[m2.mask],col.mask], file="fm-ms-g2.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  write.table(m2.fm[g.wave3[m2.mask],col.mask], file="fm-ms-g3.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  write.table(m2.fm[other[m2.mask],col.mask], file="fm-ms-main.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  
#  write.table(m2.fm, file="ms-data.csv", row.names=FALSE, col.names=FALSE, quote=FALSE)
  
}

m2.estimate <- function () {
  m2.numItems <- length(m2.item.names)
  m2.maxParam <-max(sapply(m2.spec, rpf.numParam))
  
  ip.mat <- mxMatrix(name="ItemParam", nrow=m2.maxParam, ncol=m2.numItems,
                     values=c(1, 1, rep(0, m2.maxParam-2)), free=FALSE)
  ip.mat@labels[1,] <- 'a1'
  ip.mat@free[1,] <- TRUE
  for (ix in 1:m2.numItems) {
    thr <- m2.spec[[ix]]@outcomes - 1
    ip.mat@free[(2+thr):(1+2*thr), ix] <- TRUE
  }
  
  #  m2.fmfit <- read.flexmirt("~/2012/sy/fm/ms-prm.txt")
  #  ip.mat@values <- m2.fmfit$G1$param
  
  m.mat <- mxMatrix(name="mean", nrow=1, ncol=1, values=0, free=FALSE)
  cov.mat <- mxMatrix(name="cov", nrow=1, ncol=1, values=1, free=FALSE)
  
  m2 <- mxModel(model="m2", m.mat, cov.mat, ip.mat,
                mxData(observed=m2.data, type="raw"),
                mxExpectationBA81(mean="mean", cov="cov",
                                  ItemSpec=m2.spec,
                                  ItemParam="ItemParam", scores="full"),
                mxFitFunctionML(),
                mxComputeIterate(steps=list(
                  mxComputeOnce('expectation', context='EM'),
                  #                  mxComputeGradientDescent(free.set='ItemParam', useGradient=TRUE),
                  mxComputeNewtonRaphson(free.set='ItemParam'),
                  mxComputeOnce('expectation'),
                  mxComputeOnce('fitfunction', adjustStart=TRUE, free.set=c("mean", "cov"))
                )))
  #  m2 <- mxOption(m2, "Number of Threads", 1)
  m2 <- mxRun(m2)
}
m2 <- cache(m2.estimate, 'm2')

m2.items <- t(m2@matrices$ItemParam@values)
rownames(m2.items) <- m2.item.names
espt[m2.mask, "m2.score"] <- m2@expectation@scores.out[,1]
espt[m2.mask, "m2.se"] <- m2@expectation@scores.out[,2]

if (0) {
  got <- list()
  for (item in m2.item.names) {
    for (l in levels(espt[[item]])) {
      got[[paste(item, l, sep="=")]] <- mean(espt[espt[[item]] == l, 'm2.score'], na.rm=TRUE)
    }
  }
  do.call(rbind, got)
}

if (0) {
  dm.page <- function (name) {
    item.x <- match(name, m2.item.names)
    param <- m2@matrices$ItemParam@values[,item.x]
    data.vs.model(m2.spec[[item.x]], param, m2.data, m2@expectation@scores.out[,1], name, plot.rug=TRUE) +
      labs(x = "familiarity",
           title = paste0(item.x,": ",name, ", slope = ", round(param[1],2)))
  }
  data.vs.model.booklet(dm.page, m2.item.names)
}

if (0) {
  ddply(espt, ~wave, function (slice) c(n=sum(!is.na(slice$m2.score)),
                                          m=mean(slice$m2.score, na.rm=TRUE),
                                        sd=sd(slice$m2.score, na.rm=TRUE)))
  
  plot.info(m2.spec, t(m2@matrices$ItemParam@values), m2.item.names,
            width=4, show.total=FALSE)
}

# compare two items
if (0) {
  item.cor <- function(i1, i2) {
    filter <- apply(!is.na(cbind(espt[[i1]], espt[[i2]])), 1, all)
    cor(unclass(espt[filter,i1]), unclass(espt[filter,i2]))
  }
  item.loc <- function(i1) {
    i1.x <- match(i1, rownames(m2.items))
    optimize(function (w) -rpf.info(m2.spec[[i1.x]], m2.items[i1.x,], w), interval=c(-5,5))
  }
  plot.info(m2.spec[10], t(m2.items[10,]))
}
if (0) rpf.mean.info(m2.spec, m2.items)

################################################################
if (0) {
  i1 <- rpf.gpcm(5)
  spec <- list()
  spec[1:10] <- i1
  items <- read.csv("sit21c/items.csv", stringsAsFactors=FALSE)
  items <- prepare.items(items)
  
  ms.scale.items <- items[,'name']
  ms.scale <- espt[,ms.scale.items]
  #table(apply(is.na(ms.scale), 1, sum))
  score.mask <- apply(is.na(ms.scale), 1, sum) == 0
  ms.scale <- ms.scale[score.mask,]
}

# compare sample distributions
if (0) {
  hist.plot <- ggplot(espt, aes(x=score)) + geom_histogram() + facet_grid(ppool ~ .)  
}

# nothing obvious here
if (0) {
  pairs(~score+born+sex+edu+rel, data=espt)  
}

##############################################
# data vs model ICC plots
# TODO add to vignette
if (0) {
  items <- read.csv("sit21c/items.csv", stringsAsFactors=FALSE)
  items <- prepare.items(items)
  items$name <- ms.scale.items
}

if (0) {
  print.style <- theme_bw(base_size=16)
  png("summaryplot.png", width=10, height=3.5, units="in", res=150)
  grid.arrange(plot.info() + print.style, hist.plot + print.style, ncol=2)
  dev.off()
}

data.vs.model.plot <- function(id) {
  item.x <- match(id, items$id)
  param <- items[item.x, c('slope',paste0('b',1:4))]
  data.vs.model(spec[[item.x]], param, espt, items[item.x,'name']) +
    labs(title = paste0(id, ", slope = ",param[1]))
}
#data.vs.model.plot('SHAR')

if (0) {
  data.vs.model.booklet(function (ix) data.vs.model.plot(ix), items$id)  
}

if (0) {
  print.style <- theme_bw(base_size=16)
  png("shar-d-v-m.png", width=10, height=3.5, units="in", res=150)
  data.vs.model.plot('SHAR') + print.style
  dev.off()
}
if (0) {
  print.style <- theme_bw(base_size=16)
  png("pay-d-v-m.png", width=10, height=3.5, units="in", res=150)
  data.vs.model.plot('PAY') + print.style
  dev.off()
}

if (0) {
  ms.people <- espt[,c('wave', ms.scale.items, 'score','se')]
  ms.items <- items
  save(ms.people, file='ms.people.Rda', compress=FALSE)
  save(ms.items, file='ms.items.Rda', compress=FALSE)
}

######################################################
if (0) {
  scores <- espt$m2.score
  responses <- espt[,m2.item.names]
  
  fit <- rpf.1dim.fit(m2.spec, t(m2.items), responses, scores, 2, wh.exact=FALSE)
  fit[order(-fit$infit),]
  #fit[order(-fit$outfit),]
  #mean(fit$infit)
  #mean(fit$outfit)
  write.csv(t(fit), "item-infit.csv")  
  
  fit <- rpf.1dim.fit(m2.spec, t(m2.items), responses, scores, 1, wh.exact=FALSE)
  resid <- rpf.1dim.stdresidual(m2.spec, t(m2.items), responses, scores)
  
  report <- cbind(scores, fit$infit, fit$outfit, espt[,'wave'], espt[,m2.item.names], resid)
  write.csv(report, "person-infit.csv")  
  
  for (col in c('infit','infit.z','outfit','outfit.z')) {
    espt[[col]] <- fit[[col]]
  }
}
