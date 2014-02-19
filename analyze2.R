library(stringr)
library(ggplot2)
library(rpf)
library(reshape2)
library(plyr)
library(gtools)
source("measures.R")

# collect the calibration sample

source("prepare.R")
raw <- read.csv("raw.csv", stringsAsFactors=FALSE)
espt <- prepare.espt(raw)

when <- strptime(espt[['instrument']], "%Y-%m-%d")
revision1 <- strptime("2013-02-11", "%Y-%m-%d")
ver.mask <- difftime(when, revision1) > 0
espt <- espt[ver.mask,]

manocha2013 <- read.csv("au/2013combined.csv", stringsAsFactors=FALSE)
manocha2013$wave <- 'manocha2013'
manocha2013.cms <- cbind(prep.cms201309(manocha2013[,79:101]), uid=manocha2013$uid)
espt <- smartbind(espt, manocha2013.cms)

espt$freqCause <- NULL  # response options changed 2013-12
load("germano2014/germano2014-cms.rda")
espt <- smartbind(espt, germano2014.cms)

if (length(unique(espt$uid[!is.na(espt$uid)])) != sum(!is.na(espt$uid))) stop("mismatch")
next.uid <- 1+max(espt$uid, na.rm=TRUE)
espt$uid[is.na(espt$uid)] <- seq(next.uid, next.uid+sum(is.na(espt$uid)) - 1)
save(espt, file="espt.rda")

#source('irtplot.R')

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
# compare sample distributions
if (0) {
  hist.plot <- ggplot(espt, aes(x=score)) + geom_histogram() + facet_grid(ppool ~ .)  
}

# nothing obvious here
if (0) {
  pairs(~score+born+sex+edu+rel, data=espt)  
}

