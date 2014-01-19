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
# compare sample distributions
if (0) {
  hist.plot <- ggplot(espt, aes(x=score)) + geom_histogram() + facet_grid(ppool ~ .)  
}

# nothing obvious here
if (0) {
  pairs(~score+born+sex+edu+rel, data=espt)  
}

