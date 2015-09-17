library(stringr)
library(ggplot2)
library(rpf)
library(reshape2)
library(plyr)
library(gtools)
source("measures.R")

#options(error=browser)
# collect the calibration sample

source("prepare.R")
raw <- read.csv("raw.csv", stringsAsFactors=FALSE)
raw <- stripPeriods(raw)
espt <- prepare.espt(raw)

when <- strptime(espt[['instrument']], "%Y-%m-%d")
revision1 <- strptime("2013-02-11", "%Y-%m-%d")
ver.mask <- difftime(when, revision1) > 0
espt <- espt[ver.mask,]
espt <- espt[,apply(espt, 2, function(c) !all(is.na(c) | c==''))]
espt <- cms.fixOldData(espt)

espt$population[grepl("ppool",espt$wave)] <- 'uva'
espt$population[grepl("grad",espt$wave)] <- 'uva'

manocha2013 <- read.csv("au/2013combined.csv", stringsAsFactors=FALSE)
if (TRUE) {
  manocha2013.mask <- (manocha2013$time == 2 | (manocha2013$time == 1 & !(manocha2013$id %in% manocha2013[manocha2013$time == 2, 'id'])))
  manocha2013 <- manocha2013[manocha2013.mask,]
  if (any(table(manocha2013$id) != 1)) stop("More than 1 measurement from a single participant")
}
manocha2013.cms <- cbind(prep.cms201309(manocha2013[,79:101]), uid=manocha2013$uid)
manocha2013.cms$start <- '12/15/2013';
manocha2013.cms$end <- '12/15/2013';
manocha2013.cms$wave <- 'manocha2013'
manocha2013.cms$population <- 'general'
espt <- smartbind(espt, manocha2013.cms)

espt$freqCause <- NULL  # response options changed 2013-12
# ----------------------------------- add new data after here ---------------

espt <- smartbind(espt, local({
  wd <- setwd("germano2014")
  source("getCalibrationData.R")
  setwd(wd)
  germano2014.cms
}))

espt <- smartbind(espt, local({
  wd <- setwd("cor")
  source("getCalibrationData.R")
  setwd(wd)
  cor.ppool.cms
}))

web201408 <- read.csv("earlydata/short-20140827.csv", stringsAsFactors=FALSE)
web201408Prep <- cbind(prepDemographics(web201408[1:16]),
                       prep.cms201312(web201408[17:(17+26-1)]))
web201408Prep$wave <- "earlydata/short-20140827"
espt <- smartbind(espt, web201408Prep)

web201409 <- read.csv("earlydata/short-20140915.csv", stringsAsFactors=FALSE)
web201409Prep <- cbind(prepDemographics(web201409[1:16]),
                       prep.cms201409(web201409[17:(17+29-1)]))
web201409Prep$wave <- "earlydata/short-20140915"
espt <- smartbind(espt, web201409Prep)

web201410 <- read.csv("earlydata/short-20141006.csv", stringsAsFactors=FALSE)
web201410[[33]] <- NULL  # new item, no data
web201410Prep <- cbind(prepDemographics(web201410[1:16]),
                       prep.cms201409(web201410[17:(17+29-1)]))
web201410Prep$wave <- "earlydata/short-20141006"
espt <- smartbind(espt, web201410Prep)

espt <- smartbind(espt, local({
  wd <- setwd("germano2015")
  source("getCalibrationData.R")
  setwd(wd)
  germano2015.cms
}))

web201410 <- read.csv("earlydata/short-20141022.csv", stringsAsFactors=FALSE)
web201410Prep <- cbind(prepDemographics(web201410[1:16]),
                       prep.cms201410(web201410[17:(17+29-1)]))
web201410Prep$wave <- "earlydata/short-20141022"
espt <- smartbind(espt, web201410Prep)

web201509 <- read.csv("earlydata/web-201509p.csv", stringsAsFactors=FALSE)
web201509Prep <- cbind(prepDemographics(web201509[1:16]),
                       prep.cms201508(web201509[17:(17+34-1)]))
web201509Prep$wave <- "earlydata/short-201509"
espt <- smartbind(espt, web201509Prep)

espt$population[is.na(espt$population)] <- 'general'

if (length(unique(espt$uid[!is.na(espt$uid)])) != sum(!is.na(espt$uid))) stop("mismatch")
next.uid <- 1+max(espt$uid, na.rm=TRUE)
espt$uid[is.na(espt$uid)] <- seq(next.uid, next.uid+sum(is.na(espt$uid)) - 1)

# regularize to easily sortable format
century <- rep(TRUE, length(espt$start))
century[grep("\\d{4}", espt[["start"]], perl=TRUE, invert=TRUE)] <- FALSE
for (col in c("start", "end")) {
  tmp1 <- strptime(espt[[col]][century], "%m/%d/%Y")
  tmp2 <- strptime(espt[[col]][!century], "%m/%d/%y")
  espt[[col]][century] <- strftime(tmp1, "%Y/%m/%d")
  espt[[col]][!century] <- strftime(tmp2, "%Y/%m/%d")
}

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
