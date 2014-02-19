library(ggplot2)
library(digest)
{
  wd <- setwd("..")
  source("measures.R")
  source("cms-score.R")
  setwd(wd)
}

raw1 <- read.csv("wave1-anon.csv", stringsAsFactors=FALSE)

SexItem <- c("Male","Female")

RelaItem = c('Single',
             'In a long-term relationship (i.e. together more than a year)',
             'Other')
RelaItemShort = c('Single',
                  'Together',
                  'Other')

wave1 <- data.frame(
  id=raw1$id,
  uid=raw1$uid,
  born=raw1[[3]],
  sex=factor(raw1[[4]], levels=SexItem, labels=tolower(SexItem)),
  rel=factor(raw1[[5]], levels=RelaItem, labels=tolower(RelaItemShort)))

got <- score.rrq(raw1[6:(6+24-1)])
wave1$reflection <- got$reflection
wave1$rumination <- got$rumination

wave1$psqi <- score.psqi(raw1[30:(30+19-1)])

got <- score.dass(raw1[49:(49+21-1)])
wave1$dass.d <- got$d
wave1$dass.a <- got$a
wave1$dass.s <- got$s
wave1$dass.na <- got$na

germano2014.cms <- cbind(prep.cms201312(raw1[70:95]), uid=raw1$uid)
germano2014.cms$wave <- "germano2014"
if (0) {
  save(germano2014.cms, file="germano2014-cms.rda")
}

wave1 <- cbind(wave1, cms.score(germano2014.cms))
apply(wave1, 2, function (c) sum(is.na(c)))
