library(testthat)
library(digest)
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
if (0) {
  apply(wave1, 2, function (c) sum(is.na(c)))
}

verify.col <- c("reflection", "rumination", "psqi", "dass.d", "dass.a", "dass.s",  "dass.na")
verify.digest <- c("5c048a9bde4fe7d45a2feea10894cdb4", "8e01999b6cedb96e28c7141e2237610e",
                   "40200e2e00b9ffbf1bf00a00afa92d9e", "c762151cc430e67eecd463e32088aa13",
                   "031af9ac8379ef50e25b583f8b3097bf", "5fca0690cad3c7c67ee36780703e227f",
                   "40ceca7f66dbb3dfbafa18db5e6c25fe")
for (c in 1:length(verify.col)) {
  expect_equal(digest(wave1[[verify.col[[c]]]]), verify.digest[[c]], info=verify.col[[c]])
}
