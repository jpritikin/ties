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

raw1 <- read.csv("data-anon.csv", stringsAsFactors=FALSE)

SexItem <- c("Male","Female")

RelaItem = c('Single',
             'In a long-term relationship (i.e. together more than a year)',
             'Other')
RelaItemShort = c('Single',
                  'Together',
                  'Other')

cdat <- data.frame(
  start=raw1$StartDate,
  end=raw1$EndDate,
  born=raw1[[3]],
  sex=factor(raw1[[4]], levels=SexItem, labels=tolower(SexItem)),
  rel=factor(raw1[[5]], levels=RelaItem, labels=tolower(RelaItemShort)))

# SF12 (ignore for now) 6:17

got <- score.ipipBig5(raw1[18:67])
for (n in names(got)) cdat[[n]] <- got[[n]]
if (0) {
  cor(cdat[6:10], use="pairwise.complete.obs")
  cov(cdat[6:10], use="pairwise.complete.obs")
}

got <- score.panas(raw1[68:87])
for (n in names(got)) cdat[[n]] <- got[[n]]

cdat$maas <- score.maas(raw1[88:102])

got <- score.ryff9(raw1[103:156])
for (n in names(got)) cdat[[n]] <- got[[n]]
if (0) {
  cor(cdat[14:19], use="pairwise.complete.obs")
  cov(cdat[14:19], use="pairwise.complete.obs")
}

got <- score.mcFormC(raw1[160:172])
cdat$socialDesirable <- got - mean(got, na.rm=TRUE)

got <- score.dass(raw1[173:(173+21-1)])
for (n in names(got)) cdat[[n]] <- got[[n]]

cdat$mwb <- score.wemwbs(raw1[194:200])

cdat$ei <- score.ei(raw1[201:233])

got <- score.rrq(raw1[234:(234+24-1)])
for (n in names(got)) cdat[[n]] <- got[[n]]

cdat <- cbind(cdat, cms.score(prep.cms201312(raw1[258:283])))

cdat$psqi <- score.psqi(raw1[284:(284+19-1)])

if (0) {
  germano2014.cms <- cbind(prep.cms201312(raw2[67:92]), uid=raw2$uid)
  germano2014.cms$wave <- "germano2014"
  save(germano2014.cms, file="germano2014-cms.rda")
}

if (0) {
  #cat(deparse(colnames(wave2)))
  pairs(cdat[c(6:12,29:31)], cex=.5)
  pairs(cdat[c(13:18,29:31)], cex=.5)
  pairs(cdat[c(19:24,29:31)], cex=.5)
  pairs(cdat[c(25:28,32,29:31)], cex=.5)
  
  plot(cdat[,c("barrier", "event")])
  plot(cdat[,c("training", "event")])
  plot(cdat$training - cdat$barrier, cdat[,c("event")])
  cor(cdat$training - cdat$barrier, cdat[,c("event")], use="pairwise.complete.obs") # .72 for wave2
}

write.table(cdat, file="prep.csv", row.names=FALSE)
