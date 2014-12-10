library(testthat)
library(digest)
library(ggplot2)
library(gtools)
{
  wd <- setwd("..")
  source("measures.R")
  source("cms-score.R")
  setwd(wd)
}

raw1 <- read.csv("data-201405.csv", stringsAsFactors=FALSE)

SexItem <- c("Male","Female")

RelaItem = c('Single',
             'In a long-term relationship (i.e. together more than a year)',
             'Other')
RelaItemShort = c('Single',
                  'Together',
                  'Other')

cdat1 <- data.frame(
  start=raw1$StartDate,
  end=raw1$EndDate,
  born=raw1[[3]],
  sex=factor(raw1[[4]], levels=SexItem, labels=tolower(SexItem)),
  rel=factor(raw1[[5]], levels=RelaItem, labels=tolower(RelaItemShort)))

# SF12 (ignore for now) 6:17

got <- score.ipipBig5(raw1[18:67])
for (n in names(got)) cdat1[[n]] <- got[[n]]
if (0) {
  cor(cdat1[6:10], use="pairwise.complete.obs")
  cov(cdat1[6:10], use="pairwise.complete.obs")
}

got <- score.panas(raw1[68:87])
for (n in names(got)) cdat1[[n]] <- got[[n]]

cdat1$actAware <- score.maas(raw1[88:102])

got <- score.ryff9(raw1[103:156])
for (n in names(got)) cdat1[[n]] <- got[[n]]
if (0) {
  cor(cdat1[14:19], use="pairwise.complete.obs")
  cov(cdat1[14:19], use="pairwise.complete.obs")
}

got <- score.mcFormC(raw1[160:172])
cdat1$socialDesirable <- got - mean(got, na.rm=TRUE)

got <- score.dass(raw1[173:(173+21-1)])
for (n in names(got)) cdat1[[n]] <- got[[n]]

cdat1$mwb <- score.wemwbs(raw1[194:200])

cdat1$ei <- score.ei(raw1[201:233])

got <- score.rrq(raw1[234:(234+24-1)])
for (n in names(got)) cdat1[[n]] <- got[[n]]

cdat1 <- cbind(cdat1, cms.score('uva', prep.cms201312(raw1[258:283])))

cdat1$sleep <- -score.psqi(raw1[284:(284+19-1)])

# ------------------------------------------------------------------
raw2 <- read.csv("data-201410.csv", stringsAsFactors=FALSE)

cdat2 <- data.frame(
  start=raw2$StartDate,
  end=raw2$EndDate,
  born=raw2[[3]],
  sex=factor(raw2[[4]], levels=SexItem, labels=tolower(SexItem)),
  rel=factor(raw2[[5]], levels=RelaItem, labels=tolower(RelaItemShort)))

# SF12 (ignore for now) 6:17

got <- score.ipipBig5(raw2[18:67])
for (n in names(got)) cdat2[[n]] <- got[[n]]

got <- score.panas(raw2[68:87])
for (n in names(got)) cdat2[[n]] <- got[[n]]

got <- score.5fMindfulness2(code.5fMindfulness2(raw2[88:126]))
for (n in names(got)) cdat2[[n]] <- got[[n]]

got <- score.ryff9(raw2[127:180])
for (n in names(got)) cdat2[[n]] <- got[[n]]

got <- score.mcFormC(raw2[184:196])
cdat2$socialDesirable <- got - mean(got, na.rm=TRUE)

got <- score.dass(raw2[197:(197+21-1)])
for (n in names(got)) cdat2[[n]] <- got[[n]]

cdat2$mwb <- score.wemwbs(raw2[218:(218+7-1)])

cdat2$ei <- score.ei(raw2[225:(225+33-1)])

got <- score.rrq(raw2[258:(258+24-1)])
for (n in names(got)) cdat2[[n]] <- got[[n]]

cdat2 <- cbind(cdat2, cms.score('uva', prep.cms201409(raw2[282:(282+29-1)])))

cdat2$sleep <- -score.psqi(raw2[311:(311+19-1)])

# ------------------------------------------------------------------
raw3 <- read.csv("data-201412.csv", stringsAsFactors=FALSE)

cdat3 <- data.frame(
  start=raw3$StartDate,
  end=raw3$EndDate,
  born=raw3[[3]],
  sex=factor(raw3[[4]], levels=SexItem, labels=tolower(SexItem)),
  rel=factor(raw3[[5]], levels=RelaItem, labels=tolower(RelaItemShort)))

# SF12 (ignore for now) 6:17

got <- score.ipipBig5(raw3[18:67])
for (n in names(got)) cdat3[[n]] <- got[[n]]

got <- score.panas(raw3[68:87])
for (n in names(got)) cdat3[[n]] <- got[[n]]

got <- score.5fMindfulness2(code.5fMindfulness2(raw3[88:126]))
for (n in names(got)) cdat3[[n]] <- got[[n]]

got <- score.ryff9(raw3[127:180])
for (n in names(got)) cdat3[[n]] <- got[[n]]

got <- score.mcFormC(raw3[184:196])
cdat3$socialDesirable <- got - mean(got, na.rm=TRUE)

got <- score.dass(raw3[197:(197+21-1)])
for (n in names(got)) cdat3[[n]] <- got[[n]]

cdat3$mwb <- score.wemwbs(raw3[218:(218+7-1)])

cdat3$ei <- score.ei(raw3[225:(225+33-1)])

got <- score.rrq(raw3[258:(258+24-1)])
for (n in names(got)) cdat3[[n]] <- got[[n]]

cdat3 <- cbind(cdat3, cms.score('uva', prep.cms201410(raw3[282:(282+29-1)])))

cdat3$sleep <- -score.psqi(raw3[311:(311+19-1)])

# --------------------------------------------------------

cdat <- smartbind(cdat1, cdat2)
cdat <- smartbind(cdat, cdat3)
cdat <- cdat[,c(1:13,33:36,32,14:31)] # move ffmq together and CMS to the end

if (0) {
  #cat(deparse(colnames(wave2)))
  pairs(cdat[c(6:12,29:31)], cex=.25)
  pairs(cdat[c(13:18,29:31)], cex=.25)
  pairs(cdat[c(19:24,29:31)], cex=.25)
  pairs(cdat[c(25:28,32,29:31)], cex=.25)
  
  plot(cdat[,c("barrier", "event")])
  plot(cdat[,c("training", "event")])
  plot(cdat$training - cdat$barrier, cdat[,c("event")])
  cor(cdat$training - cdat$barrier, cdat[,c("event")], use="pairwise.complete.obs") # .72 for wave2
  summary(lm(event ~ training + barrier, cdat))
}

write.csv(cdat, file="corr.csv", row.names=FALSE)
