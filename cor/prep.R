library(testthat)
library(digest)
library(ggplot2)
library(gtools)
{
  wd <- setwd("..")
  source("measures.R")
  source("ties-score.R")
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

cdat1 <- cbind(cdat1, ties.score('uva', prep.cms201312(raw1[258:283])))

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

cdat2 <- cbind(cdat2, ties.score('uva', prep.cms201409(raw2[282:(282+29-1)])))

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

cdat3 <- cbind(cdat3, ties.score('uva', prep.cms201410(raw3[282:(282+29-1)])))

cdat3$sleep <- -score.psqi(raw3[311:(311+19-1)])

# ------------------------------------------------------------------
raw4 <- read.csv("data-201509.csv", stringsAsFactors=FALSE)

cdat4 <- data.frame(
  start=raw4$StartDate,
  end=raw4$EndDate,
  born=raw4[[3]],
  sex=factor(raw4[[4]], levels=SexItem, labels=tolower(SexItem)),
  rel=factor(raw4[[5]], levels=RelaItem, labels=tolower(RelaItemShort)))

# SF12 (ignore for now) 6:17

got <- score.ipipBig5(raw4[18:67])
for (n in names(got)) cdat4[[n]] <- got[[n]]

got <- score.panas(raw4[68:87])
for (n in names(got)) cdat4[[n]] <- got[[n]]

got <- score.5fMindfulness2(code.5fMindfulness2(raw4[88:126]))
for (n in names(got)) cdat4[[n]] <- got[[n]]

got <- score.ryff9(raw4[127:180])
for (n in names(got)) cdat4[[n]] <- got[[n]]

got <- score.mcFormC(raw4[184:196])
cdat4$socialDesirable <- got - mean(got, na.rm=TRUE)

got <- score.dass(raw4[197:(197+21-1)])
for (n in names(got)) cdat4[[n]] <- got[[n]]

cdat4$mwb <- score.wemwbs(raw4[218:(218+7-1)])

cdat4$ei <- score.ei(raw4[225:(225+33-1)])

got <- score.rrq(raw4[258:(258+24-1)])
for (n in names(got)) cdat4[[n]] <- got[[n]]

cdat4 <- cbind(cdat4, ties.score('uva', prep.cms201508(raw4[282:(282+34-1)])))

cdat4$sleep <- -score.psqi(raw4[316:(316+19-1)])

# ------------------------------------------------------------------
raw5 <- read.csv("data-201510.csv", stringsAsFactors=FALSE)

cdat5 <- data.frame(
  start=raw5$StartDate,
  end=raw5$EndDate,
  born=raw5[[3]],
  sex=factor(raw5[[4]], levels=SexItem, labels=tolower(SexItem)),
  rel=factor(raw5[[5]], levels=RelaItem, labels=tolower(RelaItemShort)))

# SF12 (ignore for now) 6:17

got <- score.ipipBig5(raw5[18:67])
for (n in names(got)) cdat5[[n]] <- got[[n]]

got <- score.panas(raw5[68:87])
for (n in names(got)) cdat5[[n]] <- got[[n]]

got <- score.5fMindfulness2(code.5fMindfulness2(raw5[88:126]))
for (n in names(got)) cdat5[[n]] <- got[[n]]

got <- score.ryff9(raw5[127:180])
for (n in names(got)) cdat5[[n]] <- got[[n]]

got <- score.mcFormC(raw5[184:196])
cdat5$socialDesirable <- got - mean(got, na.rm=TRUE)

got <- score.dass(raw5[197:(197+21-1)])
for (n in names(got)) cdat5[[n]] <- got[[n]]

cdat5$mwb <- score.wemwbs(raw5[218:(218+7-1)])

cdat5$ei <- score.ei(raw5[225:(225+33-1)])

got <- score.rrq(raw5[258:(258+24-1)])
for (n in names(got)) cdat5[[n]] <- got[[n]]

cdat5 <- cbind(cdat5, ties.score('uva', prep.cms201510(raw5[282:(282+34-1)])))

cdat5$sleep <- -score.psqi(raw5[316:(316+19-1)])

# ------------------------------------------------------------------
raw6 <- read.csv("data-201512.csv", stringsAsFactors=FALSE)

cdat6 <- data.frame(
  start=raw6$StartDate,
  end=raw6$EndDate,
  born=raw6[[3]],
  sex=factor(raw6[[4]], levels=SexItem, labels=tolower(SexItem)),
  rel=factor(raw6[[5]], levels=RelaItem, labels=tolower(RelaItemShort)))

# SF12 (ignore for now) 6:17

got <- score.ipipBig5(raw6[18:67])
for (n in names(got)) cdat6[[n]] <- got[[n]]

got <- score.panas(raw6[68:87])
for (n in names(got)) cdat6[[n]] <- got[[n]]

got <- score.5fMindfulness2(code.5fMindfulness2(raw6[88:126]))
for (n in names(got)) cdat6[[n]] <- got[[n]]

got <- score.ryff9(raw6[127:180])
for (n in names(got)) cdat6[[n]] <- got[[n]]

got <- score.mcFormC(raw6[184:196])
cdat6$socialDesirable <- got - mean(got, na.rm=TRUE)

got <- score.dass(raw6[197:(197+21-1)])
for (n in names(got)) cdat6[[n]] <- got[[n]]

cdat6$mwb <- score.wemwbs(raw6[218:(218+7-1)])

cdat6$ei <- score.ei(raw6[225:(225+33-1)])

got <- score.rrq(raw6[258:(258+24-1)])
for (n in names(got)) cdat6[[n]] <- got[[n]]

cdat6 <- cbind(cdat6, ties.score('uva', prep.cms201511(raw6[282:(282+36-1)])))

cdat6$sleep <- -score.psqi(raw6[318:(318+19-1)])

# --------------------------------------------------------

# a harmless warning is issued here with gtools 3.5
cdat <- smartbind(cdat1, cdat2, cdat3, cdat4, cdat5, cdat6)

mind5f <- c('nonreact', 'observe', 'actAware', 'describe', 'nonjudge')
perm <- c(setdiff(colnames(cdat), mind5f), mind5f)
cdat <- cdat[,perm]
perm <- c(setdiff(colnames(cdat), c('training', 'practice', 'ties')),
          c('training', 'practice', 'ties'))
cdat <- cdat[,perm]

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
