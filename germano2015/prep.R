library(ggplot2)
{
  wd <- setwd("..")
  source("measures.R")
  source("ties-score.R")
  setwd(wd)
}
source("spammer.R")

SexItem <- c("Male","Female")

RelaItem = c('Single',
             'In a long-term relationship (i.e. together more than a year)',
             'Other')
RelaItemShort = c('Single',
                  'Together',
                  'Other')

# declare global variables
born <- NULL
sex <- NULL
rel <- NULL
labTime <- NULL
labTA <- NULL

for (wave in 1:9) {
  raw <- read.csv(sprintf("wave%d-anon.csv", wave), stringsAsFactors=FALSE)
  offset <- 3+ifelse(wave==1, 3, 0)
  if (wave == 1) {
    born <- raw[[3]]
    sex <- factor(raw[[4]], levels=SexItem, labels=tolower(SexItem))
    rel <- factor(raw[[5]], levels=RelaItem, labels=tolower(RelaItemShort))
    labTime <- raw$labTime
    labTA <- raw$labTA
  }
  scored <- data.frame(
    id=raw$id, uid=raw$uid,
    start=raw$StartDate, end=raw$EndDate,
    born=born, sex=sex, rel=rel, labTime = labTime, labTA=labTA)
  # deal with lab section TODO
  scored$envMastery <- score.ryff.envMastery14(raw[offset:(offset + 14 - 1)])
  offset <- offset + 14 + ifelse(wave==1, 5, 0)
  cmsCol <- raw[,offset:(offset+24-1)]
  cmsCol <- cbind(NA,NA,NA,NA,NA,cmsCol)
  cms <- prep.cms201410(cmsCol)
  scored <- cbind(scored, ties.score("uva", cms))
  mask <- match(spammer, scored$id)
  mask <- mask[!is.na(mask)]
  scored[mask,c('envMastery', 'training', 'ties')] <- NA
  write.table(scored, file=sprintf("prep%d.csv", wave), row.names=FALSE)
}

if (0) {
  #cat(deparse(colnames(wave2)))
  cor(wave1[,c("reflection",  "rumination", "psqi", "dass.d", "dass.a",
               "dass.s", "dass.na",  "barrier", "training", "event")], use="pairwise.complete.obs")
  plot(wave2[,c("barrier", "event")])
  plot(wave2[,c("training", "event")])
  plot(wave2$training - wave2$barrier, wave2[,c("event")])
  cor(wave2$training - wave2$barrier, wave2[,c("event")], use="pairwise.complete.obs") # .72 for wave2
  
  df <- rbind(cbind(t=1, wave1[,c('id','event')]),
              cbind(t=2, wave2[,c('id','event')]),
              cbind(t=3, wave3[,c('id','event')]))
  ggplot(df, aes(x=t, y=event, group=id)) + geom_line()
  
  etraj <- cbind(wave1[,c('id','event')], wave2[,c('event')], wave3[,c('event')])
  colnames(etraj) <- c('id',paste('t',1:3,sep=""))
  emin <- min(etraj$t1, na.rm = TRUE)
  mask <- etraj$t1 > emin  & etraj$t2 == emin & etraj$t3 > emin
  mask <- !is.na(mask) & mask
  mask <- etraj$t1 == emin  & etraj$t2 > emin & etraj$t3 == emin
  mask <- !is.na(mask) & mask
  mask <- etraj$t2 > emin & etraj$t3 == emin
  mask <- !is.na(mask) & mask
  
  df <- rbind(cbind(t=1, wave1[,c('id','training')]),
              cbind(t=2, wave2[,c('id','training')]),
              cbind(t=3, wave3[,c('id','training')]))
  df[df$t==1 & is.na(df$training), 'training'] <- min(df$training, na.rm=TRUE)
  ggplot(df, aes(x=t, y=training, group=id)) + geom_line()
}
