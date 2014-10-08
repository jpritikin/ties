library(testthat)
library(digest)
library(ggplot2)
{
  wd <- setwd("..")
  source("measures.R")
  source("cms-score.R")
  setwd(wd)
}

raw1 <- read.csv("wave1-anon.csv", stringsAsFactors=FALSE)
raw2 <- read.csv("wave2-anon.csv", stringsAsFactors=FALSE)
raw3 <- read.csv("wave3-anon.csv", stringsAsFactors=FALSE)

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
  start=raw1$StartDate,
  end=raw1$EndDate,
  born=raw1[[3]],
  sex=factor(raw1[[4]], levels=SexItem, labels=tolower(SexItem)),
  rel=factor(raw1[[5]], levels=RelaItem, labels=tolower(RelaItemShort)))

wave2 <- data.frame(
  id=raw2$id,
  uid=raw2$uid,
  start=raw2$StartDate,
  end=raw2$EndDate)
wwmap <- match(wave2$id, wave1$id)
for (col in c('born', "sex", "rel")) {
  wave2[[col]] <- wave1[[col]][wwmap]
}

wave3 <- data.frame(
  id=raw3$id,
  uid=raw3$uid,
  start=raw3$StartDate,
  end=raw3$EndDate)
wwmap <- match(wave3$id, wave1$id)
for (col in c('born', "sex", "rel")) {
  wave3[[col]] <- wave1[[col]][wwmap]
}

got <- score.rrq(raw1[6:(6+24-1)])
wave1$reflection <- got$reflection
wave1$rumination <- got$rumination

got <- score.rrq(raw2[3:(3+24-1)])
wave2$reflection <- got$reflection
wave2$rumination <- got$rumination

got <- score.rrq(raw3[3:(3+24-1)])
wave3$reflection <- got$reflection
wave3$rumination <- got$rumination

wave1$psqi <- score.psqi(raw1[30:(30+19-1)])
wave2$psqi <- score.psqi(raw2[27:(27+19-1)])
wave3$psqi <- score.psqi(raw3[27:(27+19-1)])

got <- score.dass(raw1[49:(49+21-1)])
for (k in names(got)) { wave1[[k]] <- got[[k]] }

got <- score.dass(raw2[46:(46+21-1)])
for (k in names(got)) { wave2[[k]] <- got[[k]] }

got <- score.dass(raw3[46:(46+21-1)])
for (k in names(got)) { wave3[[k]] <- got[[k]] }

cms1 <- prep.cms201312(raw1[70:95])
cms2 <- prep.cms201312(raw2[67:92])
cms3 <- prep.cms201312(raw3[67:92])

wave1 <- cbind(wave1, cms.score(cms1))
wave2 <- cbind(wave2, cms.score(cms2))
wave3 <- cbind(wave3, cms.score(cms3))
if (0) {
  apply(wave3, 2, function (c) sum(is.na(c)))
}

verify.col <- c("reflection", "rumination", "psqi", "dass.d", "dass.a", "dass.s",  "dass.na")
if (0) {
  for (c in verify.col) {
    print(digest(wave1[,c]))
  }
}
verifyDigest1 <- c("5c048a9bde4fe7d45a2feea10894cdb4", "8e01999b6cedb96e28c7141e2237610e",
                   "40200e2e00b9ffbf1bf00a00afa92d9e", "c762151cc430e67eecd463e32088aa13",
                   "031af9ac8379ef50e25b583f8b3097bf", "5fca0690cad3c7c67ee36780703e227f",
                   "40ceca7f66dbb3dfbafa18db5e6c25fe")
verifyDigest2 <- c("bd7f2c37039e0a56b7b882e798d01558", "dd8ea8d635334f15d1b914452dbf9c52",
                   "c4951099654ee0b200504d2ddea036bb", "08edbbc65af5e2773898ca5187c3a315",
                   "7004c4c6f840a31cd681c8b06ba76221", "c3a3294297e6b7a14f5dcdf13ff74687",
                   "7400d78035f5a08db98af30ad9754dfe")
verifyDigest3 <- c("4c2d04afc4f8a03f09886d48c33a5fea", "a72053073791beef15a418b29f1b7774",
                   "3bef1ba6fa58f1d86dca71c3274ca3f1", "6a9738b15a4b871b5e09c2e9c2cd861b",
                   "1c2ef03ccdf9cce9c10494ec354c0b98", "d3b7faa04cf71ccba4f3d5016dc6cc15",
                   "d71026cd07eaf6b74f0486f914f9dac9")
for (c in 1:length(verify.col)) {
  expect_equal(digest(wave1[[verify.col[[c]]]]), verifyDigest1[[c]], info=verify.col[[c]])
  expect_equal(digest(wave2[[verify.col[[c]]]]), verifyDigest2[[c]], info=verify.col[[c]])
  expect_equal(digest(wave3[[verify.col[[c]]]]), verifyDigest3[[c]], info=verify.col[[c]])
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

write.table(wave1, file="prep1.csv", row.names=FALSE)
write.table(wave2, file="prep2.csv", row.names=FALSE)
write.table(wave3, file="prep3.csv", row.names=FALSE)
