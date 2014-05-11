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

if (0) {
  germano2014.cms <- cbind(prep.cms201312(raw2[67:92]), uid=raw2$uid)
  germano2014.cms$wave <- "germano2014"
  germano2014.cms$start <- raw2$StartDate
  germano2014.cms$end <- raw2$EndDate
  save(germano2014.cms, file="germano2014-cms.rda")
}

wave1 <- cbind(wave1, cms.score(prep.cms201312(raw1[70:95])))
wave2 <- cbind(wave2, cms.score(prep.cms201312(raw2[67:92])))
wave3 <- cbind(wave3, cms.score(prep.cms201312(raw3[67:92])))
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
verifyDigest2 <- c("366287a482c1b4ea838c47229482c5fd", "b878525361621e136b83cc56dbe62c69",
                   "0aeb6964dab5d24cb5032862ecbae217", "5b33c257d3d33c0c9d26edea7cae85e3",
                   "14e40f383a39f6babd8762aec1de608d", "f5e1c5574465d095a7ae3b9d183d6a86",
                   "83f51b1728bf893f216e124c7f9fede1")
verifyDigest3 <- c("edff78b1027dc6c4ce9bf921c7a08f30", "be4e353dedd0af5b2be45455686dd72c",
                   "414cc102ba76ce0837700857dc204f12", "9c24a0f393ed7066d8c6922b94aa552c",
                   "733fbb1b20681fdfbddc6fa9f930633e", "e2eeef65dd5008fb8e3816d6f7b78c83",
                   "16bb5d12a37642c36820f158afd635c0")
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
}

write.table(wave1, file="prep1.csv", row.names=FALSE)
write.table(wave2, file="prep2.csv", row.names=FALSE)
write.table(wave3, file="prep3.csv", row.names=FALSE)
