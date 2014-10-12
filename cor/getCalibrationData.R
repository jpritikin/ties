library(gtools)
{
  wd <- setwd("..")
  source("measures.R")
  setwd(wd)
}

raw1 <- read.csv("data-201405.csv", stringsAsFactors=FALSE)
raw2 <- read.csv("data-201410.csv", stringsAsFactors=FALSE)

w1 <- prep.cms201312(raw1[258:283])
w1$wave <- "cor-ppool1"
w1$start <- raw1$StartDate
w1$end <- raw1$EndDate

w2 <- prep.cms201409(raw2[282:(282+29-1)])
w2$wave <- "cor-ppool2"
w2$start <- raw2$StartDate
w2$end <- raw2$EndDate

cor.ppool.cms <- smartbind(w1, w2)
cor.ppool.cms$population <- 'uva'
