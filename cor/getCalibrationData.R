library(gtools)
{
  wd <- setwd("..")
  source("measures.R")
  setwd(wd)
}

raw1 <- read.csv("data-201405.csv", stringsAsFactors=FALSE)
raw2 <- read.csv("data-201410.csv", stringsAsFactors=FALSE)
raw3 <- read.csv("data-201412.csv", stringsAsFactors=FALSE)
raw4 <- read.csv("data-201509.csv", stringsAsFactors=FALSE)
raw5 <- read.csv("data-201510.csv", stringsAsFactors=FALSE)
raw6 <- read.csv("data-201512.csv", stringsAsFactors=FALSE)

w1 <- prep.cms201312(raw1[258:283])
w1$wave <- "cor-ppool1"
w1$start <- raw1$StartDate
w1$end <- raw1$EndDate

w2 <- prep.cms201409(raw2[282:(282+29-1)])
w2$wave <- "cor-ppool2"
w2$start <- raw2$StartDate
w2$end <- raw2$EndDate

w3 <- prep.cms201410(raw3[282:(282+29-1)])
w3$wave <- "cor-ppool3"
w3$start <- raw3$StartDate
w3$end <- raw3$EndDate

w4 <- prep.cms201508(raw4[282:(282+34-1)])
w4$wave <- "cor-ppool4"
w4$start <- raw4$StartDate
w4$end <- raw4$EndDate

w5 <- prep.cms201510(raw5[282:(282+34-1)])
w5$wave <- "cor-ppool5"
w5$start <- raw5$StartDate
w5$end <- raw5$EndDate

w6 <- prep.cms201511(raw6[282:(282+36-1)])
w6$wave <- "cor-ppool6"
w6$start <- raw6$StartDate
w6$end <- raw6$EndDate

cor.ppool.cms <- smartbind(w1, w2, w3, w4, w5, w6)
cor.ppool.cms$population <- 'uva'
