library(gtools)
{
  wd <- setwd("..")
  source("measures.R")
  setwd(wd)
}

raw1 <- read.csv("data-201405.csv", stringsAsFactors=FALSE)
raw2 <- read.csv("data-201410.csv", stringsAsFactors=FALSE)
raw3 <- read.csv("data-201412.csv", stringsAsFactors=FALSE)

w1 <- prep.ryff9(raw1[103:156])
w1$wave <- "cor-ppool1"
w1$start <- raw1$StartDate
w1$end <- raw1$EndDate

w2 <- prep.ryff9(raw2[127:180])
w2$wave <- "cor-ppool2"
w2$start <- raw2$StartDate
w2$end <- raw2$EndDate

w3 <- prep.ryff9(raw3[127:180])
w3$wave <- "cor-ppool3"
w3$start <- raw3$StartDate
w3$end <- raw3$EndDate

cor.ppool <- smartbind(w1, w2, w3)
cor.ppool.envm <- cbind(cor.ppool[,c(10:18)], cor.ppool[,c('wave', 'start', 'end')])

