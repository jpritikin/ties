{
  wd <- setwd("..")
  source("measures.R")
  setwd(wd)
}
raw2 <- read.csv("wave2-anon.csv", stringsAsFactors=FALSE)

germano2014.cms <- cbind(prep.cms201312(raw2[67:92]), uid=raw2$uid)
germano2014.cms$wave <- "germano2014"
germano2014.cms$start <- raw2$StartDate
germano2014.cms$end <- raw2$EndDate
