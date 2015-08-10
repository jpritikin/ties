{
  wd <- setwd("..")
  source("measures.R")
  setwd(wd)
}
source("spammer.R")

wave = 1
raw <- read.csv(sprintf("wave%d-anon.csv", wave), stringsAsFactors=FALSE)
raw <- subset(raw, !(id %in% spammer))
offset <- 3+ifelse(wave==1, 3, 0)
offset <- offset + 14
if (wave == 1) {
  cmsCol <- raw[,offset:(offset+29-1)]
} else {
  cmsCol <- raw[,offset:(offset+24-1)]
  cmsCol <- cbind(NA,NA,NA,NA,NA,cmsCol)
}

germano2015.cms <- cbind(prep.cms201410(cmsCol), uid=raw$uid)
germano2015.cms$wave <- "germano2015"
germano2015.cms$start <- raw$StartDate
germano2015.cms$end <- raw$EndDate
germano2015.cms$population <- 'uva'
