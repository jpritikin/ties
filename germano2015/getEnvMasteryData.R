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
germano2015.envm <- prep.ryff.envMastery14(raw[offset:(offset + 14 - 1)])

germano2015.envm$wave <- "germano2015"
germano2015.envm$start <- raw$StartDate
germano2015.envm$end <- raw$EndDate
