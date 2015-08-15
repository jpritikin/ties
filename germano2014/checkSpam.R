library(testthat)
library(digest)
library(ggplot2)
{
  wd <- setwd("..")
  source("measures.R")
  source("ties-score.R")
  setwd(wd)
}

cube <- NULL

for (wave in 1:3) {
  raw <- read.csv(sprintf("wave%d-anon.csv", wave), stringsAsFactors=FALSE, header=TRUE)

  offset <- ifelse(wave == 1, 70, 67)
  cms <- prep.cms201312(raw[offset:(offset+25)])
  cms <- cms.testlets(cms)

  imask <- which(sapply(cms, is.factor))
  if (is.null(cube)) {
    cube <- array(dim=c(nrow(cms), length(imask), 3))
    rownames(cube) <- raw$id
    colnames(cube) <- names(imask)
  }
  cube[,,wave] <- sapply(cms[,imask], unclass)
}

spam <- ties.spam.score(cube)
stem(spam)
spammer <- names(spam[spam <= -9])
cat(deparse(as.integer(spammer)))
