library(rpf)
library(OpenMx)
source("cms-score-lib.R")

try(load("ties-fit.rda"))

ifa.score <- function(grp, df) {
  grp$data <- df
  EAPscores(grp)[,1]
}

ties.score <- function(population, df) {
  df <- cms.testlets(df)
  cms <- cbind(training=ifa.score(tiesPop[[population]]$training, df),
               practice=ifa.score(tiesPop[[population]]$practice, df),
               ties=ifa.score(tiesPop[[population]]$ties, df))
  cms
}
