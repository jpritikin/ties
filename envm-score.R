library(rpf)
library(OpenMx)

try(load("envm-fit.rda"))

envm.score <- function(df) {
  for (c in colnames(envmGrp$spec)) {
    if (is.null(df[[c]])) df[[c]] <- NA
  }
  envmGrp$data <- df
  EAPscores(envmGrp)[,1]
}
