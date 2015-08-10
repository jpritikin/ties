library(gtools)

envm <- local({
  wd <- setwd("cor")
  source("getEnvMasteryData.R")
  setwd(wd)
  cor.ppool.envm
})

envm <- smartbind(envm, local({
  wd <- setwd("germano2015")
  source("getEnvMasteryData.R")
  setwd(wd)
  germano2015.envm
}))


save(envm, file="envMastery.rda")
