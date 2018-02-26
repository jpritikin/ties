source("modelUtil.R")

rcd <- loadSingleFactorData()

fit2t2 <- stan(file = "model2.stan",
               data = prepDataForStan(rcd),
               chains = 6,
               iter = 500,
               include=FALSE,
               pars=c('thetaCorChol'),
               control = list(max_treedepth = 15))

save(fit2t2, rcd, file=paste0(outputDir(), "fit2t2.rda"))
