source("modelUtil.R")

rcd <- loadSingleFactorData()

fit2t2 <- stan(file = "model2.stan",
               data = prepDataForStan(rcd),
               chains = stanChains,
               iter = 1000,
               include=FALSE,
               pars=c('theta_raw', 'thetaCorChol'),
               control = list(max_treedepth = 15))

save(fit2t2, rcd, file=paste0(outputDir(), "fit2t2.rda"))
