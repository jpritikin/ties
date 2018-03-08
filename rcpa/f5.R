source("modelUtil.R")

rcd <- loadSingleFactorData()

fit2t3 <- stan(file = "model4.stan",
               data = prepDataForStan(rcd),
               chains = stanChains,
               iter = 1000,
               include=FALSE,
               pars=c('theta_raw', 'thetaCorChol'),
               control = list(max_treedepth = 15))

save(fit2t3, rcd, file=paste0(outputDir(), "fit2t3.rda"))
