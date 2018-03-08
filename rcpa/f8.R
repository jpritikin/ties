source("modelUtil.R")

rcd <- loadSimData()

fit2t6 <- stan(file = "model2.stan",
               data = prepDataForStan(rcd),
               chains = stanChains,
               iter = 1000,
               include=FALSE,
               pars=c('theta_raw', 'thetaCorChol'),
               control = list(max_treedepth = 15, adapt_delta=.9))

save(fit2t6, rcd, file=paste0(outputDir(), "fit2t6.rda"))
