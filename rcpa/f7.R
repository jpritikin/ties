source("modelUtil.R")

rcd <- loadSingleFactorData()

fit2t5 <- stan(file = "model3.stan",
               data = prepDataForStan(rcd),
               chains = stanChains,
               iter = 2000,
               include=FALSE,
               pars=c('theta_raw', 'rawFlow', 'rawLoadings'),
               control = list(max_treedepth = 15, adapt_delta=.95))

save(fit2t5, rcd, file=paste0(outputDir(), "fit2t5.rda"))
