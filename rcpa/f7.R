source("modelUtil.R")

rcd <- loadSingleFactorData()

# With iter=1000, some Rhat>1.1 seen

fit2t5 <- stan(file = "model3.stan",
               data = prepDataForStan(rcd),
               chains = stanChains,
               iter = 2000,
               include=FALSE,
               pars=c('rawFlow', 'rawTheta', 'rawLoadings'),
               control = list(max_treedepth = 15))

save(fit2t5, rcd, file=paste0(outputDir(), "fit2t5.rda"))
