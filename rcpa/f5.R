source("modelUtil.R")

rcd <- loadSingleFactorData()

fit2t3 <- stan(file = "model4.stan",
               data = prepDataForStan(rcd),
               chains = 6,
               iter = 500,
               include=FALSE,
               pars=c('thetaCorChol'),
               control = list(max_treedepth = 15))

save(fit2t3, rcd, file=paste0(outputDir(), "fit2t3.rda"))
