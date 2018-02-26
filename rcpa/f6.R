source("modelUtil.R")

rcd <- loadSingleFactorData()

fit2t4 <- stan(file = "model1.stan",
               data = prepDataForStan(rcd),
               chains = 6,
               iter = 500,
               control = list(max_treedepth = 15))

save(fit2t4, rcd, file=paste0(outputDir(), "fit2t4.rda"))
