source("modelUtil.R")

rcd <- loadWhitelistRawData()

fit1s2 <- stan(file = "model1.stan",
                data = prepDataForStan(rcd),
                chains = stanChains,
                iter = 1000,
                control = list(max_treedepth = 15))

save(fit1s2, rcd, file=paste0(outputDir(), "fit1s2.rda"))
