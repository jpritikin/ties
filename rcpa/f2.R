source("modelUtil.R")

rcd <- loadWhitelistRawData()

fit1s2 <- stan(file = "model1.stan",
                data = prepDataForStan(rcd),
                chains = 6,
                iter = stanIter,
                control = list(max_treedepth = 12))

save(fit1s2, rcd, file=paste0(outputDir(), "fit1s2.rda"))
