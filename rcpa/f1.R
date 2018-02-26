source("modelUtil.R")

rcd <- loadRawData()

fit1s1 <- stan(file = "model1.stan",
                data = prepDataForStan(rcd),
                chains = 6,
                iter = 400,
                control = list(max_treedepth = 12))

save(fit1s1, rcd, file=paste0(outputDir(), "fit1s1.rda"))
