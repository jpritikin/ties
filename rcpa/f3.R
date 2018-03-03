source("modelUtil.R")

rcd <- loadWhitelistRawData()

fit2t1 <- stan(file = "model2.stan",
                data = prepDataForStan(rcd),
                chains = stanChains,
                iter = 2000,
                include=FALSE,
                pars=c('thetaCorChol'),
                control = list(max_treedepth = 15, adapt_delta=.9))

save(fit2t1, rcd, file=paste0(outputDir(), "fit2t1.rda"))
