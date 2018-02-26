source("modelUtil.R")

rcd <- loadWhitelistRawData()

fit2t1 <- stan(file = "model2.stan",
                data = prepDataForStan(rcd),
                chains = 6,
                iter = 500,
                include=FALSE,
                pars=c('thetaCorChol'),
                control = list(max_treedepth = 15))

save(fit2t1, rcd, file=paste0(outputDir(), "fit2t1.rda"))
