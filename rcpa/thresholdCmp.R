# saturated covariance matrix
# 1 set of thresholds vs separate thresholds for every facet

library(loo)
source("modelUtil.R")

load(paste0(outputDir(), "fit2t2.rda"))

regPar <- c('lp__', 'alpha', 'theta', paste0('threshold',1:2), 'thetaCor')
head(worstNeff(fit2t2, regPar), n=20)

load(paste0(outputDir(), "fit2t3.rda"))

regPar <- c('lp__', 'alpha', 'theta', paste0('threshold',1:2), 'thetaCor')
head(worstNeff(fit2t3, regPar), n=20)

fit1_ll <- extract_log_lik(fit2t2)
fit2_ll <- extract_log_lik(fit2t3)

loo1 <- loo(fit1_ll)
loo2 <- loo(fit2_ll)

compare(loo1, loo2)
