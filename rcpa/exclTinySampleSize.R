# independence model
# all data vs tiny sample size activities removed

source("modelUtil.R")

#colSums(is.na(rcd[-1:-4]))  # seems fairly uniform TODO

load(paste0(outputDir(), "fit1s1.rda"))

regPar <- c('lp__', 'alpha', 'theta', paste0('threshold',1:2))
head(worstNeff(fit1s1, regPar), n=20)

facetNames <- extractFacetNames(rcd)
palist <- extractPalist(rcd)

estimator <- 'mean'
df <- summary(fit1s1, pars=c("alpha"), probs=.5)$summary
alpha <- matrix(df[,estimator], nrow=1,
                dimnames=list(NULL, facetNames))
print(alpha)

df <- summary(fit1s1, pars=c("theta"), probs=c(.5))$summary
tar1 <- array(df[,estimator], dim=c(length(facetNames), length(palist)))

load(paste0(outputDir(), "fit1s2.rda"))

head(worstNeff(fit1s2, regPar), n=20)

whitelist <- extractPalist(rcd)

df <- summary(fit1s2, pars=c("theta"), probs=c(.5))$summary
tar2 <- array(df[,estimator], dim=c(length(facetNames), length(whitelist)))

hist(c(tar1), breaks = 100)
hist(c(tar2), breaks = 100)

cor(c(tar1[,match(whitelist, palist)]), c(tar2))

plotByFacet(fit1s2, rcd)
