library(loo)

source("modelUtil.R")

load(paste0(outputDir(), "fitwic.rda"))

load(paste0(outputDir(), "fitwip.rda"))
fit <- fitwip

facetNames <- extractFacetNames(rcd)
palist <- extractPalist(rcd)

df <- summary(fit1s2, pars=c("theta"), probs=c())$summary
tarC <- array(df[,'mean'], dim=c(length(facetNames), length(palist)))
dimnames(tarC) <- list(facetNames, palist)

df <- summary(fit, pars=c("theta"), probs=c())$summary
tar <- array(df[,'mean'], dim=c(length(facetNames), length(palist)))
dimnames(tar) <- list(facetNames, palist)

wic_ll <- extract_log_lik(fit1s2, merge_chains = FALSE)
wip_ll <- extract_log_lik(fitwip, merge_chains = FALSE)
wic_loo <- loo(wic_ll, r_eff=relative_eff(exp(wic_ll)))
wip_loo <- loo(wip_ll, r_eff=relative_eff(exp(wip_ll)))

c1 <- compare(wic_loo, wip_loo)
print(c1)
print(c1[1]/c1[2])

print(cor(c(tar), c(tarC)))  # .9938

rangeByItem <- diff(apply(tar, 1, range))

df <- summary(fit, pars=c("sigma"), probs=c())$summary
rownames(df) <- facetNames
sigmaByItem <- df[,'mean']

save(sigmaByItem, rangeByItem, file="plotAllItems.rda")

plotByFacet(fit, rcd)
