source("modelUtil.R")

load(paste0(outputDir(), "fit1s2.rda"))

facetNames <- extractFacetNames(rcd)
palist <- extractPalist(rcd)

df <- summary(fit1s2, pars=c("theta"), probs=c())$summary
tar <- array(df[,'mean'], dim=c(length(facetNames), length(palist)))
dimnames(tar) <- list(facetNames, palist)

rangeByItem <- diff(apply(tar, 1, range))

save(rangeByItem, file="plotAllItems.rda")

plotByFacet(fit1s2, rcd)
