library(loo)
source("modelUtil.R")

load(paste0(outputDir(), "fit2t4.rda"))  # independent
load(paste0(outputDir(), "fit2t2.rda"))  # saturated
load(paste0(outputDir(), "fit2t5.rda"))  # factor model

# rhat

ind_ll <- extract_log_lik(fit2t4)
sat_ll <- extract_log_lik(fit2t2)
fac_ll <- extract_log_lik(fit2t5)

ind_loo <- loo(ind_ll)
sat_loo <- loo(sat_ll)
fac_loo <- loo(fac_ll)

print(ind_loo)
print(sat_loo)
print(fac_loo)

compare(ind_loo, fac_loo)
compare(sat_loo, fac_loo)

# is this sensible?
(ind_loo$looic - fac_loo$looic)/(ind_loo$looic - sat_loo$looic)

plotByFacet(fit2t5, rcd)

pval <- ppc(fit2t5, rcd)

print(sum(pval<.05) / length(pval))
print(apply(pval, 1, function(x) sum(x<.05)))
print(apply(pval, 2, function(x) sum(x<.05)))
