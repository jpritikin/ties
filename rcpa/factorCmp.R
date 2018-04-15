library(loo)

source("modelUtil.R")
options(mc.cores = 1)

load(paste0(outputDir(), "fit2t4.rda"))  # independent
load(paste0(outputDir(), "fit2t2.rda"))  # saturated
load(paste0(outputDir(), "fit2t5.rda"))  # factor model

# rhat

ind_ll <- extract_log_lik(fit2t4, merge_chains = FALSE)
rm(fit2t4)

sat_ll <- extract_log_lik(fit2t2, merge_chains = FALSE)
rm(fit2t2)

fac_ll <- extract_log_lik(fit2t5, merge_chains = FALSE)
rm(fit2t5)

ind_loo <- loo(ind_ll, r_eff=relative_eff(exp(ind_ll)))
sat_loo <- loo(sat_ll, r_eff=relative_eff(exp(sat_ll)))
fac_loo <- loo(fac_ll, r_eff=relative_eff(exp(fac_ll)))

print(ind_loo)
print(lookupContextByDatumIndex(rcd, pareto_k_ids(ind_loo)))
print(sat_loo)
print(lookupContextByDatumIndex(rcd, pareto_k_ids(sat_loo)))
print(fac_loo)
outliers <- lookupContextByDatumIndex(rcd, pareto_k_ids(fac_loo))
print(outliers)

indVsFac <- compare(ind_loo, fac_loo)
satVsFac <- compare(sat_loo, fac_loo)

save(ind_loo, sat_loo, fac_loo, indVsFac, satVsFac, outliers, file="factorCmp.rda")

#plotByFacet(fit2t5, rcd)
