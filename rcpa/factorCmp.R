library(loo)
options(loo.cores = 2)

source("modelUtil.R")

load(paste0(outputDir(), "fit2t4.rda"))  # independent
load(paste0(outputDir(), "fit2t2.rda"))  # saturated
load(paste0(outputDir(), "fit2t5.rda"))  # factor model

# rhat

ind_ll <- extract_log_lik(fit2t4)
rm(fit2t4)

sat_ll <- extract_log_lik(fit2t2)
rm(fit2t2)

fac_ll <- extract_log_lik(fit2t5)

ind_loo <- loo(ind_ll)
sat_loo <- loo(sat_ll)
fac_loo <- loo(fac_ll)

print(ind_loo)
print(sat_loo)
print(fac_loo)

compare(ind_loo, fac_loo)
compare(sat_loo, fac_loo)

#plotByFacet(fit2t5, rcd)
