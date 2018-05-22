options(width=100)
library(loo)

source("modelUtil.R")
options(mc.cores = 1)

load(paste0(outputDir(), "fitsip.rda"))  # independent
load(paste0(outputDir(), "fitssp.rda"))  # saturated
load(paste0(outputDir(), "fitsfp.rda"))  # factor model

# rhat

ind_ll <- extract_log_lik(fitsip, merge_chains = FALSE)
sat_ll <- extract_log_lik(fitssp, merge_chains = FALSE)
fac_ll <- extract_log_lik(fitsfp, merge_chains = FALSE)

ind_loo <- loo(ind_ll, r_eff=relative_eff(exp(ind_ll)))
sat_loo <- loo(sat_ll, r_eff=relative_eff(exp(sat_ll)))
fac_loo <- loo(fac_ll, r_eff=relative_eff(exp(fac_ll)))

outlierTable <- function(rcd, loo1) {
  ids <- pareto_k_ids(loo1)
  if (0 == length(ids)) return(NULL)
  df <- lookupContextByDatumIndex(rcd, ids)
  df <- cbind(pareto=pareto_k_values(loo1)[ids], df)
  df <- df[order(-df$pareto),]
  df
}

print(ind_loo)
outlierTable(rcd, ind_loo)
print(sat_loo)
outlierTable(rcd, sat_loo)
print(fac_loo)
outlierTable(rcd, fac_loo)

indVsFac <- compare(ind_loo, fac_loo)
satVsFac <- compare(sat_loo, fac_loo)

save(ind_loo, sat_loo, fac_loo, indVsFac, satVsFac, file="factorCmp.rda")
