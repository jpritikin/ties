# saturated covariance matrix
# 1 set of thresholds vs separate thresholds for every facet

library(loo)

source("modelUtil.R")
options(mc.cores = 1)

load(paste0(outputDir(), "fit2t2.rda"))  # common threshold
fit1_ll <- extract_log_lik(fit2t2, merge_chains = FALSE)
loo1 <- loo(fit1_ll, r_eff=relative_eff(exp(fit1_ll)))
rm(fit1_ll)

load(paste0(outputDir(), "fit2t3.rda"))  # per facet threshold
fit2_ll <- extract_log_lik(fit2t3, merge_chains = FALSE)
loo2 <- loo(fit2_ll, r_eff=relative_eff(exp(fit2_ll)))
rm(fit2_ll)

looCmp <- compare(loo1, loo2)

# ppc example
ppcDemoPair <- 'hiking:swimming'
ppcDemoFacet <- 'novelty'
ppcDemoData <- ppc1(fit2t2, rcd, ppcDemoPair, ppcDemoFacet)

pval1 <- ppc(fit2t2, rcd)
pval2 <- ppc(fit2t3, rcd)

rm(fit2t2)
rm(fit2t3)

load(paste0(outputDir(), "fit2t6.rda"))

ppcDemoSim <- ppc1(fit2t6, rcd, ppcDemoPair, ppcDemoFacet)

pval3 <- ppc(fit2t6, rcd)

df <- rbind(
  data.frame(pvalue=c(pval1), model="common"),
  data.frame(pvalue=c(pval2), model="per-item"),
  data.frame(pvalue=c(pval3), model="simulated"))

save(looCmp, df, ppcDemoPair, ppcDemoFacet, ppcDemoData, ppcDemoSim, file="thresholdCmp.rda")

q()

library(ggplot2)

ggplot(df, aes(sample=pvalue, color=model)) +
  stat_qq(distribution=stats::qunif, size=.25) + coord_fixed() + geom_abline(slope=1, color="yellow")

sum(c(pval1) < .05) / length(c(pval1))
sum(c(pval2) < .05) / length(c(pval2))
