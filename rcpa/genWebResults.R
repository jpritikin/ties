source("modelUtil.R")

load(paste0(outputDir(), "fit2t5.rda"))

sim_fit <- fit2t5

facetNames <- extractFacetNames(rcd)
NFACETS <- length(facetNames)
whitelist <- extractPalist(rcd)

# check whether to flip loading sign
fl <- as.data.frame(summary(sim_fit, pars=c("flowLoadings"), probs=c(.025,.975))$summary)
fl$index <- 1:nrow(fl)
rownames(fl) <- facetNames
fl[order(fl[,'mean']),]

estimator <- 'mean'

df <- summary(sim_fit, pars=c(paste0("alpha[",1:NFACETS,']')), probs=.5)$summary
alpha <- matrix(df[,estimator], nrow=1,
                dimnames=list(NULL, facetNames))
print(alpha)

flow <- summary(sim_fit, pars=c("flow"), probs=.5)$summary

df <- summary(sim_fit, pars=c("theta"), probs=.5)$summary
tar <- array(df[,estimator], dim=c(NFACETS, length(whitelist)))

if (nrow(df) != prod(dim(tar))) stop("mismatch")
tar <- rbind(tar, flow[, estimator])

if (0) {
  library(shinystan)
  launch_shinystan(sim_fit)
}

sampleSize <- calcSampleSize(rcd)

cat(paste("var RCPA_DATA=",
          toJSON(tar, matrix="columnmajor", digits=3),
          ";\nvar RCPA_FACETS=",
          toJSON(c(facetNames,'flow')),
          ";\nvar RCPA_FACET_ALPHA=",
          toJSON(c(alpha[1,],0)),
          ";\nvar RCPA_PA=",
          toJSON(whitelist),
          ";\nvar RCPA_PA_SAMPLESIZE=",
          toJSON(sampleSize),
          ";"),
    file="pa-browser/rcpa-data.js", fill=TRUE)

