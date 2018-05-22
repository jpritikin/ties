library(jsonlite)

source("modelUtil.R")

load(paste0(outputDir(), "fitsfp.rda"))  # factor model

sim_fit <- fitsfp

facetNames <- extractFacetNames(rcd)
NFACETS <- length(facetNames)
whitelist <- extractPalist(rcd)

# check whether to flip loading sign
fl <- as.data.frame(summary(sim_fit, pars=c("flowLoadings"), probs=c())$summary)
rownames(fl) <- facetNames

flow <- summary(sim_fit, pars=c("flow"), probs=.5)$summary

df <- summary(sim_fit, pars=c("theta"), probs=.5)$summary
tar <- array(df[,'mean'], dim=c(NFACETS, length(whitelist)))

if (nrow(df) != prod(dim(tar))) stop("mismatch")
tar <- rbind(tar, flow[, 'mean'])

if (0) {
  library(shinystan)
  launch_shinystan(sim_fit)
}

sampleSize <- calcSampleSize(rcd)

cat(paste("var RCPA_DATA=",
          toJSON(tar, matrix="columnmajor", digits=3),
          ";\nvar RCPA_FACETS=",
          toJSON(c(facetNames,'flow')),
          ";\nvar RCPA_FACET_ALPHA=",  # rename TODO
          toJSON(c(abs(fl[,'mean']))),
          ";\nvar RCPA_PA=",
          toJSON(whitelist),
          ";\nvar RCPA_PA_SAMPLESIZE=",
          toJSON(sampleSize),
          ";"),
    file="pa-browser/rcpa-data.js", fill=TRUE)

