library(rstan)
library(jsonlite)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rcd <- read.csv("rawData.csv")

NCMP <- nrow(rcd)
NFACETS <- ncol(rcd) - 2L

palist <- unique(c(as.character(rcd$pa1), as.character(rcd$pa2)))
NPA <- length(palist)

sim_fit <- stan(file = "model2.stan",
                data = list(NPA=NPA, NFACETS=NFACETS, NCMP=NCMP,
                            pa1=match(rcd$pa1, palist),
                            pa2=match(rcd$pa2, palist),
                            diff=rcd[-1:-2]),
                chains = 6, 
                iter = 200)

summary(summary(sim_fit)$summary[,'Rhat'])

plot(sim_fit, pars=c(paste0("threshold",1:2)))

df <- summary(sim_fit, pars=c("theta"), probs=.5)$summary
result <- matrix(df[,"50%"], byrow=TRUE, nrow=NPA, ncol=NFACETS,
                 dimnames=list(palist, colnames(rcd[-1:-2])))

if (0) {
  # compared with the simple BradleyTerry2 model
  cor(c(result), c(sresult), use="pairwise.complete.obs")  # about .6
  
  sort(result[,'waiting'])

  library(shinystan)
  launch_shinystan(sim_fit)
}

cat(paste("var RCPA_DATA=",
          toJSON(result, matrix="rowmajor", digits=3),
          ";\nvar RCPA_FACETS=",
          toJSON(colnames(result)),
          ";\nvar RCPA_PA=",
          toJSON(rownames(result)),
          ";"),
    file="pa-browser/rcpa-data.js", fill=TRUE)
