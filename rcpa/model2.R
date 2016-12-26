library(rstan)
library(jsonlite)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

if (1) {
  rcd <- read.csv("rawData.csv")
  
  NCMP <- nrow(rcd)
  if (NCMP < 1) { stop("No data?") }
  NFACETS <- ncol(rcd) - 4L
  
  palist <- unique(c(as.character(rcd$pa1), as.character(rcd$pa2)))
  NPA <- length(palist)
} else {
  softmax <- function(y) {
    exp(y) / sum(exp(y))
  }
  NCMP <- 100
  NFACETS <- 2
  fnames <- c('waiting', 'injury')
  
  palist <- c('skydiving', 'surfing', 'running', 'walking')
  NPA <- length(palist)
  
  theta <- matrix(0, nrow=NPA, ncol=NFACETS,
                   dimnames=list(palist, fnames))
  theta[,'waiting'] <- c(-1, 1, -.75, -.75)
  theta[,'injury'] <- c(1, .5, -.1, -1)
  
  alpha <- .2
  threshold1 <- .5
  threshold2 <- 1
  
  cmp_probs <- function(pa1, pa2, thr1, thr2) {
    diff = pa1 - pa2;
    unsummed <- c(0, diff - thr2, diff - thr1, diff + thr1, diff + thr2) * alpha
    cumsum(unsummed);
  }
  
  simData <- NULL
  for (rep in 1:NCMP) {
    pa <- sample(palist, 2)
    pa1 <- pa[1]
    pa2 <- pa[2]
    row <- data.frame(pa1=factor(pa1, levels=palist),
                      pa2=factor(pa2, levels=palist))
    for (cur in fnames) {
      prob <- cmp_probs(theta[pa1,cur], theta[pa2,cur], threshold1, threshold2)
      pick <- sample(c(-2,-1,0,1,2), 1, prob=softmax(prob))
      row[,cur] <- pick
    }
    simData <- rbind(simData, row)
  }
  rcd <- simData
}

# Nice to add a prior when the only difference is solo/group TODO

sim_fit <- stan(file = "model2.stan",
                data = list(NPA=NPA, NFACETS=NFACETS, NCMP=NCMP,
                            pa1=match(rcd$pa1, palist), l1=rcd$l1,
                            pa2=match(rcd$pa2, palist), l2=rcd$l2,
                            diff=rcd[-1:-4]),
                chains = 6, 
                iter = 500)

summary(summary(sim_fit)$summary[,c('Rhat', 'n_eff')])

neOrder <- order(summary(sim_fit)$summary[,c('n_eff')])
head(summary(sim_fit, probs=.5)$summary[neOrder,], n=20)

#plot(sim_fit, pars=c("thetaScale"))
if (interactive()) {
  plot(sim_fit, pars=c(paste0("threshold",1:2)))
}

df <- summary(sim_fit, pars=c("alpha","theta"), probs=.5)$summary
summary(df[,'mean'] - df[,'50%'])
estimator <- 'mean'

facetNames <- colnames(rcd[-1:-4])

df <- summary(sim_fit, pars=c("alpha"), probs=.5)$summary
alpha <- matrix(df[,estimator], nrow=1,
                dimnames=list(NULL, facetNames))
print(alpha)

df <- summary(sim_fit, pars=c("theta"), probs=.5)$summary
tar <- array(df[,estimator], dim=c(NFACETS, NPA, 3))
cor(c(tar[,,1]), c(tar[,,2]))  #.7
cor(c(tar[,,2]), c(tar[,,3]))  #.62

if (0) {
  toJSON(array(1:8, dim=c(2,2,2)), matrix="columnmajor")
  
  # compared with the simple BradleyTerry2 model
  cor(c(result), c(sresult), use="pairwise.complete.obs")  # about .6
  
  sort(result[,'waiting'])

  library(shinystan)
  launch_shinystan(sim_fit)
}

cat(paste("var RCPA_DATA1=",
          toJSON(tar[,,1], matrix="columnmajor", digits=3),
          ";\nvar RCPA_DATA2=",
          toJSON(tar[,,2], matrix="columnmajor", digits=3),
          ";\nvar RCPA_DATA3=",
          toJSON(tar[,,3], matrix="columnmajor", digits=3),
          ";\nvar RCPA_FACETS=",
          toJSON(facetNames),
          ";\nvar RCPA_FACET_ALPHA=",
          toJSON(alpha[1,]),
          ";\nvar RCPA_PA=",
          toJSON(palist),
          ";"),
    file="pa-browser/rcpa-data.js", fill=TRUE)
