library(rstan)
library(jsonlite)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rcd <- read.csv("rawData.csv")
facetNames <- colnames(rcd[-1:-4]) # TODO move up
facetNames <- facetNames[sample.int(length(facetNames), 5)]

if (nrow(rcd) < 1) { stop("No data?") }
NFACETS <- length(facetNames)

palist <- sort(unique(c(as.character(rcd$pa1), as.character(rcd$pa2))))
palist <- palist[sample.int(length(palist),6)]
NPA <- length(palist)

softmax <- function(y) {
  exp(y) / sum(exp(y))
}

NCMP <- 100 * NPA * (NPA-1)/2  # figure out good size

flowScore <- rnorm(NPA)
flowLoading <- runif(NFACETS, 0.5, 1.5)
print(flowLoading)

theta <- matrix(t(flowLoading %*% t(flowScore)),
                nrow=NPA, ncol=NFACETS,
                dimnames=list(palist, facetNames))
theta <- theta + matrix(rnorm(NPA*NFACETS), nrow=NPA, ncol=NFACETS)
alpha <- 1
#names(alpha) <- colnames(theta)
threshold1 <- -.5
threshold2 <- .5

cmp_probs <- function(alpha, pa1, pa2, thr1, thr2) {
  diff = pa1 - pa2;
  unsummed <- c(0, diff - (thr1+thr2), diff - thr1, diff + thr1, diff + (thr1+thr2)) * alpha
  cumsum(unsummed)
}

simData <- NULL
for (rep in 1:NCMP) {
  pa <- sample(palist, 2)  # make non-uniform TODO
  pa1 <- pa[1]
  pa2 <- pa[2]
  row <- data.frame(pa1=factor(pa1, levels=palist),
                    pa2=factor(pa2, levels=palist))
  for (cur in colnames(theta)) {
    prob <- cmp_probs(alpha, theta[pa1,cur], theta[pa2,cur], threshold1, threshold2)
    pick <- sample(c(-2,-1,0,1,2), 1, prob=softmax(prob))
    row[,cur] <- pick
  }
  simData <- rbind(simData, row)
}

rcd <- simData

spokes <- rep(NA, length(palist))
names(spokes) <- palist
for (pa1 in palist) {
    other <- c()
    for (side in 1:2) {
        col1 <- paste0('pa', side)
        col2 <- paste0('pa', 3-side)
        other <- c(other, as.character(rcd[rcd[[col1]] == pa1, col2]))
    }
    spokes[pa1] <- sum(!duplicated(other))
}

# Otherwise the priors are given too much influence
whitelist <- names(spokes)[spokes > 1]
rcd <- rcd[rcd$pa1 %in% whitelist & rcd$pa2 %in% whitelist,]

rcd[is.na(rcd)] <- 10

sim_fit <- stan(
  file = "model3.stan",
  data = list(NPA=NPA, NFACETS=NFACETS, NCMP=nrow(rcd),
              pa1=match(rcd$pa1, palist), l1=rcd$l1,
              pa2=match(rcd$pa2, palist), l2=rcd$l2,
              diff=sapply(rcd[-1:-2], as.numeric),
              loadingSign=rep(1,NFACETS)),
  # pars=c(paste0('threshold',1:2),
  #        'alpha',
  #        paste0('flow[',1:NPA,']'),
  #        paste0('flowLoading[',1:NFACETS,']')),
  pars=c('theta', paste0('flowLoading', c('1','X'))),
  include=FALSE,
  chains = 4,
  iter = 300,
  control = list(max_treedepth = 15))

#save(sim_fit, facetNames, spokes, NPA, NFACETS, file="sim3Fit3.rda")
if (0) {
    load("simFit.rda")
}

summary(summary(sim_fit)$summary[,c('Rhat', 'n_eff')])

divergent <- get_sampler_params(sim_fit, inc_warmup=FALSE)[[1]][,'divergent__']
print(sum(divergent))

neOrder <- order(summary(sim_fit)$summary[,c('n_eff')])
head(summary(sim_fit, probs=c(.25,.75))$summary[neOrder,], n=20)

#plot(sim_fit, pars=c("thetaScale"))
if (interactive()) {
  summary(sim_fit, pars=c(paste0("threshold",1:2)))$summary
  plot(sim_fit, pars=c(paste0("threshold",1:2)))
  plot(sim_fit, pars=c(paste0("alpha[",1:NFACETS,"]")))
  plot(sim_fit, pars=c(paste0("flowLoading[",1:NFACETS,"]")))
}
if (0){
  library(shinystan)
  shinystan::launch_shinystan(sim_fit)
}

df1 <- summary(sim_fit, pars=c(paste0('flow[',1:NPA,']'),
                               paste0('flowLoading[',1:NFACETS,"]")), probs=.5)$summary
cor(c(flowScore, flowLoading), df1[,'mean'])

df1 <- summary(sim_fit, pars=c(paste0('flow[',1:NPA,']')), probs=.5)$summary
cor(c(flowScore), df1[,'mean'])
