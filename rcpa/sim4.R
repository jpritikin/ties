# single latent score per physical activity

library(rstan)
library(jsonlite)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rcd <- read.csv("rawData.csv")

if (nrow(rcd) < 1) { stop("No data?") }
NFACETS <- ncol(rcd) - 4L

palist <- sort(unique(c(as.character(rcd$pa1), as.character(rcd$pa2))))
palist <- palist[sample.int(length(palist),50)]
NPA <- length(palist)

softmax <- function(y) {
  exp(y) / sum(exp(y))
}

# 1500 for 5 is good
# 50 for 8,15,50 is okay
NCMP <- 50 * NPA * (NPA-1)/2  # figure out good size

flowScore <- rnorm(NPA)
names(flowScore) <- palist

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
                    pa2=factor(pa2, levels=palist),
                    pick=NA)
  prob <- cmp_probs(alpha, flowScore[pa1], flowScore[pa2], threshold1, threshold2)
  pick <- sample(c(-2,-1,0,1,2), 1, prob=softmax(prob))
  row[,'pick'] <- pick
  simData <- rbind(simData, row)
}

sim_fit <- stan(
  file = "model4.stan",
  data = list(NPA=NPA, NCMP=nrow(simData),
              pa1=match(simData$pa1, palist),
              pa2=match(simData$pa2, palist),
              diff=as.numeric(simData$pick)),
  chains = 4,
  iter = 400,
  verbose=TRUE,
  control = list(max_treedepth = 15))

save(sim_fit, NPA, file="sim4Fit.rda")
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
  plot(sim_fit, pars=c("alpha", paste0("threshold",1:2)))
#  plot(sim_fit, pars=c(paste0('flow[',1:NPA,']')))
}
if (0){
  library(shinystan)
  shinystan::launch_shinystan(sim_fit)
}

df <- summary(sim_fit, pars=c("alpha",paste0("threshold",1:2),
                              paste0('flow[',1:NPA,']')),
              probs=.5)$summary
cor(df[,'mean'], c(alpha, threshold1, threshold2, flowScore))
#cor(df[,'50%'], c(alpha, threshold1, threshold2, flowScore))
df[,'mean'] - c(alpha, threshold1, threshold2, flowScore)
