library(mvtnorm)
source("modelUtil.R")

load(paste0(outputDir(), "fitsfp.rda"))
fit <- fitsfp

facetNames <- extractFacetNames(rcd)

df <- summary(fit, pars=c("sigma"), probs=c())$summary
sigma <- df[,'mean']
names(sigma) <- facetNames

df <- summary(fit, pars=c("alpha"), probs=c())$summary
alpha <- df['alpha','mean']

df <- summary(fit, pars=paste0("threshold1"), probs=c())$summary
origTh1 <- df[,'mean']
th1 <- rnorm(length(origTh1), mean(origTh1), sd(origTh1))
names(th1) <- facetNames

df <- summary(fit, pars=paste0("threshold2"), probs=c())$summary
origTh2 <- df[,'mean']
th2 <- rnorm(length(origTh2), mean(origTh2), sd(origTh2))
names(th2) <- facetNames

df <- summary(fit, pars='flowLoadings', probs=c())$summary
loadings <- df[,'mean']
names(loadings) <- facetNames

rcd[,facetNames] <- NA

palist <- extractPalist(rcd)

flowScore <- matrix(rnorm(length(palist)), ncol=1,
  dimnames=list(palist, 'flow'))

theta <- flowScore %*% loadings + matrix(rnorm(length(palist) * length(facetNames)),
  nrow=length(palist), ncol=length(facetNames))

theta <- theta * matrix(sigma, byrow=T, nrow=length(palist), ncol=length(facetNames))

dimnames(theta) <- list(palist, facetNames)

cmp_probs <- function(alpha, pa1, pa2, thr1, thr2) {
  diff = alpha * (pa1 - pa2);
  unsummed <- c(0, diff - (thr1+thr2), diff - thr1, diff + thr1, diff + (thr1+thr2))
  cumsum(unsummed)
}

for (rx in 1:nrow(rcd)) {
  pa1 <- match(rcd[rx,'pa1'], palist)
  pa2 <- match(rcd[rx,'pa2'], palist)
  rcd[rx, facetNames] <- sapply(facetNames, function(f1) {
    prob <- cmp_probs(alpha, theta[pa1,f1], theta[pa2,f1], th1[f1], th2[f1])
    pick <- sample(c(-2,-1,0,1,2), 1, prob=softmax(prob))
  })
}

write.csv(rcd, "sim6Data.csv", row.names=FALSE)
write.csv(theta, "sim6Theta.csv")
write.csv(flowScore, "sim6Flow.csv")
write.csv(cbind(th1,th2), "sim6Thresh.csv")
