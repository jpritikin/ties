library(mvtnorm)
source("modelUtil.R")

load(paste0(outputDir(), "fit2t2.rda"))

facetNames <- extractFacetNames(rcd)

df <- summary(fit2t2, pars=c("thetaCor"), probs=c(.975,.025))$summary
# Could cause non-positive definite problem
#df[sign(df[,'97.5%']) != sign(df[,'2.5%']), estimator] <- 0
tc <- matrix(df[,'mean'], length(facetNames), length(facetNames),
  dimnames= list(facetNames, facetNames))

df <- summary(fit2t2, pars=c("sigma"), probs=c())$summary
sigma <- df[,'mean']
names(sigma) <- facetNames

df <- summary(fit2t2, pars=c("alpha"), probs=c())$summary
alpha <- df[,'mean']

df <- summary(fit2t2, pars=paste0("threshold",1:2), probs=c())$summary
th <- df[,'mean']

rcd[,facetNames] <- NA

palist <- extractPalist(rcd)

theta <- rmvnorm(length(palist), sigma=diag(sigma) %*% tc %*% diag(sigma))
colnames(theta) <- facetNames
rownames(theta) <- palist

cmp_probs <- function(alpha, pa1, pa2, thr1, thr2) {
  diff = pa1 - pa2;
  unsummed <- c(0, diff - (thr1+thr2), diff - thr1, diff + thr1, diff + (thr1+thr2))
  cumsum(alpha * unsummed)
}

for (rx in 1:nrow(rcd)) {
  pa1 <- match(rcd[rx,'pa1'], palist)
  pa2 <- match(rcd[rx,'pa2'], palist)
  rcd[rx, facetNames] <- sapply(facetNames, function(f1) {
    prob <- cmp_probs(alpha, theta[pa1,f1], theta[pa2,f1], th[1], th[2])
    pick <- sample(c(-2,-1,0,1,2), 1, prob=softmax(prob))
  })
}

write.csv(rcd, "simData.csv", row.names=FALSE)
write.csv(theta, "simTheta.csv")
