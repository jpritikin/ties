library(mvtnorm)
source("modelUtil.R")

load(paste0(outputDir(), "fitssp.rda"))
fit <- fitssp

facetNames <- extractFacetNames(rcd)

df <- summary(fit, pars=c("thetaCor"), probs=c(.975,.025))$summary
# Could cause non-positive definiteness
#df[sign(df[,'97.5%']) != sign(df[,'2.5%']), estimator] <- 0
tc <- matrix(df[,'mean'], length(facetNames), length(facetNames),
  dimnames= list(facetNames, facetNames))

df <- summary(fit, pars=c("sigma"), probs=c())$summary
sigma <- df[,'mean']
names(sigma) <- facetNames

df <- summary(fit, pars=c("alpha"), probs=c())$summary
alpha <- df[,'mean']

df <- summary(fit, pars=paste0("threshold1"), probs=c())$summary
origTh1 <- df[,'mean']
th1 <- rnorm(length(origTh1), mean(origTh1), sd(origTh1))
names(th1) <- facetNames

df <- summary(fit, pars=paste0("threshold2"), probs=c())$summary
origTh2 <- df[,'mean']
th2 <- rnorm(length(origTh2), mean(origTh2), sd(origTh2))
names(th2) <- facetNames

rcd[,facetNames] <- NA

palist <- extractPalist(rcd)

theta <- rmvnorm(length(palist), sigma=diag(sigma) %*% tc %*% diag(sigma))
colnames(theta) <- facetNames
rownames(theta) <- palist

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

write.csv(rcd, "sim5Data.csv", row.names=FALSE)
write.csv(theta, "sim5Theta.csv")
write.csv(cbind(th1,th2), "sim5Thresh.csv")
