# This is for a saturated covariance matrix

source("modelUtil.R")

load(paste0(outputDir(), "fitssp.rda"))

estimator <- 'mean'
facetNames <- extractFacetNames(rcd)
palist <- extractPalist(rcd)

df <- summary(fitssp, pars=c("thetaCor"), probs=c())$summary
tc <- matrix(df[,estimator], length(facetNames), length(facetNames),
  dimnames= list(facetNames, facetNames))

df <- summary(fitssp, pars=c("sigma"), probs=c())$summary
sigma1 <- matrix(df[,estimator], length(facetNames), 1,
  dimnames= list(facetNames, c()))

df <- summary(fitssp, pars=c("alpha"), probs=c())$summary
alpha1 <- df[,estimator]

load(paste0(outputDir(), "fits5sp.rda"))

df <- summary(fits5sp, pars=c("thetaCor"), probs=c())$summary
sc <- matrix(df[,estimator], length(facetNames), length(facetNames),
  dimnames= list(facetNames, facetNames))

df <- summary(fits5sp, pars=c("sigma"), probs=c())$summary
sigma2 <- matrix(df[,estimator], length(facetNames), 1,
  dimnames= list(facetNames, c()))

df <- summary(fits5sp, pars=c("alpha"), probs=c())$summary
alpha2 <- df[,estimator]

print(max(abs(alpha1 - alpha2)))
print(max(abs(sigma1 - sigma2)))
print(cor(sigma1, sigma2))

df <- summary(fits5sp, pars=c("threshold1"), probs=c())$summary
th1 <- df[,estimator]
df <- summary(fits5sp, pars=c("threshold2"), probs=c())$summary
th2 <- df[,estimator]

simThr <- as.matrix(read.csv("sim5Thresh.csv", row.names=1))

print(cor(c(th1,th2), c(simThr)))

mask <- lower.tri(sc, F)
print(cor(tc[mask], sc[mask]))
print(max(abs(tc[mask] - sc[mask])))

df <- summary(fits5sp, pars=c("theta"), probs=c())$summary
tar <- t(array(df[,estimator], dim=c(length(facetNames), length(palist))))
dimnames(tar) <- list(palist, facetNames)

simTheta <- as.matrix(read.csv("sim5Theta.csv", row.names=1))
print(cor(c(tar), c(simTheta)))
print(max(abs(c(tar) - c(simTheta))))

#pval <- ppc(fits5sp, rcd)
#print(sum(pval<.05) / length(pval))
#print(apply(pval, 1, function(x) sum(x<.05)))
#print(apply(pval, 2, function(x) sum(x<.05)))
