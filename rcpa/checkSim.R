source("modelUtil.R")

load(paste0(outputDir(), "fit2t2.rda"))

estimator <- 'mean'
facetNames <- extractFacetNames(rcd)
palist <- extractPalist(rcd)

df <- summary(fit2t2, pars=c("thetaCor"), probs=c(.975,.025))$summary
#df[sign(df[,'97.5%']) != sign(df[,'2.5%']), estimator] <- 0
tc <- matrix(df[,estimator], length(facetNames), length(facetNames),
  dimnames= list(facetNames, facetNames))

rm(fit2t2)

load(paste0(outputDir(), "fit2t6.rda"))

df <- summary(fit2t6, pars=c("thetaCor"), probs=c(.975,.025))$summary
#df[sign(df[,'97.5%']) != sign(df[,'2.5%']), estimator] <- 0
sc <- matrix(df[,estimator], length(facetNames), length(facetNames),
  dimnames= list(facetNames, facetNames))

mask <- lower.tri(sc, F)
print(cor(tc[mask], sc[mask]))

df <- summary(fit2t6, pars=c("theta"), probs=c())$summary
tar <- t(array(df[,estimator], dim=c(length(facetNames), length(palist))))
dimnames(tar) <- list(palist, facetNames)

simTheta <- as.matrix(read.csv("simTheta.csv", row.names=1))
print(cor(c(tar), c(simTheta)))

pval <- ppc(fit2t6, rcd)
print(sum(pval<.05) / length(pval))
print(apply(pval, 1, function(x) sum(x<.05)))
print(apply(pval, 2, function(x) sum(x<.05)))
