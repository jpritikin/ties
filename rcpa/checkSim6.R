source("modelUtil.R")

load(paste0(outputDir(), "fit2t5.rda"))

facetNames <- extractFacetNames(rcd)
palist <- extractPalist(rcd)

df <- summary(fit2t5, pars=c("sigma"), probs=c())$summary
sigma <- df[,'mean']
names(sigma) <- facetNames

df <- summary(fit2t5, pars=c("alpha", paste0("threshold",1:2)), probs=c())$summary
item <- df[,'mean']

df <- summary(fit2t5, pars='flowLoadings', probs=c())$summary
loadings <- df[,'mean']
names(loadings) <- facetNames

simFlow  <- as.matrix(read.csv("simFlow.csv", row.names=1))
simTheta <- as.matrix(read.csv("simTheta.csv", row.names=1))

rm(fit2t5)

load(paste0(outputDir(), "fit2t6.rda"))

df <- summary(fit2t6, pars=c("sigma"), probs=c())$summary
estSigma <- matrix(df[,'mean'], length(facetNames), 1,
  dimnames= list(facetNames, c()))

df <- summary(fit2t6, pars=c("alpha", paste0("threshold",1:2)), probs=c())$summary
estItem <- df[,'mean']

df <- summary(fit2t6, pars=c("flowLoadings"), probs=c())$summary
estLoadings <- df[,'mean']

df <- summary(fit2t6, pars='flow', probs=c())$summary
estFlow <- df[,'mean']

df <- summary(fit2t6, pars=c("theta"), probs=c())$summary
estTheta <- t(array(df[,'mean'], dim=c(length(facetNames), length(palist))))
dimnames(estTheta) <- list(palist, facetNames)

options(stringsAsFactors=FALSE)

simResult <- rbind(
  data.frame(par='$\\alpha$', true=item[1], recovered=estItem[1]),
  data.frame(par='$\\tau$', true=item[2:3], recovered=estItem[2:3]),
  data.frame(par='$\\sigma$', true=sigma, recovered=estSigma),
  data.frame(par='$\\lambda$', true=loadings, recovered=estLoadings),
  data.frame(par='$\\pi$', true=c(simFlow), recovered=c(estFlow)))

simResultTheta <- data.frame(par='$\\theta$', true=c(simTheta), recovered=c(estTheta))

simResult$par = ordered(simResult$par, levels=unique(simResult$par))

cor(simResult$true, simResult$recovered)

save(simResult, simResultTheta, file="checkSim6.rda")

library(ggplot2)
library(tikzDevice)

xMin <- min(c(simResult$true, simResult$recovered))
xMax <- max(c(simResult$true, simResult$recovered))

tikz(file = "checkSim6.tex", height=4, width=4)
theme_set(theme_bw())
ggplot(simResult) + geom_point(aes(true, recovered, group=par, color=par, shape=par),
  size=2) + geom_abline(slope=1, color="yellow", alpha=.5) + coord_fixed() +
  xlim(xMin, xMax) + ylim(xMin, xMax)
dev.off()
