options(stringsAsFactors=FALSE)

source("modelUtil.R")

load(paste0(outputDir(), "fitsfp.rda"))

facetNames <- extractFacetNames(rcd)
palist <- extractPalist(rcd)

df <- summary(fitsfp, pars=c("sigma"), probs=c())$summary
sigma <- df[,'mean']
names(sigma) <- facetNames

df <- summary(fitsfp, pars=c("alpha", paste0("threshold",1:2)), probs=c())$summary
alphaOrig <- df['alpha', 'mean']

df <- summary(fitsfp, pars='flowLoadings', probs=c())$summary
loadings <- df[,'mean']
names(loadings) <- facetNames

simFlow  <- as.matrix(read.csv("sim6Flow.csv", row.names=1))
simTheta <- as.matrix(read.csv("sim6Theta.csv", row.names=1))
simThr <- as.matrix(read.csv("sim6Thresh.csv", row.names=1))

load(paste0(outputDir(), "fits6fp.rda"))

df <- summary(fits6fp, pars=c("sigma"), probs=c())$summary
estSigma <- matrix(df[,'mean'], length(facetNames), 1,
  dimnames= list(facetNames, c()))

df <- summary(fits6fp, pars="alpha", probs=c())$summary
alpha <- df[,'mean']

df <- summary(fits6fp, pars=c("threshold1"), probs=c())$summary
th1 <- df[,'mean']
df <- summary(fits6fp, pars=c("threshold2"), probs=c())$summary
th2 <- df[,'mean']

df <- summary(fits6fp, pars=c("flowLoadings"), probs=c())$summary
estLoadings <- df[,'mean']

df <- summary(fits6fp, pars='flow', probs=c())$summary
estFlow <- df[,'mean']

df <- summary(fits6fp, pars=c("theta"), probs=c())$summary
estTheta <- t(array(df[,'mean'], dim=c(length(facetNames), length(palist))))
dimnames(estTheta) <- list(palist, facetNames)

simResult <- rbind(
  data.frame(par='$\\alpha$', true=alphaOrig, recovered=alpha),
  data.frame(par='$\\tau$', true=c(simThr), recovered=c(th1, th2)),
  data.frame(par='$\\sigma$', true=sigma, recovered=estSigma),
  data.frame(par='$\\lambda$', true=loadings, recovered=estLoadings),
  data.frame(par='$\\pi$', true=c(simFlow), recovered=c(estFlow)))

simResultTheta <- data.frame(par='$\\theta$', true=c(simTheta), recovered=c(estTheta))

simResult$par = ordered(simResult$par, levels=unique(simResult$par))

cor(simResult$true, simResult$recovered)
cor(simResultTheta$true, simResultTheta$recovered)

save(simResult, simResultTheta, file="checkSim6.rda")

q()

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
