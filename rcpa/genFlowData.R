source("modelUtil.R")

load(paste0(outputDir(), "fitsfp.rda"))  # factor model

fit <- fitsfp

facetNames <- extractFacetNames(rcd)
palist <- extractPalist(rcd)

tar <- extract(fit, pars=paste0('flow[',match(c('running','martial arts'), palist),']'), permuted=FALSE)
cmp1 <- c(tar[,,1]) - c(tar[,,2])
runningVsMartialArtsP <- max(sum(cmp1 < 0) / length(cmp1), 1/length(cmp1))
runningVsMartialArts <- quantile(cmp1, c(.025,.975))

tar <- extract(fit, pars=paste0('flow[',match(c('hiking','mountain biking'), palist),']'), permuted=FALSE)
cmp1 <- c(tar[,,1]) - c(tar[,,2])
hikingVsMountainBikingP <- max(sum(cmp1 < 0) / length(cmp1), 1/length(cmp1))
hikingVsMountainBiking <- quantile(cmp1, c(.025,.975))

loadings <- summary(fit, pars=c("flowLoadings"), probs=c(.025,.975))$summary
rownames(loadings) <- facetNames

rawLoadings <- extract(fit, pars=c("flowLoadings"))[[1]]
colnames(rawLoadings) <- facetNames
names(dimnames(rawLoadings)) <- c('iteration', 'facet')

flow <- summary(fit, pars=c("flow"), probs=c(.025,.975))$summary
rownames(flow) <- palist
flow <- cbind(flow, ss=calcSampleSize(rcd))
flow <- cbind(flow, index=1:nrow(flow))
flow <- flow[order(flow[,'mean']),]

largeSampleThreshold <- 25
largeSampleActivities <- rev(rownames(flow[flow[,'ss'] >= largeSampleThreshold,]))
bigDiffL <- matrix(NA, nrow=length(largeSampleActivities), ncol=length(largeSampleActivities),
  dimnames=list(largeSampleActivities,largeSampleActivities))
bigDiffU <- matrix(NA, nrow=length(largeSampleActivities), ncol=length(largeSampleActivities),
  dimnames=list(largeSampleActivities,largeSampleActivities))
for (rx in 2:length(largeSampleActivities)) {
  for (cx in 1:(rx-1)) {
    tar <- extract(fit, pars=paste0('flow[',match(c(
      largeSampleActivities[cx],
      largeSampleActivities[rx]), palist),']'), permuted=FALSE)
    cmp1 <- c(tar[,,1]) - c(tar[,,2])
    q1 <- quantile(cmp1, c(.025,.975))
    bigDiffL[rx,cx] <- q1[1]
    bigDiffU[rx,cx] <- q1[2]
  }
}
bigDiffL <- bigDiffL[-1,-ncol(bigDiffL)]
bigDiffU <- bigDiffU[-1,-ncol(bigDiffU)]

rawFlow <- sapply(extract(fit, pars=paste0('flow[', flow[,'index'], ']')), function(x) c(x))
colnames(rawFlow) <- rownames(flow)
names(dimnames(rawFlow)) <- c('iteration', 'facet')

rawLoadings <- rawLoadings[1:500,]
rawFlow <- rawFlow[1:500,]

df <- summary(fit, pars=c("sigma"), probs=c())$summary
rownames(df) <- facetNames
sigmaByItemFM <- df[,'mean']

df <- summary(fit, pars=c("theta"), probs=c())$summary
tar <- array(df[,'mean'], dim=c(length(facetNames), length(palist)))
dimnames(tar) <- list(facetNames, palist)
rangeByItemFM <- diff(apply(tar, 1, range))
  
numIterations <- length(cmp1)

df <- extract(fit, pars=c('flowLoadings', 'sigma'), permuted=FALSE)
stdP <- apply(df, 1:2, function(v) {
  std <- rep(NA, length(facetNames))
  for (fx in 1:length(facetNames)) {
    loading <- v[fx]
    sigma <- v[length(facetNames) + fx]
    itemVar <- (1 + loading^2) * sigma^2
    std[fx] <- (loading^2 * sigma^2) / itemVar
  }
  names(std) <- facetNames
  std
})
stdLoadings <- t(apply(stdP, 1, function(v) quantile(c(v), c(.025,.975))))

save(hikingVsMountainBikingP, hikingVsMountainBiking, runningVsMartialArts, runningVsMartialArtsP,
  largeSampleThreshold, largeSampleActivities, bigDiffL, bigDiffU, sigmaByItemFM, rangeByItemFM,
  numIterations, loadings, rawLoadings, flow, rawFlow, tar,
  stdLoadings, file="genFlowData.rda")

q()

# ------------------

library(reshape2)
library(ggplot2)

mdf <- data.frame(facet=ordered(rownames(loadings)), x=loadings[,'mean'])
rdf <- melt(rawLoadings)
sdf <- data.frame(facet=ordered(rownames(loadings)), x=loadings[,'2.5%'], xend=loadings[,'97.5%'])

ggplot() +   geom_vline(xintercept=0, color="green") +
  geom_jitter(data=rdf, aes(value, facet), height = 0.35, alpha=.05) +
  geom_segment(data=sdf, aes(y=facet, yend=facet, x=x, xend=xend), color="yellow") +
  geom_point(data=mdf, aes(x, facet), color="red", size=1) + ylab("facet loading")

#-#

mdf <- data.frame(facet=ordered(rownames(flow)), x=-flow[,'mean'])
rdf <- melt(rawFlow)
rdf$value <- -rdf$value
sdf <- data.frame(facet=ordered(rownames(flow)), x=-flow[,'2.5%'], xend=-flow[,'97.5%'])

ggplot() +
  geom_jitter(data=rdf, aes(value, facet), height = 0.35, alpha=.05, size=.5) +
  geom_segment(data=sdf, aes(y=facet, yend=facet, x=x, xend=xend), color="yellow") +
  geom_point(data=mdf, aes(x, facet), color="red", size=1) +
  ylab("physical activity") + xlab("latent flow score")
