source("modelUtil.R")

load(paste0(outputDir(), "fit2t5.rda"))  # factor model

facetNames <- extractFacetNames(rcd)
palist <- extractPalist(rcd)

tar <- extract(fit2t5, pars=paste0('flow[',match(c('running','martial arts'), palist),']'), permuted=FALSE)
cmp1 <- c(tar[,,1]) - c(tar[,,2])
runningVsMartialArtsP <- max(sum(cmp1 < 0) / length(cmp1), 1/length(cmp1))
runningVsMartialArts <- quantile(cmp1, c(.025,.975))

loadings <- summary(fit2t5, pars=c("flowLoadings"), probs=c(.025,.975))$summary
rownames(loadings) <- facetNames

rawLoadings <- extract(fit2t5, pars=c("flowLoadings"))[[1]]
colnames(rawLoadings) <- facetNames
names(dimnames(rawLoadings)) <- c('iteration', 'facet')

flow <- summary(fit2t5, pars=c("flow"), probs=c(.025,.975))$summary
rownames(flow) <- palist
flow <- cbind(flow, ss=calcSampleSize(rcd))
flow <- cbind(flow, index=1:nrow(flow))
flow <- flow[order(flow[,'mean']),]

rawFlow <- sapply(extract(fit2t5, pars=paste0('flow[', flow[,'index'], ']')), function(x) c(x))
colnames(rawFlow) <- rownames(flow)
names(dimnames(rawFlow)) <- c('iteration', 'facet')

rawLoadings <- rawLoadings[1:500,]
rawFlow <- rawFlow[1:500,]

df <- summary(fit2t5, pars=c("sigma"), probs=c())$summary
sigma <- df[,'mean']

df <- summary(fit2t5, pars=c("theta"), probs=c())$summary
tar <- array(df[,'mean'], dim=c(length(facetNames), length(palist)))
dimnames(tar) <- list(facetNames, palist)
  
itemInfo <- data.frame('$\\hat\\sigma_j$'=c(sigma),
  '$\\max(\\hat\\theta_j)-\\min(\\hat\\theta_j)$'=c(diff(apply(tar, 1, range))),
  '$\\hat\\lambda_j$'=c(loadings[,'mean']),
  row.names=facetNames, check.names=FALSE)
itemInfo <- itemInfo[order(-itemInfo[,2]),]

save(runningVsMartialArts, runningVsMartialArtsP, itemInfo, loadings, rawLoadings, flow, rawFlow, file="genFlowData.rda")

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
