source("modelUtil.R")

load(paste0(outputDir(), "fit2t5.rda"))  # factor model

facetNames <- extractFacetNames(rcd)

loadings <- summary(fit2t5, pars=c("flowLoadings"), probs=c(.025,.975))$summary
rownames(loadings) <- facetNames

rawLoadings <- extract(fit2t5, pars=c("flowLoadings"))[[1]]
colnames(rawLoadings) <- facetNames
names(dimnames(rawLoadings)) <- c('iteration', 'facet')

palist <- extractPalist(rcd)

minSampleSize <- 10

flow <- summary(fit2t5, pars=c("flow"), probs=c(.025,.975))$summary
rownames(flow) <- palist
flow <- cbind(flow, ss=calcSampleSize(rcd))
flow <- cbind(flow, index=1:nrow(flow))
flow <- flow[order(flow[,'mean']),]
flow <- flow[flow[,'ss'] > minSampleSize,]

rawFlow <- sapply(extract(fit2t5, pars=paste0('flow[', flow[,'index'], ']')), function(x) c(x))
colnames(rawFlow) <- rownames(flow)
names(dimnames(rawFlow)) <- c('iteration', 'facet')

rawLoadings <- rawLoadings[1:500,]
rawFlow <- rawFlow[1:500,]

save(loadings, rawLoadings, flow, rawFlow, minSampleSize, file="genFlowData.rda")

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