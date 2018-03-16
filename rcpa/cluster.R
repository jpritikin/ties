# saturated covariance matrix
# 1 set of thresholds vs separate thresholds for every facet

library(qgraph)
source("modelUtil.R")

load(paste0(outputDir(), "fit2t1.rda"))

if (0){
  library(shinystan)
  shinystan::launch_shinystan(fit2t1)
}

estimator <- 'mean'
facetNames <- extractFacetNames(rcd)

df <- summary(fit2t1, pars=c("thetaCor"), probs=c(.975,.025))$summary
#df[sign(df[,'95%']) != sign(df[,'5%']), estimator] <- 0
tc <- matrix(df[,estimator], length(facetNames), length(facetNames))
dimnames(tc) <- list(facetNames, facetNames)

ss <- calcSampleSize(rcd)

save(ss, tc, file="cluster.rda")

# exclude: spont goal1 feedback1 control waiting

q()

corGraph <- qgraph(tc, layout = "spring", graph = "cor",
                   legend.cex = 0.3,
                   cut = 0.3, maximum = 1, minimum = 0, esize = 20,
                   vsize = 5, repulsion = 0.8)

pcorGraph <- qgraph(tc, layout = corGraph$layout, graph = "pcor",
                    legend.cex = 0.3, 
                    cut = 0.1, maximum = 1, minimum = 0, esize = 20,
                    vsize = 5)

optGraph <- findGraph(tc, nrow(rcd), type = "pcor")
optimalGraph <- qgraph(optGraph, layout = corGraph$layout,
                       legend.cex = 0.3, 
                       cut = 0.1, maximum = 1, minimum = 0, esize = 20,
                       vsize = 5)

glassoGraph <- qgraph(tc, layout = corGraph$layout, 
                      graph = "glasso", sampleSize = nrow(rcd),
                      legend.cex = 0.3, 
                      cut = 0.1, maximum = 1, minimum = 0, esize = 20,
                      vsize = 5)

centralityPlot(
  list(saturated = corGraph,
       modelSearch = optimalGraph,
       glasso = glassoGraph)
)
clusteringPlot(
  list(saturated = corGraph,
       modelSearch = optimalGraph,
       glasso = glassoGraph)
)

factanal(covmat=tc, factors=1)

#df <- summary(fit2t1, pars=c("theta"), probs=.5)$summary
#tar <- array(df[,estimator], dim=c(NFACETS, NPA))

if (0) {
  library(shinystan)
  launch_shinystan(fit2t1)
}

if (0) {
  char <- tar
  rownames(char) <- facetNames
  colnames(char) <- palist
  pc <- prcomp(t(char), retx=TRUE)
  df <- as.data.frame(pc$x[,1:2])
  df$name <- rownames(pc$x)
  ggplot(df) + geom_point(aes(x=PC1,y=PC2)) +
    geom_text(aes(x=PC1,y=PC2,label=name), nudge_y=-.05)
  
  sort(abs(pc$rotation[,1] - pc$rotation[,2]))
  
  library(rgl)
  plot3d(pc$x[,1:3], col = rainbow(nrow(pc$x)))
}

