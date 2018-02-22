library(rstan)
library(jsonlite)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rcd <- read.csv("rawData.csv")
rcd <- rcd[,-match(c('recno', paste0('injury', 1:2), paste0(c('goal','feedback'),2)), colnames(rcd))]

if (nrow(rcd) < 1) { stop("No data?") }
facetNames <- colnames(rcd[-1:-4])
NFACETS <- length(facetNames)

palist <- sort(unique(c(as.character(rcd$pa1), as.character(rcd$pa2))))
NPA <- length(palist)

spokes <- rep(NA, length(palist))
names(spokes) <- palist
for (pa1 in palist) {
    other <- c()
    for (side in 1:2) {
        col1 <- paste0('pa', side)
        col2 <- paste0('pa', 3-side)
        other <- c(other, as.character(rcd[rcd[[col1]] == pa1, col2]))
    }
    spokes[pa1] <- sum(!duplicated(other))
}

# Otherwise the priors are given too much influence
whitelist <- names(spokes)[spokes >= 3]
rcd <- rcd[rcd$pa1 %in% whitelist & rcd$pa2 %in% whitelist,]

#soloGroupList <- which(duplicated(sub(";(solo|group)$", "", palist)))

rcd[is.na(rcd)] <- 10

sim_fit <- stan(file = "model2.stan",
                data = list(NPA=NPA, NFACETS=NFACETS, NCMP=nrow(rcd),
                            pa1=match(rcd$pa1, palist),
                            pa2=match(rcd$pa2, palist),
                            diff=sapply(rcd[-1:-4], as.numeric)),
                chains = 6,
                iter = 500,
                include=FALSE,
                pars=c('thetaCorChol'),
                control = list(max_treedepth = 15))

facetNames <- colnames(rcd[-1:-4])

save(sim_fit, facetNames, rcd, spokes, NPA, NFACETS, whitelist, file="/tmp/simFit2.rda")
if (0) {
    load("/tmp/simFit2.rda")
}

divergent <- get_sampler_params(sim_fit, inc_warmup=FALSE)[[1]][,'divergent__']
print(sum(divergent))

neOrder <- order(summary(sim_fit)$summary[,c('n_eff')])
head(summary(sim_fit, probs=.5)$summary[neOrder,], n=20)

#plot(sim_fit, pars=c("thetaScale"))
if (interactive()) {
  summary(sim_fit, pars=c(paste0("threshold",1:2)))$summary
  plot(sim_fit, pars=c(paste0("threshold",1:2)))
}
if (0){
  library(shinystan)
  shinystan::launch_shinystan(sim_fit)
}

#df <- summary(sim_fit, pars=c("alpha","theta"), probs=.5)$summary
#summary(df[,'mean'] - df[,'50%'])
estimator <- 'mean'

df <- summary(sim_fit, pars=c("alpha"), probs=.5)$summary
alpha <- matrix(df[,estimator], nrow=1,
                dimnames=list(NULL, facetNames))
print(alpha)

df <- summary(sim_fit, pars=c("thetaCor"), probs=c(.95,.05))$summary
#df[sign(df[,'95%']) != sign(df[,'5%']), estimator] <- 0
tc <- matrix(df[,estimator], NFACETS, NFACETS)
dimnames(tc) <- list(facetNames, facetNames)

stop("here")

library(qgraph)
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

#df <- summary(sim_fit, pars=c("theta"), probs=.5)$summary
#tar <- array(df[,estimator], dim=c(NFACETS, NPA))

if (0) {
  library(shinystan)
  launch_shinystan(sim_fit)
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

