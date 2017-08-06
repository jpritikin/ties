options(width=120)
library(rstan)
library(jsonlite)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rcd <- read.csv("rawData.csv")
rcd <- rcd[,-match(c('recno'), colnames(rcd))] #, paste0('injury',1:2)

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

sim_fit <- stan(
  file = "model3.stan",
  data = list(NPA=length(whitelist), NFACETS=NFACETS, NCMP=nrow(rcd),
              pa1=match(rcd$pa1, whitelist),
              pa2=match(rcd$pa2, whitelist),
              diff=sapply(rcd[-1:-4], as.numeric)),
  pars=c('rawFlow', 'rawTheta', 'rawLoadings'),
  include=FALSE,
  chains = 6,
  iter = 1000,
#  verbose=TRUE,
  control = list(max_treedepth = 15))

save(sim_fit, facetNames, spokes, NPA, NFACETS, whitelist, file="simFit3.rda")
if (0) {
    load("simFit3.rda")
}

summary(summary(sim_fit)$summary[,c('Rhat', 'n_eff')])

divergent <- get_sampler_params(sim_fit, inc_warmup=FALSE)[[1]][,'divergent__']
print(sum(divergent))

neOrder <- order(summary(sim_fit)$summary[,c('n_eff')])
head(summary(sim_fit, probs=c(.25,.75))$summary[neOrder,], n=20)

#plot(sim_fit, pars=c("thetaScale"))
if (interactive()) {
  summary(sim_fit, pars=c(paste0("threshold",1:2)))$summary
  plot(sim_fit, pars=c(paste0("threshold",1:2)))
  plot(sim_fit, pars=c(paste0("alpha[",1:NFACETS,"]")))
  plot(sim_fit, pars=c(paste0("flowLoadings[",1:NFACETS,"]")))
}
if (0){
  library(shinystan)
  shinystan::launch_shinystan(sim_fit)
}

# check whether to flip loading sign
fl <- as.data.frame(summary(sim_fit, pars=c("flowLoadings"), probs=.5)$summary)
fl$index <- 1:nrow(fl)
rownames(fl) <- facetNames
fl[order(fl[,'mean']),]

df <- summary(sim_fit, pars=c("alpha","theta"), probs=.5)$summary
summary(df[,'mean'] - df[,'50%'])
estimator <- 'mean'

df <- summary(sim_fit, pars=c(paste0("alpha[",1:NFACETS,']')), probs=.5)$summary
alpha <- matrix(df[,estimator], nrow=1,
                dimnames=list(NULL, facetNames))
print(alpha)

flow <- summary(sim_fit, pars=c("flow"), probs=.5)$summary

df <- summary(sim_fit, pars=c("theta"), probs=.5)$summary
tar <- array(df[,estimator], dim=c(NFACETS, length(whitelist)))
if (nrow(df) != prod(dim(tar))) stop("mismatch")
tar <- rbind(tar, flow[, estimator])

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

sampleSize <- spokes[whitelist]

cat(paste("var RCPA_DATA=",
          toJSON(tar, matrix="columnmajor", digits=3),
          ";\nvar RCPA_FACETS=",
          toJSON(c(facetNames,'flow')),
          ";\nvar RCPA_FACET_ALPHA=",
          toJSON(c(alpha[1,],0)),
          ";\nvar RCPA_PA=",
          toJSON(whitelist),
          ";\nvar RCPA_PA_SAMPLESIZE=",
          toJSON(sampleSize),
          ";"),
    file="pa-browser/rcpa-data.js", fill=TRUE)

