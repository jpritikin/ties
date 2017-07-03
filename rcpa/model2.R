library(rstan)
library(jsonlite)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

if (1) {
  rcd <- read.csv("rawData.csv")
  
  if (nrow(rcd) < 1) { stop("No data?") }
  NFACETS <- ncol(rcd) - 4L
  
  palist <- sort(unique(c(as.character(rcd$pa1), as.character(rcd$pa2))))
  NPA <- length(palist)
}
if (0) {
  softmax <- function(y) {
    exp(y) / sum(exp(y))
  }
  NCMP <- 100
  NFACETS <- 2
  fnames <- c('waiting', 'injury')
  
  palist <- c('skydiving', 'surfing', 'running', 'walking')
  NPA <- length(palist)
  
  theta <- matrix(0, nrow=NPA, ncol=NFACETS,
                   dimnames=list(palist, fnames))
  theta[,'waiting'] <- c(-1, 1, -.75, -.75)
  theta[,'injury'] <- c(1, .5, -.1, -1)
  
  alpha <- .2
  threshold1 <- .5
  threshold2 <- 1
  
  cmp_probs <- function(pa1, pa2, thr1, thr2) {
    diff = pa1 - pa2;
    unsummed <- c(0, diff - thr2, diff - thr1, diff + thr1, diff + thr2) * alpha
    cumsum(unsummed);
  }
  
  simData <- NULL
  for (rep in 1:NCMP) {
    pa <- sample(palist, 2)
    pa1 <- pa[1]
    pa2 <- pa[2]
    row <- data.frame(pa1=factor(pa1, levels=palist),
                      pa2=factor(pa2, levels=palist))
    for (cur in fnames) {
      prob <- cmp_probs(theta[pa1,cur], theta[pa2,cur], threshold1, threshold2)
      pick <- sample(c(-2,-1,0,1,2), 1, prob=softmax(prob))
      row[,cur] <- pick
    }
    simData <- rbind(simData, row)
  }
  rcd <- simData
}

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
whitelist <- names(spokes)[spokes > 1]
rcd <- rcd[rcd$pa1 %in% whitelist & rcd$pa2 %in% whitelist,]

soloGroupList <- which(duplicated(sub(";(solo|group)$", "", palist)))

rcd[is.na(rcd)] <- 10

sim_fit <- stan(file = "model2.stan",
                data = list(NPA=NPA, NFACETS=NFACETS, NCMP=nrow(rcd),
                            pa1=match(rcd$pa1, palist), l1=rcd$l1,
                            pa2=match(rcd$pa2, palist), l2=rcd$l2,
                            diff=sapply(rcd[-1:-4], as.numeric),
                            NSGP=length(soloGroupList)),
#                            soloGroupList=soloGroupList),
                chains = 7,
                iter = 500,
                control = list(max_treedepth = 15))

facetNames <- colnames(rcd[-1:-4])

save(sim_fit, facetNames, spokes, NPA, NFACETS, file="simFit.rda")
if (0) {
    load("simFit.rda")
}

summary(summary(sim_fit)$summary[,c('Rhat', 'n_eff')])

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

df <- summary(sim_fit, pars=c("alpha","theta"), probs=.5)$summary
summary(df[,'mean'] - df[,'50%'])
estimator <- 'mean'

df <- summary(sim_fit, pars=c("alpha"), probs=.5)$summary
alpha <- matrix(df[,estimator], nrow=1,
                dimnames=list(NULL, facetNames))
print(alpha)

df <- summary(sim_fit, pars=c("theta"), probs=.5)$summary
tar <- array(df[,estimator], dim=c(NFACETS, NPA))

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

mask <- spokes>3    # increase this TODO

cat(paste("var RCPA_DATA=",
          toJSON(tar[,mask], matrix="columnmajor", digits=3),
          ";\nvar RCPA_FACETS=",
          toJSON(facetNames),
          ";\nvar RCPA_FACET_ALPHA=",
          toJSON(alpha[1,]),
          ";\nvar RCPA_PA=",
          toJSON(palist[mask]),
          ";"),
    file="pa-browser/rcpa-data.js", fill=TRUE)

