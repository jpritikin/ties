library(rstan)
library(jsonlite)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rcd <- read.csv("rawData.csv")
rcd <- rcd[,-match(c('recno'), colnames(rcd))]

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

N <- sum(!is.na(rcd[-1:-4]))
colSums(is.na(rcd[-1:-4]))  # seems fairly uniform

rcd[is.na(rcd)] <- 10

sim_fit <- stan(file = "model1.stan",
                data = list(NPA=NPA, NFACETS=NFACETS, NCMP=nrow(rcd), N=N,
                            pa1=match(rcd$pa1, palist),
                            pa2=match(rcd$pa2, palist),
                            diff=sapply(rcd[-1:-4], as.numeric)),
                chains = 6,
                iter = 500,
                control = list(max_treedepth = 15))

facetNames <- colnames(rcd[-1:-4])

save(sim_fit, facetNames, rcd, spokes, NPA, NFACETS, whitelist, file="/tmp/simFit1.rda")
if (0) {
    load("/tmp/simFit1.rda")
}

divergent <- get_sampler_params(sim_fit, inc_warmup=FALSE)[[1]][,'divergent__']
print(sum(divergent))

neOrder <- order(summary(sim_fit)$summary[,c('n_eff')])
head(summary(sim_fit, probs=.5)$summary[neOrder,], n=20)

estimator <- 'mean'
df <- summary(sim_fit, pars=c("alpha"), probs=.5)$summary
alpha <- matrix(df[,estimator], nrow=1,
                dimnames=list(NULL, facetNames))
print(alpha)

df <- summary(sim_fit, pars=c("theta"), probs=.5)$summary
tar <- array(df[,estimator], dim=c(NFACETS, length(whitelist)))

# Practically impossible to overcome prior mean at zero
hist(tar, breaks=100)
