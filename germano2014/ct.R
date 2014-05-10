source("CT_SEM.R")

w1 <- read.table("prep1.csv", header=TRUE, stringsAsFactors=FALSE)
w2 <- read.table("prep2.csv", header=TRUE, stringsAsFactors=FALSE)
w3 <- read.table("prep3.csv", header=TRUE, stringsAsFactors=FALSE)

map2 <- match(w1$id, w2$id)
map3 <- match(w1$id, w3$id)

w1end <- strptime(w1$end, "%m/%d/%Y %H:%M:%S")
w2end <- strptime(w2$end, "%m/%d/%Y %H:%M:%S")
w3end <- strptime(w3$end, "%m/%d/%Y %H:%M:%S")

manifests <- colnames(w1)[15:17]  # 8:17

replaceNA <- function (v, with=-1) {
  v[is.na(v)] <- with
  v
}

ctdat <- cbind(w1[,manifests], w2[map2,manifests], w3[map3,manifests],
               replaceNA(as.numeric(w2end[map2] - w1end)),
               replaceNA(as.numeric(w3end[map3] - w1end)))

rownames(ctdat) <- w1$id
colnames(ctdat) <- c(paste(manifests, 1, sep=""), paste(manifests, 2, sep=""), paste(manifests, 3, sep=""),
                     paste("t", 2:3, sep=""))

n.latent <- length(manifests)
n.manifest <- length(manifests)

PHI1 <- cov(w1[,manifests,drop=FALSE], use="pairwise.complete.obs")

latentM1  <- matrix(apply(w1[,manifests, drop=FALSE],2,mean, na.rm=TRUE), nrow=n.latent, ncol=1)

manifestM <- matrix(0, nrow=n.manifest, ncol=1)

G    	<- matrix(.1, n.latent, n.latent)
G[upper.tri(G)] <- 0

drift <- .1 - diag(n.latent)

THETA    <- matrix(0									# var/cov matrix of measurement error
                   ,nrow=n.manifest, ncol=n.manifest)

fit <- continuous_time(ctdat, Tpoints=3, n.latent, n.manifest,
                       LAMBDA=diag(3), #manifest to latent loading
                       DRIFT=drift,
                       CINT=manifestM,
                       G=G, D=5,
                       PHI1,
                       THETA=THETA, latentM1, manifestM=manifestM)

