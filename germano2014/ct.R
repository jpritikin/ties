library(ctsem)

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

if (0) {
  # a simple-minded approach
  summary(lm(event2 ~ barrier1 + training1 + event1, ctdat))
  summary(lm(event3 ~ barrier2 + training2 + event2, ctdat))
}

n.latent <- length(manifests)
n.manifest <- length(manifests)

model <- ctModel(basemodel=NA, n.manifest, n.latent, Tpoints=3,
               LAMBDA=diag(3), #manifest to latent loading
               PHIT1="auto",
               latentM1="auto",
               manifestM="auto",
               THETA="auto",
               DRIFT="auto",
               PRITRAIT=NULL)

mxOption(NULL, "Default optimizer", "NPSOL")
mxOption(NULL, "Calculate Hessian", "No")
fits <- ctFit(ctdat, model, D=0, reasonable=T, initialconstraints=T)
fit = fits[[1]]

if (0) {
  plot(fits)
  compareResiduals(fit, 3,3, oldomx=FALSE, nopause=F)
}
