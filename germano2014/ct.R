#options(error = browser)
library(ctsem)
#setwd("~/2012/sy/germano2014")

w1 <- read.table("prep1.csv", header=TRUE, stringsAsFactors=FALSE)
w2 <- read.table("prep2.csv", header=TRUE, stringsAsFactors=FALSE)
w3 <- read.table("prep3.csv", header=TRUE, stringsAsFactors=FALSE)

w1end <- strptime(w1$end, "%m/%d/%Y %H:%M:%S")
w2end <- strptime(w2$end, "%m/%d/%Y %H:%M:%S")
w3end <- strptime(w3$end, "%m/%d/%Y %H:%M:%S")

#manifests <- colnames(w1)[c(15,17)]  # 8:17
manifests <- colnames(w1)[c(8,14,15,17)]  # 8:17

zeroTraining <- min(w1[,"training"], na.rm=TRUE)  # place "no training" at 0
train1 <- w1[,"training"] - zeroTraining
train1[is.na(train1)] <- 0   # if we know nothing, assume they start with 0
train2 <- w2[,"training"] - zeroTraining
train3 <- w3[,"training"] - zeroTraining

trainAll <- cbind(w1$id, train1, train2, train3)

spacer <- cbind(NA)
for (rep in 2:length(manifests)) spacer <- cbind(spacer, NA)

manifestZero <- apply(w1[,manifests], 2, min, na.rm=TRUE)
for (mx in 1:length(manifests)) {
  col <- manifests[mx]
  w1[,col] <- w1[,col] - manifestZero[col]
  w2[,col] <- w2[,col] - manifestZero[col]
  w3[,col] <- w3[,col] - manifestZero[col]
}

ctdat <- cbind(spacer, w1[,manifests],
               spacer, w2[,manifests],
               spacer, w3[,manifests],
               train1, NA,
               train2, NA,
               train3,
               -7, 0,
               NA, as.numeric(w2end - w1end),
               NA, as.numeric(w3end - w1end))

ctdat[,ncol(ctdat)-1] <- ctdat[,ncol(ctdat)] - 7
ctdat[,ncol(ctdat)-3] <- ctdat[,ncol(ctdat)-2] - 7
ctdat[,(ncol(ctdat)-5) : ncol(ctdat)] <- 7 + ctdat[,(ncol(ctdat)-5) : ncol(ctdat)]
if (0) {
  head(ctdat[,(ncol(ctdat)-5) : ncol(ctdat)])
}

rownames(ctdat) <- w1$id
colnames(ctdat) <- c(paste("prior0", 1:length(manifests)),
                     paste(manifests, 1, sep=""),
                     paste("between1", 1:length(manifests)),
                     paste(manifests, 2, sep=""),
                     paste("between2", 1:length(manifests)),
                     paste(manifests, 3, sep=""),
                     "training1", "skip1",
                     "training2", "skip2",
                     "training3",
                     paste("t", 1:6, sep=""))

if (1) {
  # exclude participants who remain at floor for event
  eventFloor <- min(ctdat[,'event1'], na.rm=TRUE)
  mask <- ctdat[,"event1"] > eventFloor | ctdat[,"event2"] > eventFloor | ctdat[,"event3"] > eventFloor
  mask <- !is.na(mask) & mask  # exclude when all occasions are NA
  ctdat <- ctdat[mask,]
}

if (0) {
  # a simple-minded approach
  summary(lm(event2 ~ barrier1 + training1 + event1, ctdat))
  summary(lm(event3 ~ barrier2 + training2 + event2, ctdat))
}

if (0) {
  ctWideNames(n.manifest=length(manifests),n.TDpred=1,Tpoints=6,n.TIpred=0)
}

ctdat<-intervalise(ctdat,Tpoints=6,n.TDpred=1,n.manifest=length(manifests),
                   flattenstarttime=TRUE, mininterval=1)

n.latent <- length(manifests)
n.manifest <- length(manifests)

model <- ctModel(n.manifest=n.manifest, n.latent=n.latent, Tpoints=6, n.TDpred=1,
               LAMBDA=diag(length(manifests)), #manifest to latent loading
               PHIT1="auto",
               latentM1="auto",
               manifestM="auto",
               THETA="auto",
#                 DRIFT=chardiag(paste0("F",1:3),3), #only autoregression terms
               DRIFT="auto", 
               PHITRAIT="auto")

#mxOption(NULL, "Default optimizer", "NPSOL")
mxOption(NULL, "Calculate Hessian", "No")
if (1) {
  fits <- ctFit(ctdat, model, D=0, reasonable=T, initialconstraints=T, withinconstraints=T,
                npsol=FALSE)
} else {
  fit0 <- ctFit(ctdat, model, D=0, reasonable=T, initialconstraints=T, nofit=T)
  diag(fit0$DRIFT$lbound) <- -1
  fits <- list(fit=mxRun(fit0),model=fit0)
  class(fits) <- "ctsemFit"
}
fit = fits[[1]]
print(manifests)

if (0) {
  plot(fits,max.time=60) #added max.time param to see effects a little better
  
  compareResiduals(fits, n.manifest = length(manifests), Tpoints=6, n.TDpred=1)
#  compareResiduals(fits, n.vars=2, 3, oldomx=FALSE, nopause=F)
  
  rownames(fit$TDPRED) <- manifests
  fit$TDPRED$values
  drift <- fit$DRIFT$values
  dimnames(drift) <- list(manifests, manifests)
  cint <- fit$CINT$values
  colnames(cint) <- manifests
  ctdat[6,(n.manifest+1):(2*n.manifest)]
  ctdat[6,(n.manifest+1):(2*n.manifest)] %*% t(omxExponential(drift * 30)) + cint
}

if (0) {
  #discrete model
dmodel<-larmaModel(ctdat,n.latent=3,n.manifest=3,Tpoints=3,LAMBDA=diag(3),reasonable=T,merror=T,traits=T,withinconstraints=F,betweenconstraints=F)
dfit<-mxRun(dmodel)
  summary(dfit)
  compareResiduals(dfit, 3,3, oldomx=FALSE, nopause=F)
}
