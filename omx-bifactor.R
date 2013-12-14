library(rpf)
library(OpenMx)
source('irtplot.R')

load("espt.rda")

gpcm <- function(outcomes) {
  rpf.nrm(outcomes, factors=2)
  #   rpf.nrm(outcomes, T.c=diag(outcomes-1))
}

check.item.names <- function(item.names) {  
  missing <- item.names[is.na(match(item.names, colnames(espt)))]
  if (length(missing)) stop(paste("Columns missing:", missing))
  item.names
}

create.spec <- function(item.names) {
  spec <- list()
  for (n in 1:length(item.names)) {
    lev <- length(levels(espt[[ item.names[n] ]]))
    if (lev == 2) {
      spec[n] <- rpf.grm(outcomes=2, factors=2)
    } else {
      spec[n] <- gpcm(lev)
    }
  }
  names(spec) <- item.names
  spec
}

set.starting <- function(ispec, ip.mat) {
  colnames(ip.mat@free) <- names(ispec)
  colnames(ip.mat@values) <- names(ispec)
  colnames(ip.mat@labels) <- names(ispec)
  for (ix in 1:length(ispec)) {
    i <- rpf.paramInfo(ispec[[ix]])
    ptype <- unlist(i)[seq(match('type', rownames(i)),length(i),length(rownames(i)))]
    ip.mat@values[which(ptype==1), ix] <- 1
    ip.mat@values[which(ptype==2), ix] <- 0
    ip.mat@free[which(ptype==2), ix] <- TRUE
  }
  ip.mat
}

set.nominal.rank <- function(ip.mat, name, a, c) {
  spec1 <- spec[[name]]
  if (is.null(spec1)) {
    return(ip.mat)
  }
  thresh <- spec1@outcomes-1
  free <- rep(FALSE, spec1@factors + 2 * thresh)
  base <- spec1@factors+1
  free[base:(base + thresh * a - 1)] <- TRUE
  base <- base + thresh
  free[base:(base + thresh * c - 1)] <- TRUE
  ip.mat@free[1:length(free),name] <- free
  ip.mat
}

# 'msEvery'  possibly too vague, better measured by other items
#     # too much missingness, revisit later
# 'msAllow',                  # drop this or msCause, need larger sample
# 'msDescarte'               # poor performer, revisit later
# 'msCause0'    # redundent
# 'msCause', 'msTeach', 'msTrainTeach' -- replace with causeTeach testlet
# 'causeTeach'                # testlet item, too much missingness
# 'durationCharacter' ??
# 'msAny'  # weak item
# 'skipExp'   # redundent

# msFreq vs freqCause ?
inames <- check.item.names(c('wantLearn', "msAfraid", "msEmo","msLife",
                             "msEffort", "msFast", "msDescarte", "msIdentity"))
enames <- check.item.names(c('msNotion', "msAny", 'skipInt', "maxDuration",
                             "msYearn", "msMet", "msEnv", "msShared",
                             "msCause", "msAllow", "msTeach","msTrainTeach"))

ispec <- create.spec(inames)
espec <- create.spec(enames)

when <- strptime(espt[['instrument']], "%Y-%m-%d")
revision1 <- strptime("2013-02-11", "%Y-%m-%d")
ver.mask <- difftime(when, revision1) > 0
manocha2013.mask <- (espt$time == 2 | (espt$time == 1 & !(espt$id %in% espt[espt$time == 2, 'id'])))
ver.mask[espt$wave == "manocha2013"] <- manocha2013.mask[espt$wave == "manocha2013"]

idata <- espt[ver.mask, inames]
sapply(idata, function(r) sum(!is.na(r)))  # per item sample size
edata <- espt[ver.mask, enames]
sapply(edata, function(r) sum(!is.na(r)))  # per item sample size

design <- matrix(c(1L,NA), nrow=2, ncol=length(ispec))
colnames(design) <- names(ispec)
design[2,c('msAfraid','msIdentity','wantLearn')] <- 2L
design[2,c('msFast','msEffort', 'msLife')] <- 3L

is.true <- function(v) v & !is.na(v)

ip.mat <- mxMatrix(name="ItemParam", nrow=max(sapply(ispec, rpf.numParam)),
                   ncol=length(ispec), free=FALSE)
ip.mat <- set.starting(ispec, ip.mat)
ip.mat@free[1:2,] <- !is.na(design)
ip.mat@labels[1,] <- "slope"
#ip.mat@labels[2, is.true(design[2,]==2L)] <- "left"
#ip.mat@labels[2, is.true(design[2,]==3L)] <- "right"
ip.mat@values[!ip.mat@free] <- 0
ip.mat@values[3,] <- 1

latents <- max(design, na.rm=TRUE)
m.mat <- mxMatrix(name="mean", nrow=1, ncol=latents, values=rep(0, latents), free=FALSE)
cov.mat <- mxMatrix("Symm", name="cov", nrow=latents, ncol=latents, values=diag(latents),
                    free=FALSE)

im <- mxModel(model="interest",
              m.mat, cov.mat, ip.mat,
              mxData(observed=idata, type="raw"),
              mxExpectationBA81(mean="mean", cov="cov",
                                ItemSpec=ispec, ItemParam="ItemParam", design=design,
                                qpoints=21, qwidth=5, scores="full"),
              mxFitFunctionML(),
              mxComputeEM('expectation',
                          mxComputeNewtonRaphson(free.set='ItemParam'),
                          mxComputeOnce('fitfunction', free.set=c("mean", "cov"), fit=TRUE),
                          verbose=2L))

im.est <- mxRun(im)
print(im.est@matrices$ItemParam@values[1:2,])

igrp <- list(spec=ispec,
             param=im.est@matrices$ItemParam@values,
             design=design,
             mean=im.est@matrices$mean@values,
             cov=im.est@matrices$cov@values,
             scores=im.est@expectation@scores.out,
             data=idata)

grp2 <- unpack.2tier(igrp)
got <- chen.thissen.1997(grp2, qwidth=5, qpoints=21)
got$pval

booklet(function(item) {
  basis <- c(1, rep(0, length(grp2$mean)-1))
  rpf.plot(grp2, item, width=4, data.bins=12, basis=basis, factor=2)
}, inames, output="interest.pdf")

if (0) {
  fixed.ip <- m2.est@matrices$ItemParam
  fixed.ip@free[,] <- FALSE
  eap <- mxModel(m2, fixed.ip, mxData(observed=espt[espt$wave == "manocha2013", item.names], type="raw"),
                 mxComputeOnce('expectation', context='EM'))
  eap.est <- mxRun(eap)
  manocha2013sc <- eap.est@expectation@scores.out[,1:2]
  save(manocha2013sc, file="manocha2013sc.rda")
}

if (0) {
  booklet(function(item) {
    rpf.plot(grp, item, width=4, data.bins=12, basis=c(0,1), factor=2)
  }, item.names[grp$param[2,] > 0], output="experience.pdf")
}

if (0) {
  map1 <- item.map(grp, 1)
  ggplot(map1, aes(x=score, y=item.name, label=outcome)) + geom_text()
  map2 <- item.map(grp, 2)
  ggplot(map2, aes(x=score, y=item.name, label=outcome)) + geom_text()
}

if (0) {
  got1 <- chen.thissen.1997(grp, data, item.names[m2.est@matrices$ItemParam@values[1,] != 0])
  got2 <- chen.thissen.1997(grp, data, item.names[m2.est@matrices$ItemParam@values[2,] != 0])
  
  c(max(got1$pval[!is.na(got1$pval)]), max(got2$pval[!is.na(got2$pval)]))
}

if (0) {
  # try to figure out ethical items
  scores <- m2.est@expectation@scores.out
  espt$interest <- scores[,1]
  espt$experience <- scores[,2]
  got <- list()
  for (item in c("ethical")) {
    for (l in levels(espt[[item]])) {
      got[[paste(item, l, sep="=")]] <- c(mean(espt[espt[[item]] == l, 'interest'], na.rm=TRUE),
                                          mean(espt[espt[[item]] == l, 'experience'], na.rm=TRUE))
    }
  }
  ethical <- do.call(rbind, got)
  colnames(ethical) <- c("interest","experience")
  ethical[order(ethical[,"experience"]),]
}

if (0) {
  library(ggplot2)
  library(stringr)
  scores <- m2.est@expectation@scores.out
  espt$interest <- scores[,1]
  espt$experience <- scores[,2]
  ggplot(subset(espt, !is.na(str_match(espt$wave, "^germano"))),
         aes(interest, experience, color=wave, group=email)) +
    geom_point(size=1) + geom_line(size=.1) + coord_fixed()
}

#items <- t(m2@matrices$ItemParam@values)
#rownames(items) <- item.names
#espt[mask, "score"] <- m2@expectation@scores.out[,1]
#espt[mask, "se"] <- m2@expectation@scores.out[,2]

if (0) {
  sc <- m2.est@expectation@scores.out
  for (l in 1:3) {
    mask <- espt$durationCharacter==levels(espt$durationCharacter)[l]
    mask <- !is.na(mask) & mask
    print(paste(l, mean(sc[mask,1]), mean(sc[mask,2])))
  }
}

if (0) {
  v1 <- got$detail$"msTrainTeach:msTeach"
  chisq(v1$observed, v1$expected)
  ptw2011.gof.test(v1$observed, v1$expected)
}
