library(rstan)
library(jsonlite)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

outputDir <- function() '/scratch/rcpa/'

loadRawData <- function() {
  rcd <- read.csv("rawData-snap.csv")  # switch back to current data TODO
  ignCol <- c('recno', paste0('injury', 1:2), paste0(c('goal','feedback'),2))
  rcd <- rcd[,-match(ignCol, colnames(rcd))]
  if (nrow(rcd) < 1) { stop("No data?") }
  rcd
}

loadWhitelistRawData <- function() {
  rcd <- loadRawData()
  whitelist <- getWhiteList(rcd)
  rcd[rcd$pa1 %in% whitelist & rcd$pa2 %in% whitelist,]
}

loadSingleFactorData <- function() {
  rcd <- loadWhitelistRawData()
  exclude <- c("spont", "goal1", "feedback1", "chatter", "control", "waiting")
  rcd <- rcd[,-match(exclude, colnames(rcd))]
  rcd
}

extractFacetNames <- function(rcd) {
  excludeCols <- -match(c(paste0('pa', 1:2), paste0('l', 1:2)), colnames(rcd))
  colnames(rcd)[excludeCols]
}

extractPalist <- function(rcd) {
  unique(sort(c(as.character(rcd$pa1), as.character(rcd$pa2))))
}

calcSpokes <- function(rcd) {
  palist1 <- extractPalist(rcd)
  spokes <- rep(NA, length(palist1))
  names(spokes) <- palist1
  for (pa1 in palist1) {
    other <- c()
    for (side in 1:2) {
      col1 <- paste0('pa', side)
      col2 <- paste0('pa', 3 - side)
      other <- c(other, as.character(rcd[rcd[[col1]] == pa1, col2]))
    }
    spokes[pa1] <- sum(!duplicated(other))
  }
  spokes
}

calcSampleSize <- function(rcd) {
  palist1 <- extractPalist(rcd)
  spokes <- rep(NA, length(palist1))
  names(spokes) <- palist1
  for (pa1 in palist1) {
    other <- c()
    for (side in 1:2) {
      col1 <- paste0('pa', side)
      col2 <- paste0('pa', 3 - side)
      other <- c(other, as.character(rcd[rcd[[col1]] == pa1, col2]))
    }
    spokes[pa1] <- length(other)
  }
  spokes
}

getWhiteList <- function(rcd) {
  palist1 <- extractPalist(rcd)
  ss <- calcSampleSize(rcd)
  sp <- calcSpokes(rcd)
  palist1[ss > 10 | sp > 1]
}

diverentPerChain <- function(fit) {
  sapply(get_sampler_params(fit, inc_warmup=FALSE),
         function(x) sum(x[,'divergent__']))
}

prepDataForStan <- function(rcd) {
  palist1 <- extractPalist(rcd)
  rcd[is.na(rcd)] <- 10
  excludeCols <- -match(c(paste0('pa', 1:2), paste0('l', 1:2)), colnames(rcd))
  list(NPA=length(palist1),
       NFACETS=ncol(rcd)-length(excludeCols),
       NCMP=nrow(rcd),
       N=sum(rcd[excludeCols] != 10),
       pa1=match(rcd$pa1, palist1),
       pa2=match(rcd$pa2, palist1),
       diff=sapply(rcd[excludeCols], as.numeric))
}

worstNeff <- function(fit, regPar) {
  regParFit <- summary(fit, regPar, probs=.5)$summary
  regParFit[order(regParFit[,'n_eff']),]
}

plotByFacet <- function(fit, rcd, output="byFacet.pdf") {
  library(ggplot2)
  
  facetNames <- extractFacetNames(rcd)
  palist <- extractPalist(rcd)
  
  df <- summary(fit, pars=c(paste0("alpha[",1:length(facetNames),']')), probs=.5)$summary
  info <- matrix(c(df[,'mean'], 1:nrow(df)), ncol=2,
                 dimnames=list(facetNames, c('alpha','index')))
  info <- info[order(-info[,'alpha']),]
  
  df <- summary(fit, pars=c("theta"), probs=.5)$summary
  tar <- array(df[,'mean'], dim=c(length(facetNames), length(palist)))
  dimnames(tar) <- list(facetNames, palist)
  
  span <- max(abs(tar))
  
  ss <- calcSampleSize(rcd)
  
  hasFlow <- "flowLoadings" %in% names(fit@inits[[1]])
  if (hasFlow) {
    flow <- summary(fit, pars=c("flowLoadings"), probs=.5)$summary
  } else {
    flow <- NULL
  }
  
  cairo_pdf(file=output, onefile=TRUE, height=3, pointsize=5)
  
  for (ix in 1:nrow(info)) {
    fx <- info[ix,'index']
    if (is.null(flow)) {
      flowLoading <- 0
      flowSign <- -1
    } else {
      flowLoading <- flow[fx,'mean']
      flowSign <- -1 * sign(flowLoading)
    }
    pl <- ggplot(data.frame(x=flowSign*tar[fx,],
                            sampleSize=ss, sampleSizeM=-ss, activity=palist, y=0.47)) +
      geom_point(aes(x=x,size=sampleSize, alpha=sampleSize),y=0) +
      geom_text(aes(label=activity, x=x, color=sampleSizeM, y=y),
                angle=85, hjust=0, size=2, position = position_jitter(width = 0, height = 0.4)) +
      xlim(-span, span) +
      ggtitle(paste(rownames(info)[ix], round(info[ix,'alpha'],2), "loading", round(flowLoading,2))) + ylim(0,1) +
      theme(legend.position="none", axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    print(pl)
  }
  
  invisible(dev.off())
}
