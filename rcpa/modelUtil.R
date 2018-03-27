library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

outputDir <- function() './data/'

stanChains <- 6

if (stanChains > parallel::detectCores()) {
  stop(paste("Reduce stanChains to less than or equal to", parallel::detectCores()))
}

loadRawData <- function(dir='.') {
  rcd <- read.csv(paste0(dir,"/rawData.csv"), stringsAsFactors=FALSE)
  ignCol <- c('recno', paste0('injury', 1:2), paste0(c('goal','feedback'),2))
  rcd <- rcd[,-match(ignCol, colnames(rcd))]
  if (nrow(rcd) < 1) { stop("No data?") }
  rcd
}

loadWhitelistRawData <- function(dir='.') {
  rcd <- loadRawData(dir)
  whitelist <- getWhiteList(rcd)
  rcd[rcd$pa1 %in% whitelist & rcd$pa2 %in% whitelist,]
}

loadSingleFactorData <- function(dir='.') {
  rcd <- loadWhitelistRawData(dir)
  exclude <- c("spont", "goal1", "feedback1", "control", "waiting", 'evaluated')
  rcd <- rcd[,-match(exclude, colnames(rcd))]
  rcd
}

loadSimData <- function(dir='.') {
  read.csv(paste0(dir,"/simData.csv"), stringsAsFactors=FALSE)
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

lookupContextByDatumIndex <- function(rcd, index) {
  got <- NULL
  if (length(index)) for (xx in 1:length(index)) {
    got <- rbind(got, lookupContextByDatumIndex1(rcd, index[xx]))
  }
  got
}

lookupContextByDatumIndex1 <- function(rcd, index) {
  excludeCols <- -match(c(paste0('pa', 1:2), paste0('l', 1:2)), colnames(rcd))
  for (rx in 1:nrow(rcd)) {
    rowData <- rcd[rx,excludeCols]
    rowDataCount <- sum(!is.na(rowData))
    if (index < rowDataCount) {
      naMap <- rep(NA, length(rowData))
      naMap[!is.na(rowData)] <- 1:rowDataCount
      pick <- match(1+index, naMap)
      pa1 <- as.character(rcd[rx,'pa1'])
      pa2 <- as.character(rcd[rx,'pa2'])
      ss <- calcSampleSize(rcd)
      return(data.frame(row.names=rownames(rcd)[rx], row=rx,
        pa1=pa1, pa1ss=ss[pa1], pa2=pa2, pa2ss=ss[pa2],
        facet=colnames(rowData)[pick]))
    } else {
      index <- index - rowDataCount
    }
  }
}

worstNeff <- function(fit, regPar) {
  regParFit <- summary(fit, regPar, probs=c())$summary
  regParFit[order(regParFit[,'n_eff']),]
}

# https://groups.google.com/forum/#!topic/stan-users/5WG51xKNSbA
# http://andrewgelman.com/2007/04/02/markov_chain_mo/

worstRhat <- function(fit, regPar) {
  regParFit <- summary(fit, regPar, probs=c())$summary
  regParFit[order(-regParFit[,'Rhat']),]
}

plotByFacet <- function(fit, rcd, output="byFacet.pdf") {
  library(ggplot2)
  
  facetNames <- extractFacetNames(rcd)
  palist <- extractPalist(rcd)
  
  df <- summary(fit, pars='sigma', probs=.5)$summary
  info <- matrix(c(df[,'mean'], 1:nrow(df)), ncol=2,
                 dimnames=list(facetNames, c('sigma','index')))
  info <- info[order(-info[,'sigma']),]
  
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
      ggtitle(paste(rownames(info)[ix], "sd=", round(info[ix,'sigma'],2), "flow=", round(flowLoading,2))) + ylim(0,1) +
      theme(legend.position="none", axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    print(pl)
  }
  
  invisible(dev.off())
}

makeSimplex5 <- function(v) {
  v <- v[v >= -2 & v <= 2]  # ignore missing data
  tbl <- table(v)
  got <- rep(0, 5)
  names(got) <- -2:2
  for (tx in 1:length(tbl)) {
    got[ names(tbl)[tx] ] <- tbl[tx]
  }
  got
}

ppc <- function(sim_fit, rcd) {
  edges <- paste(rcd[,'pa1'], rcd[,'pa2'], sep=":")
  edgeTable <- table(edges)

  #print(edgeTable[edgeTable >= 5])
  checkList <- names(edgeTable[edgeTable >= 5])

  facetNames <- extractFacetNames(rcd)
  NFACETS <- length(facetNames)

  rcat_sim <- extract(sim_fit, pars=c("rcat_sim"), permuted=TRUE)$rcat_sim
  dimnames(rcat_sim)[[3]] <- facetNames

  pval <- matrix(NA, length(checkList), NFACETS,
    dimnames = list(checkList, facetNames))
  for (cx in 1:length(checkList)) {
    for (fx in 1:NFACETS) {
      obs <- makeSimplex5(rcd[edges == checkList[cx], facetNames[fx]])
      ex <- makeSimplex5(rcat_sim[,edges == checkList[cx], facetNames[fx]])
      ex[ex==0] <- 0.5
      ex <- (sum(obs) * ex) / sum(ex)
      stat <- sum((obs - ex)^2 / ex)
      # thresholds are common across all items so don't count for df?
      df <- 3    # 5 (categories) - 1 - 1 (sigma)
      pval[cx,fx] <- pchisq(stat, df, lower.tail=FALSE)
    }
  }
  pval
}

ppc1 <- function(sim_fit, rcd, pair, facet) {
  edges <- paste(rcd[,'pa1'], rcd[,'pa2'], sep=":")

  facetNames <- extractFacetNames(rcd)
  rcat_sim <- extract(sim_fit, pars=c("rcat_sim"), permuted=TRUE)$rcat_sim
  dimnames(rcat_sim)[[3]] <- facetNames

  obs <- makeSimplex5(rcd[edges == pair, facet])
  ex <- makeSimplex5(rcat_sim[,edges == pair, facet])
  ex[ex==0] <- 0.5
  ex <- (sum(obs) * ex) / sum(ex)
  stat <- sum((obs - ex)^2 / ex)
  # thresholds are common across all items so don't count for df?
  df <- 3    # 5 (categories) - 1 - 1 (sigma)
  pval <- pchisq(stat, df, lower.tail=FALSE)
  list(obs=obs, ex=ex, stat=stat, pval=pval)
}

softmax <- function(y) exp(y) / sum(exp(y))

