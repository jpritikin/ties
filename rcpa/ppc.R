library(rstan)

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

load("/tmp/simFit3.rda")

# level of mastery not used yet
rcd <- rcd[,-match(paste0('l',1:2), colnames(rcd))]

edges <- paste(rcd[,'pa1'], rcd[,'pa2'], sep=":")
edgeTable <- table(edges)

print(edgeTable[edgeTable >= 5])
checkList <- names(edgeTable[edgeTable >= 5])

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
    df <- 3    # 5 (categories) - 1 - 1 (alpha)
    pval[cx,fx] <- pchisq(stat, df, lower.tail=FALSE)
  }
}

print(sum(pval<.05) / length(pval))
print(apply(pval, 1, function(x) sum(x<.05)))
print(apply(pval, 2, function(x) sum(x<.05)))
