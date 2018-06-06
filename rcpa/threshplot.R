source("modelUtil.R")

library(ggplot2)

load(paste0(outputDir(), "fitsip.rda"))  # independence model

alphaInd <- summary(fitsip, pars="alpha", probs=c(.025,.975))$summary

print(alphaInd)

load(paste0(outputDir(), "fitsfp.rda"))  # factor model
fit <- fitsfp

alpha.summary <- summary(fit, pars="alpha", probs=c(.025,.975))$summary

print(alpha.summary)  # should be similar to alphaInd

facetNames <- extractFacetNames(rcd)

th1.summary <- summary(fit, pars="threshold1", probs=c())$summary
rownames(th1.summary) <- facetNames

th2.summary <- summary(fit, pars="threshold2", probs=c())$summary
rownames(th2.summary) <- facetNames

pdata <- extract(fit, pars=c("alpha", paste0('threshold',1:2)), permuted = FALSE)
item50 <- pdata[1:50,1,]

save(facetNames, alphaInd, alpha.summary,
  th1.summary, th2.summary, item50, file="threshplot.rda")

q()

load("genFlowData.rda")

itemList <- ordered(facetNames, levels=facetNames[order(-abs(loadings[,'mean']))])

pl <- ggplot() + xlab("difference in latent ranking (logits)") + ylab("probability") + ylim(0,1)

draw <- function(item, alpha, th1, th2) {
  tdiff <- seq(-2.5/alpha, 2.5/alpha, .05/alpha)
  gr <- expand.grid(item=item, tdiff=tdiff, category=c("much more","somewhat more", 'equal',
    "somewhat less", "much less"), p=NA)
  gg <- matrix(c(0,
    -(th1 + th2),
    -th1,
    th1,
    th1 + th2), ncol=5, nrow=length(tdiff), byrow=TRUE)
  gg[,2:5] <- (gg[,2:5] + alpha * tdiff)
  gg <- t(apply(gg, 1, cumsum))
  gg <- t(apply(gg, 1, softmax))
  for (lev in 1:length(levels(gr$category))) {
    gr[gr$category == levels(gr$category)[lev],'p'] <- gg[,lev]
  }
  geom_line(data=gr, aes(x=tdiff,y=p,color=category,linetype=category), alpha=.1)
}

for (fx in 1:length(facetNames)) {
  for (cx in 1:50) {
    pl <- pl + draw(itemList[fx], item50[cx,'alpha'], item50[cx,1 + fx], item50[cx,1 + length(facetNames) + fx])
  }
}

print(pl + facet_wrap(~item))
