library(rstan)
library(ggplot2)

load("/tmp/simFit3.rda")

df <- summary(sim_fit, pars=c(paste0("alpha[",1:NFACETS,']')), probs=.5)$summary
alpha <- matrix(df[,'mean'], nrow=1,
                dimnames=list(NULL, facetNames))

df <- summary(sim_fit, pars=c("theta"), probs=.5)$summary
tar <- array(df[,'mean'], dim=c(NFACETS, length(whitelist)))
dimnames(tar) <- list(facetNames, whitelist)

span <- max(abs(tar))

spokes <- spokes[whitelist]

flow <- summary(sim_fit, pars=c("flow"), probs=.5)$summary

cairo_pdf(file="byFacet.pdf", onefile=TRUE, height=3, pointsize=5)

for (fx in order(alpha)) {
  flowLoading <- flow[fx,'mean']
  flowSign <- -1 * sign(flowLoading)
  pl <- ggplot(data.frame(x=flowSign*tar[fx,],
    sampleSize=spokes, sampleSizeM=-spokes, activity=whitelist, y=0.47)) +
    geom_point(aes(x=x,size=sampleSize, alpha=sampleSize),y=0) +
    geom_text(aes(label=activity, x=x, color=sampleSizeM, y=y),
      angle=85, hjust=0, size=2, position = position_jitter(width = 0, height = 0.4)) +
    xlim(-span, span) +
    ggtitle(paste(facetNames[fx], round(alpha[fx],2), "loading", round(flowLoading,2))) + ylim(0,1) +
    theme(legend.position="none", axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())
  print(pl)
}

dev.off()
