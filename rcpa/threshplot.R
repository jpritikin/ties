library(rstan)
library(ggplot2)

load("/tmp/simFit3.rda")

softmax <- function(v) {
    exp(v) / sum(exp(v))
}
draw <- function(alpha, th) {
  tdiff <- seq(-5,5,.1)
  gr <- expand.grid(tdiff=tdiff, category=c("much more","somewhat more", 'equal',
    "somewhat less", "much less"), p=NA)
  gg <- matrix(c(0,
    -(th[1] + th[2]),
    -th[1],
    th[1],
    th[1] + th[2]), ncol=5, nrow=length(tdiff), byrow=TRUE)
  gg[,2:5] <- alpha * (gg[,2:5] + tdiff)
  gg <- t(apply(gg, 1, cumsum))
  gg <- t(apply(gg, 1, softmax))
  for (lev in 1:length(levels(gr$category))) {
    gr[gr$category == levels(gr$category)[lev],'p'] <- gg[,lev]
  }
  ggplot(gr) + geom_line(aes(x=tdiff,y=p,color=category,linetype=category)) +
    xlab("difference in latent ranking (logits)") + ylab("probability") + ylim(0,1)
}

al <- summary(sim_fit, pars=c(paste0("alpha[",1:NFACETS,']')), probs=.5)$summary
if (0) {
  th1 <- summary(sim_fit, pars=c(paste0("threshold1[",1:NFACETS,']')), probs=.5)$summary
  th2 <- summary(sim_fit, pars=c(paste0("threshold2[",1:NFACETS,']')), probs=.5)$summary
} else {
  th1 <- summary(sim_fit, pars=c(paste0("threshold1")), probs=.5)$summary
  th2 <- summary(sim_fit, pars=c(paste0("threshold2")), probs=.5)$summary
}

cairo_pdf(file="crc.pdf", onefile=TRUE)
for (fx in 1:NFACETS) {
  if (0) {
    th <- c(th1[fx,'mean'], th2[fx,'mean'])
  } else {
    th <- c(th1[1,'mean'], th2[1,'mean'])
  }
  pl <- draw(al[fx,'mean'], th) + ggtitle(facetNames[fx])
  print(pl)
}
dev.off()

