source("modelUtil.R")

library(ggplot2)

load(paste0(outputDir(), "fit2t5.rda"))  # factor model
fit <- fit2t5

th.summary <- summary(fit, pars=paste0("threshold",1:2), probs=c(.025,.975))$summary

th1 <- unlist(extract(fit, pars="threshold1"), use.names=F)
th2 <- unlist(extract(fit, pars="threshold2"), use.names=F)

s1 <- 1:25

th25 <- list(th1 = th1[s1], th2=th2[s1])

save(th.summary, th25, file="threshplot.rda")

q()

draw <- function(th) {
  tdiff <- seq(-5,5,.1)
  gr <- expand.grid(tdiff=tdiff, category=c("much more","somewhat more", 'equal',
    "somewhat less", "much less"), p=NA)
  gg <- matrix(c(0,
    -(th[1] + th[2]),
    -th[1],
    th[1],
    th[1] + th[2]), ncol=5, nrow=length(tdiff), byrow=TRUE)
  gg[,2:5] <- gg[,2:5] + tdiff
  gg <- t(apply(gg, 1, cumsum))
  gg <- t(apply(gg, 1, softmax))
  for (lev in 1:length(levels(gr$category))) {
    gr[gr$category == levels(gr$category)[lev],'p'] <- gg[,lev]
  }
  geom_line(data=gr, aes(x=tdiff,y=p,color=category,linetype=category), alpha=.05)
}

pl <- ggplot() + xlab("difference in latent ranking (logits)") + ylab("posterior density") + ylim(0,1)
for (cx in sample.int(length(th1), 25)) {
  pl <- pl + draw(c(th1[cx], th2[cx]))
}

cairo_pdf(file="crc.pdf", onefile=TRUE, height=5)
print(pl)
dev.off()

