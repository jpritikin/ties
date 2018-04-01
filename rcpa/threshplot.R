source("modelUtil.R")

library(ggplot2)

load(paste0(outputDir(), "fit2t5.rda"))  # factor model
fit <- fit2t5

alpha.summary <- summary(fit, pars="alpha", probs=c(.025,.975))$summary

th.summary <- summary(fit, pars=paste0("threshold",1:2), probs=c(.025,.975))$summary

pdata <- extract(fit, pars=c("alpha", paste0('threshold', 1:2)), permuted = FALSE)
pdata50 <- pdata[1:50,1,]

item25 <- list(
  th1 = pdata50[1:25,'threshold1'],
  th2 = pdata50[1:25,'threshold2'],
  alpha = pdata50[1:25,'alpha'])

save(alpha.summary, th.summary, item25, pdata50, file="threshplot.rda")

q()

draw <- function(alpha, th) {
  tdiff <- seq(-5/alpha,5/alpha,.1/alpha)
  gr <- expand.grid(tdiff=tdiff, category=c("much more","somewhat more", 'equal',
    "somewhat less", "much less"), p=NA)
  gg <- matrix(c(0,
    -(th[1] + th[2]),
    -th[1],
    th[1],
    th[1] + th[2]), ncol=5, nrow=length(tdiff), byrow=TRUE)
  gg[,2:5] <- gg[,2:5] + alpha * tdiff
  gg <- t(apply(gg, 1, cumsum))
  gg <- t(apply(gg, 1, softmax))
  for (lev in 1:length(levels(gr$category))) {
    gr[gr$category == levels(gr$category)[lev],'p'] <- gg[,lev]
  }
  geom_line(data=gr, aes(x=tdiff,y=p,color=category,linetype=category), alpha=.05)
}

alpha <- alpha.summary[,'mean']

pl <- ggplot() + xlab("difference in latent ranking (logits)") + ylab("posterior density") + ylim(0,1)
for (cx in 1:25) {
  pl <- pl + draw(alpha, c(th1[cx], th2[cx]))
}
pl

cairo_pdf(file="crc.pdf", onefile=TRUE, height=5)
print(pl)
dev.off()

