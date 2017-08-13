library(ggplot2)

th <- c(-2.489,0.86)
alpha <- .32

softmax <- function(v) {
    exp(v) / sum(exp(v))
}
tdiff <- seq(-4,4,.1)
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
    xlab("difference in latent ranking (logits)") + ylab("probability")
