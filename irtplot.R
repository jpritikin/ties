plot.info <- function(spec, param, i.name, width=3, show.total=TRUE) {
  if (missing(i.name)) {
    i.name <- paste0('i', 1:length(spec))
  }
  grid <- seq(-width,width,.1)
  df <- list(score=grid)
  total <- numeric(length(grid))
  for (ix in 1:length(spec)) {
    id <- i.name[ix]
    s <- spec[[ix]]
    df[[id]] <- rpf.info(s, param[ix,1:s@numParam], grid)
    total <- total + df[[id]]
  }
  if (show.total) df$total <- total
  df <- as.data.frame(df)
  long<- melt(df, id.vars=c('score'), variable.name="item")
  long$item <- factor(long$item)
  ggplot(long, aes(score, value, group=item)) +
    geom_line(size=1.1,aes(color=item)) + ylab("information")
}
# linetype=item,

data.vs.model <- function(spec1, param, rawdata, score, item.name, width=3, data.bins=11, plot.rug=FALSE) {
  labels <- abbreviate(levels(rawdata[[item.name]]), minlength=16)

  pm <- rpf.prob(spec1, param[1:spec1@numParam], seq(-width, width, .1))
  icc <- as.data.frame(melt(pm, varnames=c("theta",'category')))
  icc$theta <- seq(-width, width, .1)
  icc$category <- ordered(icc$category, labels=labels)
  icc$type <- 'model'
  
  breaks <- seq(min(score, na.rm=TRUE),
                max(score, na.rm=TRUE),
                length.out=data.bins+1)
  bin <- unclass(cut(score, breaks, include.lowest = TRUE))
  
  eout <- array(dim=c(data.bins, spec1@numOutcomes+1))
  est <- numeric(data.bins)

  for (px in 1:data.bins) {
    t <- table(rawdata[[item.name]][bin==px])
    est[px] <- sum(t)
    eout[px,2:(spec1@numOutcomes+1)] <- t / sum(t)
  }
  eout[,1] <- ((c(breaks,0) + c(0,breaks))/2)[2:(data.bins+1)]

  if (any(est < 10)) {
    warning(paste0("For ", item.name, ", some bins have less than 10 samples; try fewer data.bins"))
  }
  rug <- data.frame(x=eout[,1], people=est)

  edf <- melt(as.data.frame(eout), id.vars=c('V1'),
              variable.name="category")
  edf$category <- ordered(unclass(edf$category), labels=labels)
  edf$theta <- edf$V1
  edf$V1 <- NULL
  edf$type <- 'data'

  both <- rbind(edf, icc)
  both$type <- factor(both$type)

  plot <- ggplot(both, aes(theta, value)) +
              geom_line(aes(color=category, linetype=category)) + facet_wrap(~type) +
    ylim(0,1) + xlim(-width,width) + labs(y="probability")
  if (plot.rug) {
    plot <- plot + geom_rug(data=rug, aes(x,y=people,size=people), sides="b")
  }
  plot
}

flush.plots <- function(plots, page) {
  if (length(plots) == 1) {
    plots$filled <- 
      ggplot(expand.grid(x=1:4, y=1:4), aes(x,y)) + geom_point()
  }
  
  pdf(sprintf("gen/data.vs.model-%03d.pdf", page))
  do.call(grid.arrange,plots)
  dev.off()
}

data.vs.model.booklet <- function (plot.fn, args) {
  page <- 1
  plots <- list()
  for (ix in args) {
    plots[[ix]] <- plot.fn(ix)
    if (length(plots) == 2) {
      flush.plots(plots,page)
      page <- page+1
      plots <- list()    
    }
  }
  if (length(plots)) flush.plots(plots,page)
  system("pdfjoin -q gen/data.vs.model-* -o data.vs.model.pdf")
}

## name <- 'msCause'
## item.x <- match(name,ms.items$name)
## param <- ms.items[item.x, c('slope',paste0('b',1:4))]
## data.vs.model(spec[[item.x]], param,ms.people , name, data.bins=12) +
##   labs(title = paste0(name, ", slope = ",param[1]))