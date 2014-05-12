library(ggplot2)
library(reshape2)
library(gridExtra)

item.map <- function(grp, factor=1) {
  item.mask <- grp$param[factor,] > 0
  result <- NULL
  for (ix in rev(colnames(grp$param)[item.mask])) {
    lev <- levels(grp$data[,ix])
    for (ox in 1:length(lev)) {
      mask <- grp$data[,ix]==lev[ox]
      mask <- !is.na(mask) & mask
      if (all(!mask)) next
      result <- rbind(result, data.frame(item=ix,
                                         outcome=ox, outcome.name=lev[ox],
                                         score=mean(grp$score[mask, factor], na.rm=TRUE)))
    }
  }
  result
}

# new version of data vs model plot
rpf.plot <- function(grp, item.name, width=3, data.bins=11, basis=c(1), factor=1) {
  ix <- match(item.name, colnames(grp$param))
  if (length(ix) != 1 || is.na(ix)) stop(paste("Can't find", item.name))
  
  labels <- levels(grp$data[[item.name]])
  spec1 <- grp$spec[[ix]]
  pm <- t(rpf.prob(spec1, grp$param[1:rpf.numParam(spec1),ix], basis %*% t(seq(-width, width, .1))))
  icc <- as.data.frame(melt(pm, varnames=c("theta",'category')))
  icc$theta <- seq(-width, width, .1)
  icc$category <- ordered(icc$category, labels=labels)
  icc$type <- 'model'
  
  score <- grp$score[,factor]
  breaks <- seq(min(score, na.rm=TRUE),
                max(score, na.rm=TRUE),
                length.out=data.bins+1)
  bin <- unclass(cut(score, breaks, include.lowest = TRUE))
  
  eout <- array(dim=c(data.bins, spec1@outcomes+1))
  est <- numeric(data.bins)
  
  for (px in 1:data.bins) {
    t <- table(grp$data[[item.name]][bin==px])
    est[px] <- sum(t)
    eout[px,2:(spec1@outcomes+1)] <- t / sum(t)
  }
  eout[,1] <- ((c(breaks,0) + c(0,breaks))/2)[2:(data.bins+1)]
  bin.n <- data.frame(n=est, theta=eout[,1])
  
  edf <- melt(as.data.frame(eout), id.vars=c('V1'),
              variable.name="category")
  edf$category <- ordered(unclass(edf$category), labels=labels)
  edf$theta <- edf$V1
  edf$V1 <- NULL
  edf$type <- 'data'
  
  both <- rbind(edf, icc)
  both$type <- factor(both$type)
  
  plot <- ggplot(both, aes(theta, value)) +
     facet_wrap(~type) +
    ylim(0,1) + xlim(-width,width) + labs(y="probability") +
    geom_text(data=bin.n, aes(label=n, x=theta), y = 1, size=1.5, angle=90)
  guide.style <- guide_legend(keywidth=.1, keyheight=.5, direction = "horizontal", title.position = "top",
                              label.position="bottom", label.hjust = 0.5, label.vjust = .5,
                              label.theme = element_text(angle = 90, size=8))
  if (length(labels) <= 12) {
    plot <- plot + geom_line(aes(color=category, linetype=category)) +
      guides(color = guide.style, linetype = guide.style)
  } else {
    plot <- plot + geom_line(aes(color=category)) + 
      guides(color = guide.style)
  }
  plot + labs(title = paste0(ix,": ",item.name))
}

plot.info <- function(grp, width=3, show.total=TRUE) {
  spec <- grp$spec
  param <- grp$param
  i.name <- colnames(grp$param)
  grid <- seq(-width,width,.1)
  df <- list(score=grid)
  total <- numeric(length(grid))
  for (ix in 1:length(spec)) {
    id <- i.name[ix]
    s <- spec[[ix]]
    df[[id]] <- rpf.info(s, param[1:rpf.numParam(s),ix], t(grid))
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
  
  pm <- t(rpf.prob(spec1, param[1:rpf.numParam(spec1)], seq(-width, width, .1)))
  icc <- as.data.frame(melt(pm, varnames=c("theta",'category')))
  icc$theta <- seq(-width, width, .1)
  icc$category <- ordered(icc$category, labels=labels)
  icc$type <- 'model'

  breaks <- seq(min(score, na.rm=TRUE),
                max(score, na.rm=TRUE),
                length.out=data.bins+1)
  bin <- unclass(cut(score, breaks, include.lowest = TRUE))
  
  eout <- array(dim=c(data.bins, spec1@outcomes+1))
  est <- numeric(data.bins)

  for (px in 1:data.bins) {
    t <- table(rawdata[[item.name]][bin==px])
    est[px] <- sum(t)
    eout[px,2:(spec1@outcomes+1)] <- t / sum(t)
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
  
  pdf(sprintf("gen/page-%03d.pdf", page))
  do.call(grid.arrange,plots)
  dev.off()
}

booklet <- function (plot.fn, args, output="booklet.pdf") {
  unlink("gen/page-*.pdf")
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
  if (length(plots) == 0 && page == 2) {
    file.rename("gen/page-001.pdf", output)
  } else {
    system(paste("pdfunite  gen/page-*", output))
  }
}

## name <- 'msCause'
## item.x <- match(name,ms.items$name)
## param <- ms.items[item.x, c('slope',paste0('b',1:4))]
## data.vs.model(spec[[item.x]], param,ms.people , name, data.bins=12) +
##   labs(title = paste0(name, ", slope = ",param[1]))
