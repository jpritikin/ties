plot.info <- function(spec, param, i.name, width=3) {
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
  df$total <- total
  df <- as.data.frame(df)
  long<- melt(df, id.vars=c('score'), variable.name="item")
  long$item <- factor(long$item)
  ggplot(long, aes(score, value, group=item)) +
    geom_line(size=1.1,aes(linetype=item, color=item)) + ylab("information")
}

data.vs.model <- function(spec1, param, espt, item.name, width=3, data.bins=10) {
  pm <- rpf.prob(spec1, param[1:spec1@numParam], seq(-width, width, .1))
  icc <- as.data.frame(melt(pm, varnames=c("theta",'category')))
  icc$theta <- seq(-width, width, .1)
  icc$category <- as.ordered(1+max(icc$category)-icc$category)  #parscale reverses stuff
  icc$type <- 'model'
  
  breaks <- seq(min(espt$score, na.rm=TRUE),
                max(espt$score, na.rm=TRUE),
                length.out=data.bins+1)
  bin <- unclass(cut(espt$score, breaks, include.lowest = TRUE))
  est <- tabulate(bin, length(levels(bin)))
  if (any(est < 10)) {
    warning("Some bins have less than 10 samples; try fewer data.bins")
  }
  
  eout <- array(dim=c(data.bins, spec1@numOutcomes+1))
  for (px in 1:data.bins) {
    t <- table(espt[[item.name]][bin==px], useNA="no")
    eout[px,2:(spec1@numOutcomes+1)] <- t / sum(t)
  }
  eout[,1] <- ((c(breaks,0) + c(0,breaks))/2)[2:(data.bins+1)]
  
  edf <- melt(as.data.frame(eout), id.vars=c('V1'),
              variable.name="category")
  edf$category <- ordered(unclass(edf$category))
  edf$theta <- edf$V1
  edf$V1 <- NULL
  edf$type <- 'data'

  both <- rbind(edf, icc)
  both$type <- factor(both$type)

  ggplot(both, aes(theta, value)) +
              geom_line(aes(color=category, linetype=category)) + facet_wrap(~type) +
    ylim(0,1) + xlim(-width,width) + labs(y="probability", x="score")
}

## name <- 'msCause'
## item.x <- match(name,ms.items$name)
## param <- ms.items[item.x, c('slope',paste0('b',1:4))]
## data.vs.model(spec[[item.x]], param,ms.people , name, data.bins=12) +
##   labs(title = paste0(name, ", slope = ",param[1]))
