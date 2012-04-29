# http://people.virginia.edu/~js6ew/jrs.r
source("jrs.R")
options(jrsCacheDir='.cache')
library(eRm)
library(reshape2)
library(ggplot2)
library(psych)
library(stringr)
library(xtable)

num <- 200

set.seed(7)

cms.edu <- factor(floor(runif(num)*3),
                  labels=c("high school","college","graduate"))
cms.training <- factor(runif(num) > .75, labels=c("no","yes"))
cms.familiar <- (rt(num, df=8) +
  ifelse(cms.training=="yes", abs(rnorm(num)),0))

cms.latent <- data.frame(id=1:num, edu=cms.edu, train=cms.training,
                         familiar=cms.familiar)

cms <- cache(function () {
  # rbinom would have worked better
  sim.categorical <- function(person, item, discr, levels) {
    pick <- rnorm(length(person), person - item, sd=discr)
    return(apply(as.matrix(pick), c(1), function (p) { sum(p > levels) }))
  }
  sim.familiar <- function(person, item, discr) {
    sim.categorical(person, item, discr, qnorm(c(.2,.8)))
  }
  sim.context <- function(person, item, discr) {
    sim.categorical(person, item, discr, qnorm(c(.05,.3,.7,.95)))
  }
  
  cms <- data.frame(
    notSense=sim.familiar(cms.latent$familiar, -2, .8),
    possAny=sim.familiar(cms.latent$familiar, -1.5, 1.6),
    possMyself=sim.familiar(cms.latent$familiar, -1, 1.1),
    accident=sim.familiar(cms.latent$familiar, 0, 1.3),
    intention=sim.familiar(cms.latent$familiar, 1, .6),
    certain=sim.familiar(cms.latent$familiar, 1.5, .9),
    
    cxReligion=sim.context(cms.latent$familiar, 0, 1.5),
    cxSolving=sim.context(cms.latent$familiar + unclass(cms.latent$edu)/3, 0, .8),
    cxSpirit=sim.context(cms.latent$familiar, -.5, 1.3),
    cxRelax=sim.context(cms.latent$familiar, 0, 1.5),
    cxDaydream=sim.context(cms.latent$familiar, 1, .9),
    cxMeditate=sim.context(cms.latent$familiar, -1, 1.5),
    cxExercise=sim.context(cms.latent$familiar, 0, 1.7))
  return(cms)
}, 'simData')

fam.pcm1 <- cache({ PCM(cms[,1:6]) }, "fam.pcm")
fam.pcm1.p <- cache({ person.parameter(fam.pcm1) }, "fam.pcm.p")
fam.pcm1.res <- principal(itemfit(fam.pcm1.p)$st.res, rotate="none")

ctx.pcm1 <- cache({ PCM(cms[,7:13]) }, "ctx.pcm")
ctx.pcm1.p <- cache({ person.parameter(ctx.pcm1) }, "ctx.pcm.p")
ctx.pcm1.res <- principal(itemfit(ctx.pcm1.p)$st.res, rotate="none")

plotInfo <- function(threshold, xrange=c(-8,8)) {
  ability <- seq(xrange[1],xrange[2],length.out=100)
  df <- data.frame(ability=ability)
  all <- rep(0,100)
  names <- rownames(threshold)
  for (item in 1:dim(threshold)[1]) {
    row <- threshold[item,2:dim(threshold)[2]]
    info <- calcICIF(row, xrange)
    df[,names[item]] <- info
    all <- all + info
  }
  df$all <- all
  return(ggplot(melt(df, id.vars=c('ability')),
                aes(ability, value, color=variable, linetype=variable)) +
         geom_line() +
    ylab("Information") + xlab("Trait score") +
    scale_x_continuous(limits=xrange,
                       breaks=seq(xrange[1],xrange[2],by=2)) +
                         opts(legend.title=theme_blank()))
}

calcICC <- function(thr, xrange=c(-8,8)) {
  ability <- seq(xrange[1],xrange[2],length.out=100)
  denom <- rep(1,length(ability))
  for (tx in 1:length(thr)) {
    part <- rep(0,length(ability))
    for (ty in 1:tx) {
      part <- part + ability - thr[ty]
    }
    denom <- denom + exp(part)
  }
  numer <- as.matrix(rep(0,length(ability)))
  for (tx in 1:length(thr)) {
    numer <- cbind(numer, numer[,dim(numer)[2]] + ability - thr[tx])
  }
  icc <- as.data.frame(exp(numer)/denom)
  icc$ability <- ability
  return(icc)
}

calcICIF <- function(thr, xrange=c(-8,8)) {
  icc <- calcICC(thr, xrange)
  m <- length(thr)+1
  refpt <- apply(t(icc[1:m]) * 1:m, c(2), sum)
  for (cat in 1:m) {
    icc[cat] <- icc[cat] * (cat - refpt)^2
  }
  info <- apply(icc[1:m], c(1), sum)
  return(info)
}

myPlotICC <- function(thr, xrange=c(-8,8)) {
  icc <- calcICC(thr, xrange)
  return(ggplot(melt(icc, id.vars=c('ability')),
         aes(ability, value, color=variable, linetype=variable)) +
         geom_line() +
         ylab("Probability") + xlab("Trait score") +
         ylim(0,1) +
         scale_x_continuous(limits=xrange,
                              breaks=seq(xrange[1],xrange[2],by=2)) +
         opts(legend.title=theme_blank()))
}

# http://wiki.stdout.org/rcookbook/Graphs
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  } 
}

train.flag <- round(cms.training=='yes')

sink("/dev/null")
lrt <- LRtest(fam.pcm1, splitcr=train.flag)
sink()

mySummary <- function(v) {
  c('count'=length(v),
    quantile(v, 0),
    quantile(v, .25),
    'median'=median(v),
    'mean'=mean(v),
    quantile(v, .75),
    quantile(v, 1),
    'std deviation'=sd(v))
}

pcmSummary <- function(pcm1, pcm1.p, ...) {
  item.param <- pcm1$etapar
  item.fit <- itemfit(pcm1.p)
  person.param <- pcm1.p$theta.table$Person
  person.fit <- personfit(pcm1.p)

  brief <- cbind('Item'=mySummary(item.param),
                  'Outfit MSQ'=mySummary(item.fit$i.outfitMSQ),
                  'Infit MSQ'=mySummary(item.fit$i.infitMSQ),
                  'Person'=mySummary(person.param),
                  'Outfit MSQ'=mySummary(person.fit$p.outfitMSQ),
                  'Infit MSQ'=mySummary(person.fit$p.infitMSQ))
  print(xtable(brief, ...), table.placement=table.placement)
}
