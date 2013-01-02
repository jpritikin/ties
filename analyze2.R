library(stringr)
library(ggplot2)
library(rpf)
library(reshape2)
library(gridExtra)

espt <- read.csv("raw.csv", stringsAsFactors=FALSE)

for (col in c('end')) {
  espt[[col]] <- strptime(espt[[col]], "%m/%d/%Y %H:%M:%S")
}
#espt$elapsed <- espt$end - espt$start

for (col in c('edu','sex','rel')) {
  espt[[col]] <- factor(espt[[col]])
}

agreement.levels <- c('Agree','Agree somewhat','Not sure','Disagree somewhat','Disagree')
for (col in c('msNotAny','msNotSelf','msMet','msAccident',
              'msShared','msCause','msTeach','msEvery','msPay',
              'msTrainTeach')) {
  espt[[col]] <- ordered(espt[[col]], levels=agreement.levels)
}

boredom.levels <- c('True','Not sure','False')
for (col in c('boreFidget', 'boreCheer', 'boreLone')) {
  espt[[col]] <- factor(espt[[col]], levels=boredom.levels, ordered=TRUE)
}

freq.levels <- c('Every time','Frequently','Infrequently',
                 'Almost never','Not at all')
subj.time.levels <- c('I am unaware of actual time.',
                      'Subjective time is somewhat faster than actual time.',
                      'Subjective time is typical.',
                      'Subjective time is somewhat slower than actual time.',
                      'I am watching the clock.')
importance.levels <- c('Important', 'Somewhat important', 'Not sure',
                       'Somewhat inappropriate', 'Inappropriate')
for (context in c('re','sp','ps','rx','pe','wo','me','dd','fl')) {
  for (col in c(paste(context,'ms.pf',sep='.'),
                paste(context,'b.pf',sep='.'))) {
    espt[[col]] <- factor(espt[[col]], levels=freq.levels, ordered=TRUE)
  }
  col <- paste(context,'subjTime',sep='.')
  espt[[col]] <- factor(espt[[col]], levels=subj.time.levels, ordered=TRUE)
  for (col in c(paste(context,'ms.gi',sep='.'),
                paste(context,'b.gi',sep='.'))) {
    espt[[col]] <- factor(espt[[col]], levels=importance.levels, ordered=TRUE)
  }
}

thinking.levels <- c('None', 'A little thinking',
                     'Some moderate thinking',
                     'Rigorous, intensive thinking')
espt$flow.think <- factor(espt$flow.think, levels=thinking.levels, ordered=TRUE)

prepare.levels <- c('Yes', 'Yes, somewhat', 'Not sure / maybe',
                    'Probably not', 'No')
for (col in c('ms.flow', 'flow.ms')) {
  espt[[col]] <- factor(espt[[col]], levels=prepare.levels, ordered=TRUE)
}

for (col in c('m.training', 'm.regular', 'wave', 'ip.continent',
              'ip.country', 'ip.region', 'ip.city')) {
  espt[[col]] <- factor(espt[[col]])
}
espt$ppool <- factor(espt$wave == "ppool-20121230",
                     levels=c(TRUE,FALSE), labels=c("UVa Human Subjects Pool","Web Surfers"))

########################################################
# demographics

# do participants accurately self report their location? looks good:
# espt[(tolower(as.character(espt$ip.country)) != tolower(espt$country)),c('ip.country', 'country')]

table(espt$wave, grepl("\\bstudent\\b", espt$work, ignore.case=TRUE))

table(espt$wave, espt$sex)

table(espt$born)
table(espt$wave, espt$rel)
table(espt$edu)
table(espt$wave, espt$m.training)

########################################################
# Check for crazy stuff. The vast majority of the data looks reasonable.
# No need to exclude anything.

dis.logic <- cbind(
  msOpposite1=unclass(espt$msEvery) + unclass(espt$msNotAny)-6 < -2,
  msOpposite2=unclass(espt$msNotSelf) + unclass(espt$msCause)-6 < -2,
  msCrazy1=pmax(unclass(espt$msTeach) - unclass(espt$msTrainTeach),0)>1,
  boreCrazy=espt$boreFidget=='No' & espt$boreCheer=='Yes' & espt$boreLone=='False',
  msCrazy2=((espt$wantLearn=='Not sure' | espt$wantLearn=='No') &
    (espt$freqCause=='Daily' | espt$freqCause=='Weekly' | espt$freqCause=='Infrequently')),
  msCrazy3=xor(espt$maxDuration=='I have not experienced complete mental silence',
      espt$durationCharacter=='I have not experienced complete mental silence')
  )
table(apply(dis.logic, 1, sum, na.rm=TRUE))

# msCause -- It is not clear who is causing mental silence.
# table(pmax(unclass(espt$msCause) - unclass(espt$msTeach),0))

dis.flow <- cbind(
  pmax(4-unclass(espt$fl.b.pf),0),
  pmax(unclass(espt$fl.subjTime)-3,0),
  pmax(3-unclass(espt$fl.b.gi),0))
table(apply((dis.flow), 1, sum, na.rm=TRUE))

################################################################
i1 <- rpf.gpcm(5)

ms.scale.items <- c('msNotAny','msNotSelf','msMet','msAccident','msShared','msCause',
'msTeach','msEvery','msPay','msTrainTeach')
ms.scale.reverse <- c(FALSE,FALSE, rep(TRUE,8))

ms.scale <- espt[,ms.scale.items]
#table(apply(is.na(ms.scale), 1, sum))
na.filter <- apply(is.na(ms.scale), 1, sum) == 0
ms.scale <- ms.scale[na.filter,]
parscale.export10 <- function () {
  foo <- sapply(ms.scale, as.integer)
  foo[,ms.scale.reverse] <- 6 - foo[,ms.scale.reverse]
  ms.responses <- apply(foo, 1, paste0, collapse='')
  cat(sprintf("%s %s\n", espt[na.filter,'id'], ms.responses), file="ms.dat", sep='')
  write.csv(foo, "ms.csv")
}
parscale.export8 <- function () {
  small.ms <- ms.scale
  small.ms$msAccident <- NULL
  small.ms$msPay <- NULL
  foo <- sapply(small.ms, as.integer)
  foo[,3:8] <- 6 - foo[,3:8]  # reverse score items 1,2
  ms.responses <- apply(foo, 1, paste0, collapse='')
  cat(sprintf("%s %s\n", espt[na.filter,'id'], ms.responses), file="ms.dat", sep='')
  write.csv(foo, "ms.csv")
}
parscale.export.sim <- function () {
  param <- list()
  items <- list()
  for (ix in 1:10) {
    items[[ix]] <- i1
    param[[ix]] <- c(1+ix/10, -.5+ix/10, -.25+ix/10, .5+ix/10, 2+ix/10)
  }
  foo <- rpf.sample(length(espt[na.filter,'id']), items, param)
  bar <- sapply(foo, as.integer)
  ms.responses <- apply(bar, 1, paste0, collapse='')
  cat(sprintf("%s %s\n", espt[na.filter,'id'], ms.responses), file="ms.dat", sep='')
  write.csv(foo, "ms.csv")
}
parscale.export10()

scores <- read.csv("scores.csv", stringsAsFactors=FALSE)
for (col in c("score","se")) {
  espt[[col]] <- NA
  espt[[col]][match(scores$id, espt$id)] <- scores[[col]]
}

# compare sample distributions
ggplot(espt, aes(x=score)) + geom_histogram() + facet_grid(ppool ~ .)

# nothing obvious here
#pairs(~msPay+born+sex+edu+score, data=espt)

##############################################
items <- read.csv("items.csv", stringsAsFactors=FALSE)

data.vs.model <- function(item.name, width=3, data.bins=10) {
  item.x <- match(item.name, ms.scale.items)
  pm <- rpf.prob(i1, items[item.x,], seq(-width, width, .1))
  icc <- as.data.frame(melt(pm, varnames=c("theta",'category')))
  icc$theta <- seq(-width, width, .1)
  icc$category <- as.ordered(icc$category)
  icc$type <- 'model'
  
  pgrid <- seq(min(espt$score, na.rm=TRUE),
    max(espt$score, na.rm=TRUE), length.out=data.bins+1)
  part <- findInterval(espt$score, pgrid, rightmost.closed=TRUE)
  eout <- array(dim=c(data.bins, i1@numOutcomes+1))
  for (px in 1:data.bins) {
    t <- table(espt[[item.name]][part==px], useNA="no")
    eout[px,2:(i1@numOutcomes+1)] <- t / sum(t)
  }
  eout[,1] <- ((c(pgrid,0) + c(0,pgrid))/2)[2:11]
  edf <- melt(as.data.frame(eout), id.vars=c('V1'),
              variable.name="category")
  head(edf$category, n=20)
  kat <- unclass(edf$category)
  if (ms.scale.reverse[item.x]) kat <- 6 - kat
  edf$category <- ordered(kat)
  head(edf$category, n=20)
  edf$theta <- edf$V1
  edf$V1 <- NULL
  edf$type <- 'data'

  both <- rbind(edf, icc)
  both$type <- factor(both$type)

  ggplot(both, aes(theta, value)) +
              geom_line(aes(color=category)) + facet_wrap(~type) +
    ylim(0,1) + xlim(-width,width) +
              labs(title=item.name, y="probability")
}

flush.plots <- function(plots, page) {
  pdf(paste0("data.vs.model-",page,".pdf"))
  do.call(grid.arrange,plots)
  dev.off()
}

page <- 1
plots <- list()
for (ix in ms.scale.items) {
  plots[[ix]] <- data.vs.model(ix)
  if (length(plots) == 2) {
    flush.plots(plots,page)
    page <- page+1
    plots <- list()    
  }
}
if (length(plots)) flush.plots(plots,page)
# pdfjoin data.vs.model-* -o d.vs.m.pdf
