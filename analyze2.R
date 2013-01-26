library(stringr)
library(ggplot2)
library(rpf)
library(reshape2)
library(gridExtra)
source("jrs.R")
options(jrsCacheDir='.cache')

source("prepare.R")
source('irtplot.R')
espt <- read.csv("raw.csv", stringsAsFactors=FALSE)
scores <- read.csv("sit21c/scores.csv", stringsAsFactors=FALSE)
espt <- prepare.espt(espt, scores)

########################################################
# demographics

# do participants accurately self report their location? looks good:
# espt[(tolower(as.character(espt$ip.country)) != tolower(espt$country)),c('ip.country', 'country')]

#table(espt$wave, grepl("\\bstudent\\b", espt$work, ignore.case=TRUE))
if(0) {
  table(espt$wave, espt$rel)
  table(espt$edu)
  table(espt$wave, espt$m.training)
  
  table(espt[espt$ppool=='Web Surfers','edu'])
  sort(table(espt[espt$ppool=='Web Surfers','ip.country']))
}

########################################################
# Check for crazy stuff. The vast majority of the data looks reasonable.
# No need to exclude anything.
# WARNING: THIS CODE IS STALE. Some items are reversed scored now. DANGER

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
library(OpenMx)

m2.items <- c('msNotion','msNotAny','msNotSelf','msMet','msAccident',
              'msShared', 'msEnv','msCause', 'msPaySure', 'msTeach','msEvery',
              'msTrainTeach', 'wantLearn', 'freqCause', 'maxDuration')
m2.spec <- list()
m2.spec[1:15] <- rpf.gpcm(5)
m2.spec[13:14] <- rpf.gpcm(4)
m2.data <- espt[,m2.items]
m2.mask <- apply(m2.data, 1, function (r) !all(is.na(r)))
m2.data <- m2.data[m2.mask,]

m2 <- cache(function () {
  m2.numItems <- length(m2.items)
  m2.param <- sapply(m2.spec, function (s) c(rep(TRUE, s@numOutcomes),
                                             rep(FALSE, 5-s@numOutcomes)))
  m2 <- mxModel(model="m2",
                mxMatrix(name="ItemSpec", nrow=4, ncol=m2.numItems,
                         values=sapply(m2.spec, function(m) slot(m,'spec')),
                         free=FALSE, byrow=TRUE),
                mxMatrix(name="ItemParam", nrow=5, ncol=m2.numItems,
                         values=c(1, rep(0,4)),
                         free=m2.param),
                mxData(observed=m2.data, type="raw"),
                mxExpectationBA81(
                  ItemSpec="ItemSpec",
                  ItemParam="ItemParam",
                  GHpoints=23),
                mxFitFunctionBA81())
  m2 <- mxOption(m2, "Analytic Gradients", 'yes')
  m2 <- mxOption(m2, "Verify level", '-1')
  m2 <- mxOption(m2, "Function precision", '1.0E-5')
  m2 <- mxRun(m2)
}, 'm2')

if(0) {
  m2@matrices$ItemParam@values
  head(m2@output$ability)
}

if (0) {
  data.vs.model.booklet(function (name) {
    item.x <- match(name, m2.items)
    param <- m2@matrices$ItemParam@values[,item.x]
    data.vs.model(m2.spec[[item.x]], param, m2.data, m2@output$ability[,1], name) +
      labs(x = "familiarity",
           title = paste0(name, ", slope = ", round(param[1],2)))
  }, m2.items)
}

if (0) {
  plot.info(m2.spec, t(m2@matrices$ItemParam@values), m2.items, show.total=FALSE)
}

################################################################
i1 <- rpf.gpcm(5)
spec <- list()
spec[1:10] <- i1
items <- read.csv("sit21c/items.csv", stringsAsFactors=FALSE)
items <- prepare.items(items)

ms.scale.items <- items[,'name']
ms.scale <- espt[,ms.scale.items]
#table(apply(is.na(ms.scale), 1, sum))
score.mask <- apply(is.na(ms.scale), 1, sum) == 0
ms.scale <- ms.scale[score.mask,]
parscale.export <- function (items) {
  foo <- sapply(items, as.integer)
  ms.responses <- apply(foo, 1, paste0, collapse='')
  cat(sprintf("%s %s\n", espt[score.mask,'id'], ms.responses), file="ms.dat", sep='')
  write.csv(foo, "ms.csv")
}
if (1) {
  parscale.export(ms.scale)
} else {
  small.ms <- ms.scale
  small.ms$msAccident <- NULL
  small.ms$msPay <- NULL
  parscale.export(small.ms)  
}
parscale.export.sim <- function () {
  param <- list()
  items <- list()
  for (ix in 1:10) {
    items[[ix]] <- i1
    param[[ix]] <- c(1+ix/10, -.5+ix/10, -.25+ix/10, .5+ix/10, 2+ix/10)
  }
  foo <- rpf.sample(length(espt[score.mask,'id']), items, param)
  parscale.export(foo)
}

# compare sample distributions
if (0) {
  hist.plot <- ggplot(espt, aes(x=score)) + geom_histogram() + facet_grid(ppool ~ .)  
}

# nothing obvious here
if (0) {
  pairs(~score+born+sex+edu+rel, data=espt)  
}

##############################################
# data vs model ICC plots
# TODO add to vignette
items <- read.csv("sit21c/items.csv", stringsAsFactors=FALSE)
items <- prepare.items(items)
items$name <- ms.scale.items

if (0) {
  print.style <- theme_bw(base_size=16)
  png("summaryplot.png", width=10, height=3.5, units="in", res=150)
  grid.arrange(plot.info() + print.style, hist.plot + print.style, ncol=2)
  dev.off()
}

data.vs.model.plot <- function(id) {
  item.x <- match(id, items$id)
  param <- items[item.x, c('slope',paste0('b',1:4))]
  data.vs.model(spec[[item.x]], param, espt, items[item.x,'name']) +
    labs(title = paste0(id, ", slope = ",param[1]))
}
#data.vs.model.plot('SHAR')

if (0) {
  data.vs.model.booklet(function (ix) data.vs.model.plot(ix), items$id)  
}

if (0) {
  print.style <- theme_bw(base_size=16)
  png("shar-d-v-m.png", width=10, height=3.5, units="in", res=150)
  data.vs.model.plot('SHAR') + print.style
  dev.off()
}
if (0) {
  print.style <- theme_bw(base_size=16)
  png("pay-d-v-m.png", width=10, height=3.5, units="in", res=150)
  data.vs.model.plot('PAY') + print.style
  dev.off()
}

if (0) {
  ms.people <- espt[,c('wave', ms.scale.items, 'score','se')]
  ms.items <- items
  save(ms.people, file='ms.people.Rda', compress=FALSE)
  save(ms.items, file='ms.items.Rda', compress=FALSE)
}

######################################################
if (0) {
  scores <- espt$score
  params <- items[,c('slope',paste0('b',1:4))]
  rownames(params) <- as.character(items$name)
  responses <- ms.people[,as.character(ms.items$name)]
  
  fit <- rpf.1dim.fit(spec, params, responses, scores, 2)
  fit[order(-fit$outfit),]
  
  fit <- rpf.1dim.fit(spec, params, responses, scores, 1, wh.exact=FALSE)
  
  for (col in c('infit','infit.z','outfit','outfit.z')) {
    espt[[col]] <- fit[[col]]
  }
}
#fivenum(fit$infit)

#options(width=300)
if (0) {
  write.csv(espt, "person-fit.csv")  
}

######################################################
# openmx

if (0) {
  load("ms.openmx.rda")
  ms1.items <- as.data.frame(t(ms1.items))
  colnames(ms1.items) <- c('slope',paste0('b',1:4))
  ms1.items$id <- items$id
  ms1.items$name <- items$name
  
  omx.data.vs.model.plot <- function(id) {
    item.x <- match(id, ms1.items$id)
    param <- ms1.items[item.x, c('slope',paste0('b',1:4))]
    data.vs.model(spec[[item.x]], param, espt, ms1.items[item.x,'name']) +
      labs(title = paste0(id, ", slope = ",param[1]))
  }
  data.vs.model.booklet(function (ix) omx.data.vs.model.plot(ix), ms1.items$id)  
  
  cor(ms1.scores[,1], scores[!is.na(scores)])
  cor(items$slope, ms1.items$slope)
  cor(c(as.matrix(items[,c(paste0('b',1:4))])), c(as.matrix(ms1.items[,c(paste0('b',1:4))])))
}
