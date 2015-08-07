library(ggplot2)
library(reshape2)
library(plyr)
source("lab.R")

tformat <- "%m/%d/%Y %H:%M:%S"
reftime <- strptime("01/12/2015 12:00:00", tformat)
labGrid <- buildLabTable(reftime)

labData <- NULL

numWaves <- 9

wide <- NULL
for (wave in 1:numWaves) {
  dat <- read.table(sprintf("prep%d.csv", wave), header=TRUE, stringsAsFactors=FALSE)
  if (wave == 1) {
    for (row in which(!is.na(dat$labTime))) {
      labData <- rbind(labData,
                       cbind(x=labGrid[dat[row, 'labTime'],],
                             dat[row, c('id','labTA')]))
    }
    wide <- dat
  } else {
    wide <- cbind(wide, dat)
  }
}
labData$labTA <- factor(labData$labTA)

mask <- apply(!is.na(wide[,which(colnames(wide) == "id")]), 1, sum)
wide <- wide[mask >= 5,]  # minimum number of measurements per participant
# sum(mask >= 5)
if (0) {
  inviteId <- wide[!is.na(wide$labTA), 'id']
  length(inviteId)
  cat(deparse(inviteId))
}

erange <- apply(wide[,which(colnames(wide) == "ties")], 1,
                function(x) range(c(0,x), na.rm=TRUE))
wide <- wide[order(erange[1,] - erange[2,]),]

measures <- c("training", "ties", "envMastery")
perOccasionCol <- c("id", measures)
cmap <- match(colnames(wide), c("end",perOccasionCol))

labIds <- unique(labData$id)

long <- NULL
for (wave in 1:numWaves) {
  tm <- wide[[ which(cmap==1)[wave] ]]
  occasion <- data.frame(as.numeric(strptime(tm, tformat) - reftime))
  for (mx in 1:length(perOccasionCol)) {
    occasion <- cbind(occasion, wide[[ which(cmap==(mx+1))[wave] ]])
  }
  colnames(occasion) <- c("tm", perOccasionCol)
  lo <- melt(occasion, id.vars=c("tm", "id"))
  lo$hasLab <- lo$id %in% labIds
  long <- rbind(long, lo)
}

long$value[long$variable=="envMastery"] <- scale(long$value[long$variable=="envMastery"])

tmRange <- range(long$tm, na.rm=TRUE)
mRange <- range(long$value, na.rm=TRUE)

unlink("gen/page-*.pdf")
dir.create("gen", showWarnings =FALSE)

page <- 1

for (vx in levels(long$variable)) {
  pdf(file=sprintf("gen/page-%03d.pdf", page), width=11, height=8.5)
  longIndicator <- subset(long, variable==vx)
  longIndicator$bin <- cut(longIndicator$tm, 7)
  meanIndicator <- ddply(longIndicator, ~hasLab+bin, function (df) {
    c(value=mean(df$value, na.rm=TRUE), tm=mean(df$tm, na.rm=TRUE))
  })
  meanIndicator <- subset(meanIndicator, !is.na(value))
  pl <- ggplot() + geom_line(aes(tm, value, group=id),
                             data=subset(longIndicator, !is.na(value)),
                             alpha=.1, size=5) +
    facet_wrap(~hasLab) +
    geom_line(aes(tm, value), data=meanIndicator, alpha=.5, color="red", size=4) +
    labs(title=paste(vx,"conditional on lab participation")) +
    xlim(tmRange[1],tmRange[2]) + ylim(mRange[1], mRange[2])
  print(pl)
  dev.off()
  page <- page + 1L
}

for (.id in unique(long$id)) {
  if (is.na(.id)) next
  lab1 <- subset(labData, id==.id)
  
  pl <- ggplot(subset(long, id==.id & !is.na(value)),
               aes(tm, value, group=variable, color=variable)) +
    geom_line() + geom_point() +
    labs(title=paste("ID =", .id, "LabTA =", ifelse(nrow(lab1), lab1$labTA[1], ""))) +
    xlim(tmRange[1],tmRange[2]) + ylim(mRange[1], mRange[2])
  
  if (FALSE && nrow(lab1)) {
    pl <- pl + geom_vline(data=lab1,
                          aes(xintercept = x), color="yellow")
  }
    pdf(file=sprintf("gen/page-%03d.pdf", page), width=11, height=8.5)
  print(pl)
  dev.off()
  page <- page + 1
}

system(paste("pdfunite  gen/page-* iplots.pdf"))
