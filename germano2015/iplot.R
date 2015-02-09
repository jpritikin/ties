library(ggplot2)
library(reshape2)
library(plyr)

numWaves <- 4

wide <- NULL
for (wave in 1:numWaves) {
  dat <- read.table(sprintf("prep%d.csv", wave), header=TRUE, stringsAsFactors=FALSE)
  if (wave == 1) {
    wide <- dat
  } else {
    wide <- cbind(wide, dat)
  }
}

mask <- apply(!is.na(wide[,which(colnames(wide) == "id")]), 1, sum)
wide <- wide[mask >= 3,]

erange <- apply(wide[,which(colnames(wide) == "event")], 1, range)
wide <- wide[order(erange[1,] - erange[2,]),]

measures <- c("barrier", "training", "event", "envMastery")
perOccasionCol <- c("id", measures)
cmap <- match(colnames(wide), c("end",perOccasionCol))

tformat <- "%m/%d/%Y %H:%M:%S"
reftime <- strptime("01/12/2015 12:00:00", tformat)

long <- NULL
for (wave in 1:numWaves) {
  tm <- wide[[ which(cmap==1)[wave] ]]
  occasion <- data.frame(as.numeric(strptime(tm, tformat) - reftime))
  for (mx in 1:length(perOccasionCol)) {
    occasion <- cbind(occasion, wide[[ which(cmap==(mx+1))[wave] ]])
  }
  colnames(occasion) <- c("tm", perOccasionCol)
  lo <- melt(occasion, id.vars=c("tm", "id"))
  long <- rbind(long, lo)
}

long$value[long$variable=="envMastery"] <- scale(long$value[long$variable=="envMastery"])

tmRange <- range(long$tm, na.rm=TRUE)
mRange <- range(long$value, na.rm=TRUE)

unlink("gen/page-*.pdf")
dir.create("gen", showWarnings =FALSE)

spam <- c(881)
page <- 1
for (.id in unique(long$id)) {
  if (is.na(.id) || .id %in% spam) next
  pl <- ggplot(subset(long, id==.id & !is.na(value)),
               aes(tm, value, group=variable, color=variable)) +
    geom_line() + geom_point() + labs(title=paste("ID =", .id)) +
    xlim(tmRange[1],tmRange[2]) + ylim(mRange[1], mRange[2])
  
  pdf(file=sprintf("gen/page-%03d.pdf", page), width=11, height=8.5)
  print(pl)
  dev.off()
  page <- page + 1
}

system(paste("pdfunite  gen/page-* iplots.pdf"))
