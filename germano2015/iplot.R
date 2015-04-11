library(ggplot2)
library(reshape2)
library(plyr)

tformat <- "%m/%d/%Y %H:%M:%S"
reftime <- strptime("01/12/2015 12:00:00", tformat)

labWeeks <- paste(c("01/12/2015", "01/19/2015", "01/26/2015",
                    "02/02/2015", "02/09/2015", "02/16/2015",
                    "02/23/2015", "03/02/2015", "03/16/2015",
                    "03/23/2015", "03/30/2015", "04/06/2015",
                    "04/13/2015", "04/20/2015"),
                  "12:00:00")

labLabels <- c("Th 2:00PM - 2:50PM",  "Th 3:00PM - 3:50PM", "Th 4:00PM - 4:50PM",
  "Fr 11:00AM - 11:50AM", "Fr 12:00PM - 12:50PM", "Fr 1:00PM - 1:50PM",
  "Fr 2:00PM - 2:50PM", "Fr 3:00PM - 3:50PM","Fr 4:00PM - 4:50PM")

labStart <- c("01/15/2015 14:00:00", "01/15/2015 15:00:00", "01/15/2015 16:00:00",
              "01/16/2015 11:00:00", "01/16/2015 12:00:00", "01/16/2015 13:00:00",
              "01/16/2015 14:00:00", "01/16/2015 15:00:00", "01/16/2015 16:00:00")

labGrid <- sapply(labWeeks, function(lw)
  as.numeric(strptime(lw, tformat) + (strptime(labStart, tformat) - reftime) - reftime))
rownames(labGrid) <- labLabels

labData <- NULL

numWaves <- 6

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
wide <- wide[mask >= 3,]  # minimum number of measurements per participant
# sum(mask >= 5)
if (0) {
  cat(deparse(wide[!is.na(wide$labTA), 'id']))
}

erange <- apply(wide[,which(colnames(wide) == "event")], 1,
                function(x) range(c(0,x), na.rm=TRUE))
wide <- wide[order(erange[1,] - erange[2,]),]

measures <- c("barrier", "training", "event", "envMastery")
perOccasionCol <- c("id", measures)
cmap <- match(colnames(wide), c("end",perOccasionCol))

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
