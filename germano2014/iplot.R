library(ggplot2)
library(reshape2)
library(plyr)

w1 <- read.table("prep1.csv", header=TRUE, stringsAsFactors=FALSE)
w2 <- read.table("prep2.csv", header=TRUE, stringsAsFactors=FALSE)
w3 <- read.table("prep3.csv", header=TRUE, stringsAsFactors=FALSE)

alle <- cbind(w1$event, w2$event, w3$event)
alleMM <- apply(alle, 1, function(r) c(max(c(-100,r), na.rm=TRUE), min(c(100,r), na.rm = TRUE)))
alleRange <- alleMM[1,] - alleMM[2,]
mask <- !(abs(alleRange + 200) < .01 | alleRange == 0)
alleRange <- alleRange[mask]

w1 <- w1[mask,]
w2 <- w2[mask,]
w3 <- w3[mask,]
tformat <- "%m/%d/%Y %H:%M:%S"
reftime <- strptime("01/14/2014 12:00:00", tformat)
minTime <- as.numeric(min(strptime(w1$end, tformat) - reftime, na.rm = TRUE))
maxTime <- as.numeric(max(strptime(w3$end, tformat) - reftime, na.rm = TRUE))

measures <- c("event", "barrier", "reflection",  "rumination",
  "psqi", "dass.d", "dass.a", "dass.s", "dass.na")

wide2long <- function(r) {
  cbind(t=as.numeric(strptime(r$end, tformat) - reftime),
        melt(r[,measures], id.vars=c()))
}
df <- rbind(ddply(w1, ~id, wide2long),
            ddply(w2, ~id, wide2long),
            ddply(w3, ~id, wide2long))
df <- df[!is.na(df$value),]

sleepMask <- df$variable=="psqi"
df[sleepMask,"value"] <- -df[sleepMask,"value"]

levels(df$variable)[levels(df$variable)=="event"] <- "mental silence intensity"
levels(df$variable)[levels(df$variable)=="psqi"] <- "sleep quality"
levels(df$variable)[match(c("dass.d", "dass.a", "dass.s", "dass.na"), levels(df$variable))] <-
  c("depression", "anxiety", "stress", "negative affect")

for (m in levels(df$variable)) {
  df[df$variable==m,"value"] <- scale(df[df$variable==m,"value"])
}
mRange <- ddply(df, ~variable, function(r) c(min=min(r$value), max=max(r$value)))

unlink("gen/page-*.pdf")
dir.create("gen", showWarnings =FALSE)

page <- 1
for (row in order(-alleRange)) {
  .id <- w1[row,'id']
  pl <- ggplot(subset(df, id==.id),
               aes(t, value, group=variable)) + geom_line() + geom_point() + facet_wrap(~variable) +
    labs(title=paste("ID =", .id)) + xlim(minTime,maxTime) + ylim(min(mRange$min), max(mRange$max))
  
  pdf(sprintf("gen/page-%03d.pdf", page))
  print(pl)
  dev.off()
  page <- page + 1
}

system(paste("pdfunite  gen/page-* iplots.pdf"))
