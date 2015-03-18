# This handles the non-anonymous data

uidStart <- 1100
maxPerWave <- 140 + 80

idmap <- read.csv("idmap.csv", stringsAsFactors=FALSE)

labmap <- read.csv("labmap.csv", stringsAsFactors=FALSE)
labDetail <- strsplit(labmap$Custom.Value, "; ", fixed=TRUE)
labmap$labTime <- sapply(labDetail, function(elem) elem[[1]])
labmap$labTA <- factor(sapply(labDetail, function(elem) elem[[3]]))
for (col in c("First.Name", "Last.Name", "Custom.Value")) { labmap[[col]] <- NULL }
labmap$labTA <- unclass(labmap$labTA)
labmap <- labmap[match(idmap$email, labmap$Email),]

wave1 <- read.csv("wave1-id.csv", stringsAsFactors=FALSE)
wave1[[13]] <- NULL    # lab section
wave1 <- rbind(wave1, read.csv("ppool1-id.csv", stringsAsFactors=FALSE))

wave1$id <- idmap[match(wave1[,'Email.Address'], idmap$email), 'id']
wave1$time <- 1
wave1$uid <- uidStart:(uidStart + nrow(wave1) - 1)
wave1 <- cbind(wave1, labmap[,c("labTA", "labTime")])

discardCols <- c("Email.Address", "CollectorID", "RespondentID", "IP.Address", "First.Name",
                 "LastName", "Custom.Data")

for (c in discardCols) wave1[[c]] <- NULL
write.csv(wave1, "wave1-anon.csv", row.names=FALSE)

# ----

waven <- read.csv("waven-id.csv", stringsAsFactors=FALSE)
waven <- rbind(waven, read.csv("ppooln-id.csv", stringsAsFactors=FALSE))
waven$id <- idmap[match(waven[,'Email.Address'], idmap$email), 'id']

if (0) {
  table(waven$CollectorID)
}
Collector1 <- c("63247047","63990099","64589357","65247444")
Collector2 <- c("64759933", "64960121", "TBA")

for (wn in 1:4) {
  wave2 <- subset(waven, CollectorID == Collector1[wn] | CollectorID == Collector2[wn])
  wave2 <- wave2[match(wave1$id, wave2$id),]
  wave2$time <- wn+1
  baseuid <- uidStart+wn*maxPerWave
  wave2$uid <- baseuid:(baseuid + nrow(wave2) - 1)
  
  for (c in discardCols) wave2[[c]] <- NULL
  write.csv(wave2, sprintf("wave%d-anon.csv", 1+wn), row.names=FALSE)
}
