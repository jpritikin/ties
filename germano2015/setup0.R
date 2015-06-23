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
  cid <- table(waven$CollectorID)
  df <- data.frame(CollectorID=names(cid),
                   start=waven[match(names(cid), waven$CollectorID), 'StartDate'])
  df$count <- sapply(df$CollectorID, function(cid) sum(cid == waven$CollectorID))
  df
}
Collector1 <- c("63247047","63990099","64589357","65247444","66271723",
                "68288151", "68788201", "69491717")  #relb
Collector2 <- c("64759933", "64960121", "64960311","64960615", "64960712") #ppool
if (0) {
  subset(df, CollectorID %in% Collector1)
  subset(df, CollectorID %in% Collector2)
}

for (wn in 1:9) {
  wave2 <- subset(waven, CollectorID == Collector1[wn] | CollectorID == Collector2[wn])
  wave2 <- wave2[match(wave1$id, wave2$id),]
  wave2$time <- wn+1
  baseuid <- uidStart+wn*maxPerWave
  wave2$uid <- baseuid:(baseuid + nrow(wave2) - 1)
  
  for (c in discardCols) wave2[[c]] <- NULL
  write.csv(wave2, sprintf("wave%d-anon.csv", 1+wn), row.names=FALSE)
}

if (0) {
  m3 <- c(802L, 810L, 825L, 833L, 834L, 839L, 840L, 842L, 843L, 851L,  852L, 856L,
          859L, 860L, 861L, 864L, 865L, 877L, 878L, 885L, 886L,  896L, 898L, 900L,
          904L, 905L, 906L, 907L, 908L, 909L, 911L, 915L,  918L, 922L, 923L, 924L, 925L, 929L, 930L)
  e3 <- idmap[match(m3, idmap$id), 'email']
#  e3 <- e3[-match("vb4bv@virginia.edu", e3)]  # exclude after March 27
  write.table(e3, file="e6.csv", quote=FALSE, row.names=FALSE)
}
if (0) {
  m9 <- c(810L, 833L, 840L, 842L, 843L, 856L, 859L, 860L, 877L, 885L,  898L, 900L,
          905L, 906L, 907L, 908L, 909L, 915L, 918L, 924L, 925L,  929L, 930L)
  e9 <- idmap[match(m9, idmap$id), 'email']
  e9
  length(e9)
}
