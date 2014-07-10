# This handles the non-anonymous data

idStart <- 500
uidStart <- 200

wave1 <- read.csv("wave1.csv", stringsAsFactors=FALSE)

idmap <- NULL
if (0) {
  email <- wave1[,'Email.Address']
  idmap <- data.frame(email=email, id=idStart:(idStart + length(email) - 1))
  write.csv(idmap, "idmap.csv", row.names=FALSE)
} else {
  idmap <- read.csv("idmap.csv", stringsAsFactors=FALSE)
}

wave1$id <- idmap[match(wave1[,'Email.Address'], idmap$email), 'id']
wave1$time <- 1
wave1$uid <- uidStart:(uidStart + nrow(wave1) - 1)

discardCols <- c("Email.Address", "CollectorID", "RespondentID", "IP.Address", "First.Name",
                 "LastName", "Custom.Data",
                 "Do.you.have.any.comments.about.this.survey..If.you.have.questions.for.which.you.wish..to.receive.a.prompt.answer..send.me.email.jpritikin.virginia.edu....Open.Ended.Response")

if (0) {
  # comments
  write.csv(wave1[nzchar(wave1[,96]),c(96:97)], file="comments.csv", row.names=FALSE)
}

wave23 <- read.csv("wave2+3.csv", stringsAsFactors=FALSE)
wave23$id <- idmap[match(wave23[,'Email.Address'], idmap$email), 'id']
wave23$uid <- max(wave1$uid):(max(wave1$uid) + nrow(wave23) - 1)

# 50048281,"wave2",Feb 14 2014  5:43PM
# 52504406,"wave3",Apr 20 2014 10:21PM

wave2 <- subset(wave23, CollectorID == "50048281")
wave2$time <- 2
wave3 <- subset(wave23, CollectorID == "52504406")
wave3$time <- 3

wave2 <- wave2[match(wave1$id, wave2$id),]
wave3 <- wave3[match(wave1$id, wave3$id),]

for (c in discardCols) {
  wave1[,c] <- NULL
  wave2[,c] <- NULL
  wave3[,c] <- NULL
}

write.csv(wave1, "wave1-anon.csv", row.names=FALSE)
write.csv(wave2, "wave2-anon.csv", row.names=FALSE)
write.csv(wave3, "wave3-anon.csv", row.names=FALSE)
