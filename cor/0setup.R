part2 <- read.csv("Sheet_2.csv", stringsAsFactors=FALSE, na.strings=c(""))
for (rep in 1:9) { part2[[1]] <- NULL }
cdat <- cbind(read.csv("Sheet_1.csv", stringsAsFactors=FALSE, na.strings=c("")), part2)

for (col in c("RespondentID", "CollectorID", "Email.Address", "First.Name", "LastName", "Custom.Data")) {
  cdat[[col]] <- NULL
}

mergeTo1 <- function(rows) {
  got <- cdat[rows[length(rows)],]
  for (cx in 5:ncol(cdat)) {
    mask <- !is.na(cdat[rows,cx])
    filled <- sum(mask)
    if (filled == 1) {
      got[cx] <- cdat[rows[mask],cx]
    } else if (filled > 1) {
      entered <- cdat[rows[mask],cx]
      if (all(entered[1] == entered)) {
        got[cx] <- entered[1]
      } else {
        got[cx] <- entered[length(entered)]  # take last entry
      }
    }
  }
  got
}

dups <- names(which(table(cdat[[4]]) != 1))
merged <- NULL
for (d1 in dups) {
  merged <- rbind(merged, mergeTo1(which(d1 == cdat[[4]])))
}

clean <- rbind(cdat[is.na(match(cdat[[4]], dups)),], merged)

for (rep in 1:2) {
  clean[[3]] <- NULL
}

write.csv(clean, "data-anon.csv", row.names=FALSE)
