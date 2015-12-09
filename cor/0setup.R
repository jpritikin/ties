mergeTo1 <- function(rows, cdat) {
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

processDate <- function(when) {
  s2 <- paste0("Sheet_2-", when, ".csv")
  part2 <- read.csv(s2, stringsAsFactors=FALSE, na.strings=c(""))
  if (colnames(part2)[9] != "Custom.Data") stop("Layout changed")
  for (rep in 1:9) { part2[[1]] <- NULL }
  s1 <- paste0("Sheet_1-", when, ".csv")
  cdat <- cbind(read.csv(s1, stringsAsFactors=FALSE, na.strings=c("")), part2)
  
  for (col in c("RespondentID", "CollectorID", "Email.Address", "First.Name", "LastName", "Custom.Data")) {
    cdat[[col]] <- NULL
  }
  
  dups <- names(which(table(cdat[[4]]) != 1))
  merged <- NULL
  for (d1 in dups) {
    merged <- rbind(merged, mergeTo1(which(d1 == cdat[[4]]), cdat))
  }
  
  clean <- rbind(cdat[is.na(match(cdat[[4]], dups)),], merged)
  
  if (colnames(clean)[3] != "IP.Address") stop("Layout changed")
  for (rep in 1:2) {
    clean[[3]] <- NULL
  }
  
  write.csv(clean, paste0("data-", when, ".csv"), row.names=FALSE)
}

processDate("201405")
processDate("201410")
processDate("201412")
processDate("201509")
processDate("201510")
processDate("201512")
