args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Specify one participant ID after --args")

{
wd <- setwd("..")
source("measures.R")
source("cms-score.R")
setwd(wd)
}

targetID <- as.integer(args[1])

df <- NULL
for (wave in 1:4) {
  raw <- read.csv(sprintf("wave%d-anon.csv", wave), stringsAsFactors=FALSE)
  r1 <- subset(raw, id == targetID)
  if (wave == 1) {
    for (cx in seq(5,3,-1)) r1[[cx]] <- NULL
    for (cx in seq(21,17,-1)) r1[[cx]] <- NULL
  }
  colnames(r1)[3:16] <- paste0("em", 1:14)
  
  offset <- 17
  cmsCol <- r1[,offset:(offset+24-1)]
  cmsCol <- cbind(NA,NA,NA,NA,NA,cmsCol)
  cms <- prep.cms201410(cmsCol)
  o1 <- cbind(r1[1:16], cms)
  df <- rbind(df, o1)
}

#write.csv(df, file=sprintf("raw-%d.csv", targetID), row.names=FALSE)
rownames(df) <- 1:nrow(df)
write.csv(t(df), file=sprintf("raw-%d.csv", targetID))
