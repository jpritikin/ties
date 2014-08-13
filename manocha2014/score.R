{
  wd <- setwd("..")
  source("measures.R")
  source("cms-score.R")
  setwd(wd)
}

raw <- read.csv("clean.csv", stringsAsFactors=FALSE, header=FALSE)
score <- data.frame(id=raw[,1],
                    born=raw[,2],
                    edu=raw[,3],
                    sex=raw[,4],
                    rel=raw[,5])

got <- score.rrq(raw[6:(6+24-1)])
for (k in names(got)) { score[[paste(k,"1",sep="")]] <- got[[k]] }

score$psqi1 <- score.psqi(raw[30:(30+19-1)])

got <- score.dass(raw[49:(49+21-1)])
for (k in names(got)) { score[[paste(k,"1",sep="")]] <- got[[k]] }

cms <- prep.cms201309(raw[70:92])
got <- cms.score(cms)
for (k in colnames(got)) { score[[paste(k,"1",sep="")]] <- got[,k] }

got <- score.5fMindfulness(raw[93:(93+39-1)])
for (k in names(got)) { score[[paste(k,"1",sep="")]] <- got[[k]] }

# 2nd occasion

got <- score.rrq(raw[132:(132+24-1)])
for (k in names(got)) { score[[paste(k,"2",sep="")]] <- got[[k]] }

score$psqi2 <- score.psqi(raw[156:(156+19-1)])

got <- score.dass(raw[175:(175+21-1)])
for (k in names(got)) { score[[paste(k,"2",sep="")]] <- got[[k]] }

cms <- prep.cms201309(raw[196:218])
got <- cms.score(cms)
for (k in colnames(got)) { score[[paste(k,"2",sep="")]] <- got[,k] }

got <- score.5fMindfulness(raw[219:(219+39-1)])
for (k in names(got)) { score[[paste(k,"2",sep="")]] <- got[[k]] }

write.csv(score, file="scored.csv", row.names = FALSE)
