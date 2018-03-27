source("modelUtil.R")

rcd <- loadRawData()
ss <- calcSampleSize(rcd)
print(ss[names(ss)[order(ss)]])

rcd <- loadWhitelistRawData()
length(extractPalist(rcd))
nrow(rcd)
