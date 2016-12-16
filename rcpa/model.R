library(BradleyTerry2)
library(plyr)
library(stringr)
library(jsonlite)

# TODO:
# translate to stan
# estimate +1 +2 thresholds across all PAs
# estimate composite score & per facet weights

rcd <- read.csv("rawData.csv")

palist <- unique(c(as.character(rcd$pa1), as.character(rcd$pa2)))

extractFacet <- function(facet) {
  tmp <- ddply(rcd, ~pa1 + pa2, function(df) {
    data.frame(pa1=df[1,'pa1'], pa2=df[1,'pa2'],
               win1=sum(df[df[[facet]]>0,facet]),
               win2=-sum(df[df[[facet]]<0,facet]))
  })
  tmp$pa1 <- factor(as.character(tmp$pa1), levels=palist)
  tmp$pa2 <- factor(as.character(tmp$pa2), levels=palist)
  tmp <- tmp[tmp$win1 != 0 | tmp$win2 != 0,]
  tmp
}

result <- matrix(NA, length(palist), ncol(rcd)-2,
                 dimnames=list(palist, colnames(rcd)[-1:-2]))

refcat <- "weightlifting"
result[refcat,] <- 0

for (facet in colnames(rcd)[-1:-2]) {
  facet.data <- extractFacet(facet)
  f1 <- BTm(cbind(win1, win2), pa1, pa2, ~ skill,
            id="skill", refcat=refcat, data = facet.data)
  cmap <- str_replace(names(coef(f1)), "^skill", "")
  result[cmap,facet] <- coef(f1)
}

if(0) {
  extractFacet('waiting')
  result[order(result[,'waiting']),'waiting']
}

sink("pa-browser/rcpa-data.js")
cat("var RCPA_DATA=")
cat(toJSON(result, matrix="columnmajor", digits=0))
cat(";", fill=TRUE)
cat("var RCPA_FACETS=")
cat(toJSON(colnames(result)))
cat(";", fill=TRUE)
cat("var RCPA_PA=")
cat(toJSON(rownames(result)))
cat(";", fill=TRUE)
sink()
