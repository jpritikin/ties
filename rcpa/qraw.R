# R --no-save -f qraw.R --args pa1 pa2 facet

rcd <- read.csv("rawData.csv")
palist <- sort(unique(c(as.character(rcd$pa1), as.character(rcd$pa2))))
facetNames <- colnames(rcd[-1:-4])

args <- commandArgs(trailingOnly = TRUE)
#args <- c('soccer', 'basketball', 'predict')

pa1 <- which(grepl(paste0("^",args[1]), palist))
if (is.na(pa1)) stop(paste("Cannot match", args[1]))
if (length(pa1) > 1) stop(paste("PA1", args[1], "matched more than once"))

pa2 <- which(grepl(paste0("^",args[2]), palist))
if (is.na(pa2)) stop(paste("Cannot match", args[2]))
if (length(pa2) > 1) stop(paste("PA2", args[2], "matched more than once"))

f1 <- which(grepl(paste0("^",args[3]), facetNames))
if (is.na(f1)) stop(paste("Cannot match", args[3]))
if (length(f1) > 1) stop(paste("Facet", args[3], "matched more than once"))

facet <- facetNames[f1]
got <- rcd[(rcd$pa1==palist[pa1] & rcd$pa2==palist[pa2]) |
           (rcd$pa1==palist[pa2] & rcd$pa2==palist[pa1]),
           c('recno','pa1',facet,'pa2')]
got[[facet]] <- factor(got[[facet]], labels = c('<<','<','=','>','>>'),
                       levels = rev(-2:2))
print(got)
