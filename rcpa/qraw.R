# R --no-save -f qraw.R --args pa1 facet
# R --no-save -f qraw.R --args pa1 pa2 facet

rcd <- read.csv("rawData.csv")
palist <- sort(unique(c(as.character(rcd$pa1), as.character(rcd$pa2))))
facetNames <- colnames(rcd[-1:-4])

args <- commandArgs(trailingOnly = TRUE)
#args <- c('soccer', 'basketball', 'predict')

ax <- 1L
pa1 <- which(grepl(paste0("^",args[ax]), palist))
if (is.na(pa1)) stop(paste("Cannot match", args[ax]))
if (length(pa1) > 1) stop(paste("PA1", args[ax], "matched more than once"))

pa2 <- NULL
if (length(args) == 3) {
    ax <- ax + 1L
    pa2 <- which(grepl(paste0("^",args[ax]), palist))
    if (is.na(pa2)) stop(paste("Cannot match", args[ax]))
    if (length(pa2) > 1) stop(paste("PA2", args[ax], "matched more than once"))
}

ax <- ax + 1L
f1 <- which(grepl(paste0("^",args[ax]), facetNames))
if (is.na(f1)) stop(paste("Cannot match", args[ax]))
if (length(f1) > 1) stop(paste("Facet", args[ax], "matched more than once"))

facet <- facetNames[f1]
if (is.null(pa2)) {
    mask <- rcd$pa1==palist[pa1] | rcd$pa2==palist[pa1]
} else {
    mask <- (rcd$pa1==palist[pa1] & rcd$pa2==palist[pa2]) |
        (rcd$pa1==palist[pa2] & rcd$pa2==palist[pa1])
}
got <- rcd[mask,
           c('recno','pa1',facet,'pa2')]
got[[facet]] <- factor(got[[facet]], labels = c('<<','<','=','>','>>'),
                       levels = rev(-2:2))
print(got)
