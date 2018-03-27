library(OpenMx)
source("modelUtil.R")

rcd <- read.csv(paste0('.',"/rawData.csv"), stringsAsFactors=FALSE)
whitelist <- getWhiteList(rcd)
rcd <- rcd[rcd$pa1 %in% whitelist & rcd$pa2 %in% whitelist,]

demogr <- read.csv(paste0('./', 'demogr.csv'), stringsAsFactors = FALSE,na.strings='')

mask <- with(demogr, paste(source, recno, sep=':')) %in% rcd$recno
demogr <- demogr[mask,]

SexItem <- c("Female", "Male")
demogr[['sex']] <- mxFactor(demogr[['sex']], levels=SexItem, labels=tolower(SexItem))
demogr[['age']] <- demogr[['recDate']] - demogr[['birthyear']]

capwords <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
    {s <- substring(s, 2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
  }
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

ctbl <- table(demogr[['country']])
demogr[demogr[['country']] %in% names(ctbl)[ctbl<=3], 'country'] <- 'other'

ctbl <- table(demogr[['country']])
cname <- capwords(names(ctbl))
cname[cname=='Usa'] <- 'USA'

ctbl <- ctbl[-which(names(ctbl)=='other')]
cname <- cname[-which(cname=='Other')]

demogr[['country']] <- mxFactor(demogr[['country']], levels=c(names(ctbl),'other'), labels=c(cname, 'other'))

EduItem = c('Less than high school degree',
            'High school degree or equivalent (e.g., GED)',
            'Some college but no degree',
            'Associate degree',
            'Bachelor degree',
            'Graduate degree')
demogr[['education']] <- mxFactor(demogr[['edu']], levels = EduItem,
  labels=tolower(EduItem), exclude = '')
demogr[['edu']] <- NULL

demogr[['source']] <- mxFactor(demogr[['source']], labels=c('public','MTurk'),
                               levels=c('public','mturk'),
                               exclude='')
demogr$channel <- demogr$source  # alternate name

save(demogr, file="demogr.rda")
