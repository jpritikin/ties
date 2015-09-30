safe.ordered <- function(df, levels, old.levels=c()) { # replace with mxFactor TODO
  if (!missing(old.levels)) {
    if (any(!is.na(match(old.levels, levels)))) stop("levels and old.levels overlap")
    df[which(!is.na(match(df, old.levels)))] <- ''
  }
  levels <- tolower(levels)
  df <- tolower(df)
  no.match <- !is.na(df) & is.na(match(df, c('',levels)))
  if (any(no.match)) {
    stop(paste("Unknown levels:", paste(unique(df[no.match]), collapse=" ")))
  }
  ordered(df, levels=levels)
}

mean.or.na <- function(mat, n.min) {
  mat <- as.data.frame(lapply(mat, unclass))
  size <- apply(mat, 1, function(r) sum(!is.na(r)))
  score <- apply(mat, 1, function(r) sum(r, na.rm=TRUE))
  score[size < n.min] <- NA
  score / size
}

score.ipipBig5 <- function(raw) {
  if (ncol(raw) != 50) stop("Expecting 50 columns")
  
  IPIPItem <- c('Very Inaccurate',
                'Moderately Inaccurate',
                'Neither Accurate Nor Inaccurate',
                'Moderately Accurate',
                'Very Accurate')
  for (col in 1:50) {
    lev <- NULL
    if ((col-1) %% 10 <= 4) {
      lev <- IPIPItem
    } else {
      lev <- rev(IPIPItem)
    }
    raw[[col]] <- mxFactor(raw[[col]], levels=lev, exclude='')
  }
  minItems <- 8
  list(neurotic= mean.or.na(raw[,01:10], minItems),
       extravert=mean.or.na(raw[,11:20], minItems),
       open=     mean.or.na(raw[,21:30], minItems),
       agreeable=mean.or.na(raw[,31:40], minItems),
       consc=    mean.or.na(raw[,41:50], minItems))
}

score.panas <- function(raw) {
  if (ncol(raw) != 20) stop("Expecting 20 columns")
  
  PANASItem <- c("Very Slightly or Not at All",  "A Little",	"Moderately",	"Quite a Bit",	"Extremely")
  
  raw <- mxFactor(raw, levels=PANASItem, exclude='')
  
  list(posAffect=mean.or.na(raw[,c(1,3,5,9,10,12,14,16,17,19)], 8),
       negAffect=mean.or.na(raw[,c(2,4,6,7, 8,11,13,15,18,20)], 8))
}

score.maas <- function(raw) {
  if (ncol(raw) != 15) stop("Expecting 15 columns")
  
  MAASItem <- c("Almost Always",  "Very Frequently",	"Somewhat Frequently",	"Somewhat Infrequently",
                "Very Infrequently",	"Almost Never")

  for (col in 1:15) {
    raw[[col]] <- ordered(raw[[col]], levels=MAASItem)
  }
  
  mean.or.na(raw[,1:15], 13)
}

prep.ryff9 <- function(raw) {
  if (ncol(raw) != 6 * 9) stop("Expected 54 columns")

  RyffItem <- c("Strongly Disagree",  "Disagree Somewhat",	"Disagree Slightly",
                "Agree Slightly",	"Agree Somewhat",	"Strongly Agree")
  
  for (col in 1:54) {
    lev <- RyffItem
    if (col %in% c(3,5,7,8,
                   11,12,14,17,
                   19,20,22,24,26,27,
                   29,30,32,33,35,
                   37,38,39,40,41,45,
                   48,51,52)) {
      lev <- rev(RyffItem)
    }
    raw[[col]] <- mxFactor(raw[[col]], levels=lev, exclude='')
  }

  colnames(raw)[01:09] <- paste0("autonomy",  c(2,3,4,5,6, 9,10,11,14))
  colnames(raw)[10:18] <- paste0("envm",      c(1,2,3,4,5, 7, 9,13,14))
  colnames(raw)[19:27] <- paste0("pergrowth", c(1,4,5,6,9,10,11,13,14))
  colnames(raw)[28:36] <- paste0("posrel",    c(1,2,3,4,6, 8, 9,10,12))
  colnames(raw)[37:45] <- paste0("purpose",   c(2,3,5,6,7, 8, 9,10,11))
  colnames(raw)[46:54] <- paste0("selfaccept",c(1,2,3,5,6, 7,10,12,13))

  raw
}

score.ryff9 <- function(raw) {
    raw <- prep.ryff9(raw)

  list(autonomy    = mean.or.na(raw[,01:09], 7),
       envMastery  = mean.or.na(raw[,10:18], 7),
       perGrowth   = mean.or.na(raw[,19:27], 7),
       posRelation = mean.or.na(raw[,28:36], 7),
       lifePurpose = mean.or.na(raw[,37:45], 7),
       selfAccept  = mean.or.na(raw[,46:54], 7))
}

prep.ryff.envMastery14 <- function(raw) {
  if (ncol(raw) != 14) stop("Expected 14 columns")
  
  RyffItem <- c("Strongly Disagree",  "Disagree Somewhat",	"Disagree Slightly",
                "Agree Slightly",	"Agree Somewhat",	"Strongly Agree")

  for (col in 1:14) {
    lev <- RyffItem
    if (col %in% c(2,3,5,8,11,13)) {
      lev <- rev(RyffItem)
    }
    raw[[col]] <- mxFactor(raw[[col]], levels=lev, exclude='')
  }

  colnames(raw)[1:14] <- paste0("envm", 1:14)

  raw
}

score.ryff.envMastery14 <- function(raw) {
    raw <- prep.ryff.envMastery14(raw)
    
  mean.or.na(raw[,1:14], 12)
}

# See Reynolds (1982) and Crowne & Marlowe (1960)
score.mcFormC <- function(raw) {
  if (ncol(raw) != 13) stop("Expected 13 columns")
  
  MCItem <- c("False", "True")
  for (col in 1:13) {
    lev <- MCItem
    if (col %in% c(1,2,3,4,6,8,11,12)) {
      lev <- rev(MCItem)
    }
    raw[[col]] <- mxFactor(raw[[col]], levels=lev, exclude='')
  }
  
  mean.or.na(raw[,1:13], 11)
}

score.wemwbs <- function(raw) {  # Warwick-Edinburgh MWBS
  if (ncol(raw) != 7) stop("Expected 7 columns")

  WEItem <- c("None of the time",  "Rarely",	"Some of the time",	"Often",	"All of the time")
  for (col in 1:7) {
    raw[[col]] <- ordered(raw[[col]], levels=WEItem)
  }
  
  rscore <- do.call('mapply', c(sum, lapply(raw, unclass)))

  # Bartram, Sinclair, & Baldwin (2013, p. 387) Table 3
  rasch <- c(7, 9.22, 10.87, 12.1, 13.11, 13.96, 14.73, 15.44, 16.12,
             16.79, 17.45, 18.12, 18.81, 19.5, 20.21, 20.94, 21.68, 22.45,
             23.26, 24.12, 25.04, 26.01, 27.02, 28.04, 29.07, 30.16,
             31.39, 32.97, 35)
  rasch[rscore - 6]
}

score.ei <- function(raw) {
  if (ncol(raw) != 33) stop("Expected 33 columns")
  
  EIItem <- c("None of the time",  "Rarely",	"Some of the time",	"Often",	"All of the time")
  for (col in 1:33) {
    lev <- EIItem
    if (col %in% c(5,28,33)) {
      lev <- rev(EIItem)
    }
    raw[[col]] <- ordered(raw[[col]], levels=lev)
  }
  
  mean.or.na(raw[,1:33], 30)
}

score.rrq <- function(raw) {
    if (ncol(raw) != 24) stop("Expecting 24 columns")

    RRQItem <- c('Strongly disagree',
                 'Disagree',
                 'Neutral',
                 'Agree',
                 'Strongly Agree')

    base <- 1
    for (col in base:(base+23)) {
        rrq.index <- 1 + col - base
        levels <- RRQItem
        if (any(rrq.index %in% c(6,9,10,13,14,17,20,24))) {
            levels <- RRQItem[5:1]
        }
        raw[[col]] <- ordered(raw[[col]], levels=levels)
    }

    list(rumination=mean.or.na(raw[,base:(base+11)], 8),
         reflection=mean.or.na(raw[,(base+12):(base+23)], 8))
}

rsubstr <- function(str, start, end) {
  len <- nchar(str)
  substr(str, len + start, len + end)
}

in.bed <- function(from, to) {
    if (length(from) != length(to)) stop("length mismatch")
    mapply(function(from1, to1) {
        if (is.na(from1) || nchar(from1) == 0) return(NA)
        if (is.na(to1) || nchar(to1) == 0) return(NA)
        side <- toupper(rsubstr(from1, -1, 0))
        if (side != "PM" && side != "AM") stop(side)
        sleep.day <- ifelse(side=="PM", "2014-01-01", "2014-01-02")
        hr <- (strptime(paste("2014-01-02", to1),
                        format="%Y-%m-%d %I:%M %p") -
               strptime(paste(sleep.day, from1),
                        format="%Y-%m-%d %I:%M %p"))
        as.numeric(hr)
    }, from, to)
}

score.psqi <- function(raw) {
    if (ncol(raw) != 19) stop("Expecting 19 columns")

    SleepItem <- c('Not during the past month',
                   'Less than once a week',
                   'Once or twice a week',
                   'Three or more times a week')

    SleepQualityItem = c('Very good',
        'Fairly good',
        'Fairly bad',
        'Very bad');

    SleepEnthItem = c('No problem at all',
        'Only a very slight problem',
        'Somewhat of a problem',
        'A very big problem');

    base <- 0
    for (col in (base+5):(base+19)) {
        psqi.index <- col - base
        levels <- SleepItem
        if (psqi.index == 16) {
            levels <- SleepQualityItem
        } else if (psqi.index == 19) {
            levels <- SleepEnthItem
        }
        raw[[col]] <- ordered(raw[[col]], levels=levels)
    }

    psqi.1 <- unclass(raw[[base + 16]])-1
    psqi.time.to.sleep <- cut(raw[[base + 2]], breaks=c(0, 15, 30, 60, 3600), ordered_result=TRUE)
    psqi.2 <- (unclass(raw[[base + 5]]) + unclass(psqi.time.to.sleep) - 1) %/% 2
    psqi.sleep.duration <- cut(raw[[base + 4]], c(0,5,6,7,24), right=FALSE, ordered_result=TRUE)
    psqi.3 <- 4 - unclass(psqi.sleep.duration)
    psqi.efficiency <- cut(raw[[base + 4]] / in.bed(raw[[base + 1]], raw[[base + 3]]), c(0,.65,.75,.85, 2), ordered_result=TRUE)
    psqi.4 <- 4 - unclass(psqi.efficiency)
    psqi.disturb <- raw[c((base+6):(base+13), base+15)]
    psqi.disturb.mean <- mean.or.na(psqi.disturb, 8) - 1
    psqi.5 <- unclass(cut(psqi.disturb.mean, c(-1, .9, 9, 18, 30)/9)) - 1
    psqi.6 <- unclass(raw[[base + 17]]) - 1
    psqi.daytime <- raw[c(base + 18, base + 19)]
    psqi.7 <-ceiling(mean.or.na(psqi.daytime, 1) - 1)

                                        # 0 indicates no difficulty
                                        # 21 indicates severe difficulty
    apply(cbind(psqi.1, psqi.2, psqi.3, psqi.4, psqi.5, psqi.6, psqi.7), 1, sum)
}

score.dass <- function(raw) {
    if (ncol(raw) != 21) stop("Expecting 21 columns")

    DASSItem = c('Did not apply to me at all',
        'Applied to me to some degree, or some of the time',
        'Applied to me to a considerable degree, or a good part of time',
        'Applied to me very much, or most of the time')

    base <- 1
    for (col in base:(base+20)) {
        raw[[col]] <- ordered(raw[[col]], levels=DASSItem)
    }

    # based on Henry & Crawford (2005), Figure 1
    dass.d <- raw[base+c(3,5,10,13,16,17,21)-1]  # depression
    dass.a <- raw[base+c(2,4,7,9,15,19,20)-1]    # anxiety
    dass.s <- raw[base+c(1,6,8,11,12,14,18)-1]   # stress
    list(dass.d=mean.or.na(dass.d, 6),
         dass.a=mean.or.na(dass.a, 6),
         dass.s=mean.or.na(dass.s, 6),
         dass.na=mean.or.na(cbind(dass.d, dass.a, dass.s), 18))
}

score.5fMindfulness <- function(raw) {
    if (ncol(raw) != 39) stop("Expecting 39 columns")

    FFItem = c('never or very rarely true',
                'rarely true',
                'sometimes true',
                'often true',
                'very often or always true')
    
    for (col in 1:39) {
        lev <- FFItem
        if (any(col == c(12, 16, 22))) lev <- rev(lev)
        raw[[col]] <- mxFactor(raw[[col]], levels=lev, exclude='')
    }

    # See Table 3 of Baer et al (2006)
    
    nonreact <- raw[c(4,9,19,21,24,29,33)]  # factor 1
    observe <- raw[c(1,6,11,15,20,26,31,36)]   # factor 2
    actAware <- raw[c(5,8,13,18,23,28,34,38)]  # factor 3
    describe <- raw[c(2,7,12,16,22,27,32,37)]  # factor 4
    nonjudge <- raw[c(3,10,14,17,25,30,35,39)]  # factor 5
    list(nonreact=mean.or.na(nonreact, 6),
         observe=mean.or.na(observe, 7),
         actAware=mean.or.na(actAware, 7),
         describe=mean.or.na(describe, 7),
         nonjudge=mean.or.na(nonjudge, 7))
}

code.5fMindfulness2 <- function(raw) {
    if (ncol(raw) != 39) stop("Expecting 39 columns")

    FFItem = c('never or very rarely true',
                'rarely true',
                'sometimes true',
                'often true',
                'very often or always true')
    
    for (col in 1:39) {
        lev <- FFItem
        if (any(col == c(16:23, 26:28, 32:39))) lev <- rev(lev)
        raw[[col]] <- mxFactor(raw[[col]], levels=lev, exclude='')
    }
    raw
}

score.5fMindfulness2 <- function(raw) {
    # See Table 3 of Baer et al (2006)
    
    nonreact <- raw[1:7]  # factor 1
    observe <- raw[8:15]   # factor 2
    actAware <- raw[16:23]  # factor 3
    describe <- raw[24:31]  # factor 4, *has reverse scored items*
    nonjudge <- raw[32:39]  # factor 5
    list(nonreact=mean.or.na(nonreact, 6),
         observe=mean.or.na(observe, 7),
         actAware=mean.or.na(actAware, 7),
         describe=mean.or.na(describe, 7),
         nonjudge=mean.or.na(nonjudge, 7))
}

NotionItem = c('This is the first time I have thought about it',
               "The notion has crossed my mind, but I'm not sure what it means to me",
               'I have discussed it with friends',
               'I have read something about it',
               'I study the topic with interest')

MSAgreementItem = tolower(c('Agree','Agree somewhat','Not sure','Disagree somewhat','Disagree'))

LearnItem = tolower(c('No', 'Not Sure',
              'Yes, if it was easy to learn',
              'Yes, I am moderately curious',
              'Yes, I am keenly curious'))

MSFrequencyItem <- c("more than 2 times a day",
                     "1-2 times a day",
                     "4-6 times a week",
                     "1-3 times a week",
                     "1-3 times a month",
                     "I don't try to cause myself to experience complete mental silence")

maxDurationItem = c('A moment (e.g., a second or shorter)',
                    'Longer than a moment but shorter than 10 seconds',
                    'Between 10 seconds and 1 minute',
                    'Between 1 minute and 10 minutes',
                    'More than 10 minutes')


MSNotionItem <- tolower(c("This is the first time I have thought about it",
                          "The notion has crossed my mind, but I'm not sure what it means to me",
                          "I have discussed it with friends",
                          "I have read something about it",
                          "I have an interest in this topic"))

MSFrequencyItem2 <- tolower(c("more than 2 times a day",
                              "1-2 times a day",
                              "4-6 times a week",
                              "1-3 times a week",
                              "1-3 times a month"))

MSTimeAlloc <- tolower(c("Less than 10 minutes",
                         "10-20 minutes",
                         "21-30 minutes",
                         "31-45 minutes",
                         "46-60 minutes",
                         "More than 1 hour"))

MSMaxDurationItem2 = tolower(c(
  "I didn't experience complete mental silence",
  'A moment (e.g., a second or shorter)',
  'Longer than a moment but shorter than 10 seconds',
  'Between 10 seconds and 1 minute',
  'Between 1 minute and 10 minutes',
  'Between 10 minutes and 2 hours',
  'More than 2 hours'))

cms.fixOldData <- function(raw) {
  raw$maxDuration[raw$maxDuration == "more than 10 minutes"] <- NA
  raw$maxDuration <- mxFactor(raw$maxDuration, levels=tolower(MSMaxDurationItem2))
  levels(raw$msNotion)[5] <- tolower(MSNotionItem)[5]
  if (!is.null(raw$freqCause)) {
      raw$freqCause[!is.na(raw$freqCause) &
                    raw$freqCause == "i don't try to cause myself to experience complete mental silence"] <- NA
      raw$freqCause <- mxFactor(as.character(raw$freqCause), levels = rev(tolower(MSFrequencyItem2)),
                                exclude=tolower(c('infrequently','weekly','daily',
                                          "I don't specifically allocate my time for complete mental silence.")))
  }
  raw
}

stripPeriods <- function(df) {
  as.data.frame(lapply(df, sub, pattern="\\.$", replacement="", perl=TRUE),
                stringsAsFactors = FALSE)
}

prep.cms201309 <- function(raw) {
  if (ncol(raw) != 23) stop("Expecting 23 columns")
  raw <- stripPeriods(raw)
  
  df <- data.frame(
    msNotion=safe.ordered(raw[[1]], NotionItem),
    # 2=msFreq, item removed
    msAny=safe.ordered(raw[[3]], MSAgreementItem),
    msEvery=safe.ordered(raw[[4]], MSAgreementItem),
    wantLearn=safe.ordered(raw[[5]], LearnItem,
                           "I know how to cause myself to experience complete mental silence")
  )
  
  item1 <- c("msEffort", "msEmo", "msDescarte", "msAfraid", "msFast", "msLife", "msIdentity")
  for (x1 in 1:7) {
    df[,item1[x1]] <- safe.ordered(raw[[5+x1]], MSAgreementItem)
  }
  df$skipInt <- apply(df[,item1], 1, function(t) all(is.na(t)))
  df$msNotSure <- apply(df[,item1], 1, function(t) all(!is.na(t) & t == "not sure"))
  
  # freqCause response options changed raw[[13]]
  df$pctSuccess <- raw[[14]]
  # table(df$pctSuccess[unclass(df$freqCause) == 6])   #crazy?
  df$maxDuration <- safe.ordered(raw[[15]], maxDurationItem)

  item2 <- c("msYearn", 'msMet', "msEnv", "msAllow", "msCause",
             'msShared', 'msTeach', 'msTrainTeach')
  for (x2 in 1:8) {
    df[,item2[x2]] <- safe.ordered(raw[[15+x2]], rev(MSAgreementItem))
  }
  
  df$skipExp <- apply(df[,c(item2, 'pctSuccess', 'maxDuration')], 1, function(t) all(is.na(t)))
  df$instrument <- "2013-09-12"
  
  mask <- df$skipInt & df$skipExp
  df$skipExp[mask] <- NA
  df$skipInt[mask] <- NA
  df$skipInt <- mxFactor(df$skipInt, levels=c(FALSE, TRUE))
  df$skipExp <- mxFactor(df$skipExp, levels=c(TRUE, FALSE))
  
  cms.fixOldData(df)
}

prep.cms201312 <- function(raw) {
  if (ncol(raw) != 26) stop("Expecting 26 columns")
  raw <- stripPeriods(raw)
  
  df <- data.frame(
    msNotion=safe.ordered(raw[[1]], NotionItem),
    msAny=safe.ordered(raw[[2]], MSAgreementItem),
    msEvery=safe.ordered(raw[[3]], MSAgreementItem),
    wantLearn=safe.ordered(raw[[4]], LearnItem,
                      "I know how to cause myself to experience complete mental silence")
  )

  item1 <- c("msEffort", "msEmo", "msDescarte", "msAfraid", "msFast", "msLife", "msIdentity")
  for (x1 in 1:7) {
    df[,item1[x1]] <- safe.ordered(raw[[4+x1]], MSAgreementItem)
  }
  df$skipInt <- apply(df[,item1], 1, function(t) all(is.na(t)))
  df$msNotSure <- apply(df[,item1], 1, function(t) all(!is.na(t) & t == "not sure"))
  
  item2 <- c("msYearn", "msEnv", "msAllow", "msCause")
  for (x2 in 1:4) {
    df[,item2[x2]] <- safe.ordered(raw[[11+x2]], rev(MSAgreementItem), "I don't understand this question")
  }
  evItems <- c('freqCause', 'pctSuccess', 'maxDuration')
  df$freqCause <- safe.ordered(raw[[16]], rev(MSFrequencyItem))
  df$pctSuccess <- raw[[17]]
  # table(df$pctSuccess[unclass(df$freqCause) == 6])   #crazy?
  df$maxDuration <- safe.ordered(raw[[18]], maxDurationItem)
  item3 <- c('msMet', 'msShared', 'msTeach', 'msTrainTeach')
  for (x3 in 1:4) {
    df[, item3[x3]] <- safe.ordered(raw[[17+x3*2]], rev(MSAgreementItem))
    df[, paste(item3[x3], "Num", sep="")] <- raw[[18+x3*2]]
  }
  trainItems <- c(item2, item3, paste(item3[x3], "Num", sep=""))
  
  df$skipExp <- apply(df[,c(evItems, trainItems)], 1, function(t) all(is.na(t)))

  df$instrument <- "2013-12-16"  # also works for 2014-01

  mask <- df$skipInt & df$skipExp
  df$skipExp[mask] <- NA
  df$skipInt[mask] <- NA
  df$skipInt <- mxFactor(df$skipInt, levels=c(FALSE, TRUE))
  df$skipExp <- mxFactor(df$skipExp, levels=c(TRUE, FALSE))
  
  cms.fixOldData(df)
}

prep.cms201409 <- function(raw, hack=FALSE) {
  if (ncol(raw) != 29) stop("Expecting 29 columns")
  raw <- stripPeriods(raw)
  
  # Only affects wave earlydata/short-20140915
  raw[[1]][raw[[1]] == "It is a topic of interest"] <- "I have an interest in this topic"
  
  df <- data.frame(
    msNotion=mxFactor(tolower(raw[[1]]), MSNotionItem, exclude=""),
    msAny=mxFactor(tolower(raw[[2]]), MSAgreementItem, exclude=""),
    msChildhood=mxFactor(tolower(raw[[3]]), MSAgreementItem, exclude=""),
    msEvery=mxFactor(tolower(raw[[4]]), MSAgreementItem, exclude=""),
    wantLearn=mxFactor(tolower(raw[[5]]), LearnItem,
                       exclude=tolower(c('',"I have experienced complete mental silence")))
  )
  
  item1 <- c("msEffort", "msEmo", "msAfraid", "msFast", "msLife", "msIdentity", "msPreoccu")
  for (x1 in 1:7) {
    df[,item1[x1]] <- mxFactor(tolower(raw[[5+x1]]), MSAgreementItem, exclude="")
  }
  df$skipInt <- apply(df[,item1], 1, function(t) all(is.na(t)))
  df$skipInt <- mxFactor(df$skipInt, levels=c(FALSE, TRUE))
  
  item2 <- c("msYearn", "msEnv", "msAllow", "msCause")
  for (x2 in 1:4) {
    df[,item2[x2]] <- mxFactor(tolower(raw[[12+x2]]), rev(MSAgreementItem),
                               exclude=tolower(c('',"I don't understand this question")))
  }

  evItems <- c('freqCause', 'msTimeAlloc', 'pctSuccess', 'maxDuration', "maxDurationOut")

  df$freqCause <- mxFactor(tolower(raw[[17]]), rev(MSFrequencyItem2),
                           exclude=tolower(c('',"I don't specifically allocate my time for complete mental silence")))
  df$msTimeAlloc <- mxFactor(tolower(raw[[18]]), MSTimeAlloc,
                             exclude=tolower(c("","I don't plan any particular amount of time")))
  df$pctSuccess <- raw[[19]]
  # table(df$pctSuccess[df$freqCause == "i don't specifically allocate my time for complete mental silence"])   #crazy?
  df$maxDuration <- mxFactor(tolower(raw[[20]]), MSMaxDurationItem2,
                             exclude=tolower(c("", "I have no idea of how much time elapsed")))
  df$maxDurationOut <- mxFactor(tolower(raw[[21]]), MSMaxDurationItem2,
                             exclude=tolower(c("", "I have no idea of how much time elapsed")))

  if (hack) {
    # Only affects wave earlydata/short-20140915
    mask <- unclass(df$maxDuration) == unclass(df$maxDurationOut)
    mask <- !is.na(mask) & mask
    df$maxDurationOut[mask] <- MSMaxDurationItem2[1]
    df$maxDurationOut[!mask] <- NA
  }

  item3 <- c('msMet', 'msShared', 'msTeach', 'msTrainTeach')
  for (x3 in 1:4) {
    df[, item3[x3]] <- mxFactor(tolower(raw[[20+x3*2]]), rev(MSAgreementItem),
                                exclude="")
    df[, paste(item3[x3], "Num", sep="")] <- raw[[21+x3*2]]
  }
  
  df$instrument <- "2014-09-15"
  
  df
}

prep.cms201410 <- function(raw) {
  if (ncol(raw) != 29) stop("Expecting 29 columns")
  raw <- stripPeriods(raw)
  
  df <- data.frame(
    msNotion=mxFactor(tolower(raw[[1]]), MSNotionItem, exclude=""),
    msAny=mxFactor(tolower(raw[[2]]), MSAgreementItem, exclude=""),
    msChildhood=mxFactor(tolower(raw[[3]]), MSAgreementItem, exclude=""),
    msEvery=mxFactor(tolower(raw[[4]]), MSAgreementItem, exclude=""),
    wantLearn=mxFactor(tolower(raw[[5]]), LearnItem,
                       exclude=tolower(c('',"I have experienced complete mental silence")))
  )
  
  item1 <- c("msEffort", "msEmo", "msAfraid", "msFast", "msLife", "msIdentity", "msPreoccu")
  for (x1 in 1:7) {
    df[,item1[x1]] <- mxFactor(tolower(raw[[5+x1]]), MSAgreementItem, exclude="")
  }
  df$skipInt <- apply(df[,item1], 1, function(t) all(is.na(t)))
  df$skipInt <- mxFactor(df$skipInt, levels=c(FALSE, TRUE))
  
  item2 <- c("msYearn", "msEnv", "msAllow", "msTaught")
  for (x2 in 1:4) {
    df[,item2[x2]] <- mxFactor(tolower(raw[[12+x2]]), rev(MSAgreementItem),
                               exclude=tolower(c('',"I don't understand this question")))
  }
  
  df$freqCause <- mxFactor(tolower(raw[[17]]), rev(MSFrequencyItem2),
                           exclude=tolower(c('',"I don't specifically allocate my time for complete mental silence")))
  df$msTimeAlloc <- mxFactor(tolower(raw[[18]]), MSTimeAlloc,
                             exclude=tolower(c("","I don't plan any particular amount of time")))
  df$pctSuccess <- raw[[19]]
  # table(df$pctSuccess[df$freqCause == "i don't specifically allocate my time for complete mental silence"])   #crazy?
  df$maxDuration <- mxFactor(tolower(raw[[20]]), MSMaxDurationItem2,
                             exclude=tolower(c("", "I have no idea of how much time elapsed")))
  df$maxDurationOut <- mxFactor(tolower(raw[[21]]), MSMaxDurationItem2,
                                exclude=tolower(c("", "I have no idea of how much time elapsed")))
  
  item3 <- c('msMet', 'msShared', 'msTeach', 'msTrainTeach')
  for (x3 in 1:4) {
    df[, item3[x3]] <- mxFactor(tolower(raw[[20+x3*2]]), rev(MSAgreementItem),
                                exclude="")
    df[, paste(item3[x3], "Num", sep="")] <- raw[[21+x3*2]]
  }
  
  df$instrument <- "2014-10-22"
  
  df
}

MSFrequencyItem3 <- tolower(c("continuously",
                              "frequently",
                              "occasionally",
                              "not at all"))

MSPhysicalLoc <- tolower(c("I consistently felt that I was sitting in the room",
                           "Sometimes I was sitting in the room and sometimes I lost track of the boundaries of my physical body",
                           "I consistently lost track of the boundaries of my physical body",
                           "The distinction between self and other dissolved",
                           "I felt a sense of oneness with the whole"))

MSRoughDuration <- tolower(c("Up to 1 minute",
                             "Between 1 minute and 10 minutes",
                             "Between 10 minutes and 2 hours",
                             "More than 2 hours"))

MSConsciousWill <- tolower(c("I had to struggle and force myself to get through the session",
                             "I had to make a moderate effort to attend to what I was supposed to do during the session",
                             "I drifted between effortfulness and effortlessness during the session",
                             "I enjoyed some relief from conscious will and simultaneously attended to the session",
                             "I enjoyed the stillness of not willing anything and simultaneously attended closely to the session"))
  
prep.cms201508 <- function(raw) {
  if (ncol(raw) != 34) stop("Expecting 34 columns")
  raw <- stripPeriods(raw)
  
  df <- data.frame(
    msNotion=mxFactor(tolower(raw[[1]]), MSNotionItem, exclude=""),
    msAny=mxFactor(tolower(raw[[2]]), MSAgreementItem, exclude=""),
    msChildhood=mxFactor(tolower(raw[[3]]), MSAgreementItem, exclude=""),
    wantLearn=mxFactor(tolower(raw[[4]]), LearnItem,
                       exclude=tolower(c('',"I have experienced complete mental silence")))
  )
  
  item1 <- c("msEffort", "msAfraid", "msFast", "msLife", "msIdentity", "msPreoccu")
  for (x1 in 1:6) {
    df[,item1[x1]] <- mxFactor(tolower(raw[[4+x1]]), MSAgreementItem, exclude="")
  }

  item3 <- c('msShared', 'msMet', 'msTeach', 'msTrainTeach')
  for (x3 in 1:4) {
    df[, paste(item3[x3], "Num", sep="")] <- raw[[10+x3]]
  }

  item2 <- c("msYearn", "msEnv", "msAllow", "msTaught")
  for (x2 in 1:4) {
    df[,item2[x2]] <- mxFactor(tolower(raw[[14+x2]]), rev(MSAgreementItem),
                               exclude=tolower(c('',"I don't understand this question")))
  }
  
  # revisit after 18Sep2015 revision that added another response option TODO
  noSession <- ((tolower(raw[[19]]) == tolower("I didn't specifically allocate my time for complete mental silence") &
                     tolower(raw[[20]]) == tolower("I didn't plan any particular amount of time") &
                         raw[[24]] == "0") |
                (tolower(raw[[19]]) == tolower("I didn't specifically allocate my time for complete mental silence") &
                     tolower(raw[[20]]) == tolower("not applicable, since i do not allocate time for structured sessions")))

  df$freqCause <- mxFactor(tolower(raw[[19]]), rev(MSFrequencyItem2),
                           exclude=tolower(c('',"I didn't specifically allocate my time for complete mental silence")))
  df$msTimeAlloc <- mxFactor(tolower(raw[[20]]), MSTimeAlloc,
                             exclude=tolower(c("","I didn't plan any particular amount of time",
                                 "not applicable, since i do not allocate time for structured sessions")))

  item4 <- c('thinkTime', 'thinkFuture', 'thinkPast')
  for (x1 in 1:length(item4)) {
    df[,item4[x1]] <- mxFactor(tolower(raw[[20+x1]]), MSFrequencyItem3,
                                exclude=c(''))
  }
  
  df$pctSuccess <- raw[[24]]
  
  df$physicalLoc <- mxFactor(tolower(raw[[25]]), MSPhysicalLoc, exclude=c(''))
  df$phyLocDepth <- mxFactor(tolower(raw[[26]]),
                             c(tolower("I didn't feel any change in my sense of physical location"),
                               MSRoughDuration), exclude=c(''))
  
  # table(df$pctSuccess[df$freqCause == "i don't specifically allocate my time for complete mental silence"])   #crazy?
  df$maxDuration <- mxFactor(tolower(raw[[27]]), MSMaxDurationItem2,
                             exclude=tolower(c("", "I have no idea of how much time elapsed")))
  df$maxDurationOut <- mxFactor(tolower(raw[[28]]), MSMaxDurationItem2,
                                exclude=tolower(c("", "I have no idea of how much time elapsed")))

  cw <- raw[,29:33]
  colnames(cw) <- NULL
  for (rx in 1:2) {
    df[,paste0("spon",rx)] <-
      mxFactor(MSConsciousWill[apply(cw, 1, match, x=rx)], MSConsciousWill)
  }
  df$sponDepth <- mxFactor(tolower(raw[[34]]),
                           c(tolower("I didn't feel any reduction in conscious will"),
                             MSRoughDuration),
                           exclude=c(''))
  
  # participants often select some responses instead of skipping these items
  sessionItems <- c(item4, 'pctSuccess', 'physicalLoc', 'phyLocDepth', 'maxDuration',
                    paste0("spon",1:2), 'sponDepth')
  noSession <- !is.na(noSession) & noSession
  df[noSession, sessionItems] <- NA
  
  df$instrument <- "2015-08"
  
  df
}
