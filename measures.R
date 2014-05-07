mean.or.na <- function(mat, n.min) {
  mat <- as.data.frame(lapply(mat, unclass))
  size <- apply(mat, 1, function(r) sum(!is.na(r)))
  score <- apply(mat, 1, function(r) sum(r, na.rm=TRUE))
  score[size < n.min] <- NA
  score / size
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
        if (nchar(from1) == 0) return(NA)
        if (nchar(to1) == 0) return(NA)
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
    list(d=mean.or.na(dass.d, 6),
         a=mean.or.na(dass.a, 6),
         s=mean.or.na(dass.s, 6),
         na=mean.or.na(cbind(dass.d, dass.a, dass.s), 18))
}

NotionItem = c('This is the first time I have thought about it.',
               "The notion has crossed my mind, but I'm not sure what it means to me.",
               'I have discussed it with friends.',
               'I have read something about it.',
               'I study the topic with interest.')

MSAgreementItem = c('Agree','Agree somewhat','Not sure','Disagree somewhat','Disagree')

LearnItem = c('No', 'Not Sure',
              'Yes, if it was easy to learn',
              'Yes, I am moderately curious',
              'Yes, I am keenly curious')

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

safe.ordered <- function(df, levels, old.levels=c()) {
  if (!missing(old.levels)) {
    if (any(!is.na(match(old.levels, levels)))) stop("levels and old.levels overlap")
    df[which(!is.na(match(df, old.levels)))] <- ''
  }
  levels <- tolower(levels)
  df <- tolower(df)
  no.match <- is.na(match(df, c('',levels)))
  if (any(no.match)) {
    stop(paste("In", col, "unknown levels:", paste(unique(df[no.match]), collapse=" ")))
  }
  ordered(df, levels=levels)
}

prep.cms201309 <- function(raw) {
  if (ncol(raw) != 23) stop("Expecting 23 columns")
  
  df <- data.frame(
    msNotion=safe.ordered(raw[[1]], NotionItem),
    # 2=msFreq, item removed
    msAny=safe.ordered(raw[[3]], MSAgreementItem),
    msEvery=safe.ordered(raw[[4]], MSAgreementItem),
    wantLearn=safe.ordered(raw[[5]], LearnItem,
                           "I know how to cause myself to experience complete mental silence.")
  )
  
  item1 <- c("msEffort", "msEmo", "msDescarte", "msAfraid", "msFast", "msLife", "msIdentity")
  for (x1 in 1:7) {
    df[,item1[x1]] <- safe.ordered(raw[[5+x1]], MSAgreementItem)
  }
  df$skipInt <- apply(df[,item1], 1, function(t) all(is.na(t)))
  
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
  
  df
}

prep.cms201312 <- function(raw) {
  if (ncol(raw) != 26) stop("Expecting 26 columns")

  df <- data.frame(
    msNotion=safe.ordered(raw[[1]], NotionItem),
    msAny=safe.ordered(raw[[2]], MSAgreementItem),
    msEvery=safe.ordered(raw[[3]], MSAgreementItem),
    wantLearn=safe.ordered(raw[[4]], LearnItem,
                      "I know how to cause myself to experience complete mental silence.")
  )

  item1 <- c("msEffort", "msEmo", "msDescarte", "msAfraid", "msFast", "msLife", "msIdentity")
  for (x1 in 1:7) {
    df[,item1[x1]] <- safe.ordered(raw[[4+x1]], MSAgreementItem)
  }
  df$skipInt <- apply(df[,item1], 1, function(t) all(is.na(t)))

  item2 <- c("msYearn", "msEnv", "msAllow", "msCause")
  for (x2 in 1:4) {
    df[,item2[x2]] <- safe.ordered(raw[[11+x2]], rev(MSAgreementItem), "I don't understand this question.")
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
  
  df
}
