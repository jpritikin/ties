library(OpenMx)

lax.ordered <- function(df, col, levels, rawrev=FALSE) {
  if (all(is.na(df[[col]]))) {
    return(df[[col]])
  }
  levels <- tolower(levels)
  is.raw <- str_detect(df[[col]], "^\\d$")
  is.raw[is.na(is.raw)] <- FALSE
  if (rawrev) {
    df[[col]][is.raw] <- 1 + length(levels) - as.numeric(df[[col]][is.raw])
  }
  df[[col]][!is.raw] <- tolower(df[[col]][!is.raw])
  no.match <- is.na(match(df[[col]][!is.raw], c('',levels)))
  if (any(no.match)) {
    stop(paste("In", col, "unknown levels:", unique(df[[col]][!is.raw][no.match])))
  }
  df[[col]][!is.raw] <- mxFactor(df[[col]][!is.raw], levels=levels)
  mxFactor(df[[col]], levels=1:length(levels), labels=levels)
}

prepare.espt <- function(espt, scores) {
  for (col in c('edu','sex','rel')) {
    espt[[col]] <- factor(espt[[col]])
  }

  agreement.levels <- c('Agree','Agree somewhat','Not sure','Disagree somewhat','Disagree')
  notion.levels <- c('This is the first time I have thought about it.',
                     "The notion has crossed my mind, but I'm not sure what it means to me.",
                     'I have discussed it with friends.',
                     'I have read something about it.',
                     'I study the topic with interest.')
  ethics.levels <- c('This is the first time I have thought about it.',
                     "The question has crossed my mind but I'm not sure.",
                     "I have discussed it with friends but I'm not sure.",
                     'I have a gut feeling about whether it is ethical.',
                     'My mind is made up about whether it is ethical.')
  for (col in c('msAny','msMet','msYearn','msAllow',
                'msShared','msEnv','msCause','msTeach','msEvery','msPay',
                'msTrainTeach', 'msAfraid', 'msEmo', 'msLife', 'msFast',
                'msDescarte', 'msIdentity')) {
    if (!any(colnames(espt) == col)) {
#      warning(paste(col, "not found, skipping"))
      next
    }
    levels <- agreement.levels
    revraw <- FALSE
    if (is.na(match(col, c('msNotAny', 'msNotSelf', 'msPay',
                         'msAfraid', 'msEmo', 'msLife', 'msFast',
                         'msDescarte', 'msIdentity')))) {
      levels <- rev(levels)
      revraw <- TRUE
    }
    espt[[col]] <- lax.ordered(espt, col, levels, revraw)
  }

  if (any(colnames(espt) == 'msNotion')) {
    espt$msNotion <- lax.ordered(espt, 'msNotion', notion.levels)
  }
  if (any(colnames(espt) == 'msPaySure')) {
    espt$msPaySure <- lax.ordered(espt, 'msPaySure', ethics.levels)
  }
  
  cause0.levels <- c('Yes', 'No, but working on it', 'No, I do not')
  espt$msCause0 <- lax.ordered(espt, 'msCause0', rev(cause0.levels))

  learn.levels <- c('No', 'Not Sure', 'Yes',
                    "I know how to cause myself to experience complete mental silence.")
  espt$wantLearn <- lax.ordered(espt, 'wantLearn', learn.levels)

  freqCause.levels <- c("I don't try to cause myself to experience complete mental silence",
                        'Infrequently', 'Weekly', 'Daily')
  espt$freqCause <- lax.ordered(espt, 'freqCause', freqCause.levels)

  msFreq.levels <- c("i haven't experienced complete mental silence.",
                     'Infrequently', 'Weekly', 'Daily')
  espt$msFreq <- lax.ordered(espt, 'msFreq', msFreq.levels)

  maxDuration.levels <- c('A moment (e.g., a second or shorter)',
                          '10 seconds',
                          '1-2 minutes',
                          'More than 10 minutes',
                          'I have not experienced complete mental silence')
  espt$maxDuration <- lax.ordered(espt, 'maxDuration', maxDuration.levels)
  # remove old data
  espt$maxDuration[espt$maxDuration=='i have not experienced complete mental silence'] <- NA
  espt$maxDuration <- mxFactor(espt$maxDuration, levels(espt$maxDuration)[1:4])

  boredom.levels <- c('True','Not sure','False')
  for (col in c('boreFidget', 'boreCheer', 'boreLone')) {
    espt[[col]] <- lax.ordered(espt, col, boredom.levels)
  }

  freq.levels <- c('Every time','Frequently','Infrequently',
                   'Almost never','Not at all')
  subj.time.levels <- c('I am unaware of actual time.',
                        'Subjective time is somewhat faster than actual time.',
                        'Subjective time is typical.',
                        'Subjective time is somewhat slower than actual time.',
                        'I am watching the clock.')
  importance.levels <- c('Important', 'Somewhat important', 'Not sure',
                         'Somewhat inappropriate', 'Inappropriate')
  for (context in c('re','sp','ps','rx','pe','wo','me','dd','fl')) {
    for (col in c(paste(context,'ms.pf',sep='.'),
                  paste(context,'b.pf',sep='.'))) {
      espt[[col]] <- mxFactor(espt[[col]], levels=freq.levels)
    }
    col <- paste(context,'subjTime',sep='.')
    espt[[col]] <- mxFactor(espt[[col]], levels=subj.time.levels)
    for (col in c(paste(context,'ms.gi',sep='.'),
                  paste(context,'b.gi',sep='.'))) {
      espt[[col]] <- mxFactor(espt[[col]], levels=importance.levels)
    }
  }

  thinking.levels <- c('None', 'A little thinking',
                       'Some moderate thinking',
                       'Rigorous, intensive thinking')
  espt$flow.think <- mxFactor(espt$flow.think, levels=thinking.levels)

  prepare.levels <- c('Yes', 'Yes, somewhat', 'Not sure / maybe',
                      'Probably not', 'No')
  for (col in c('ms.flow', 'flow.ms')) {
    espt[[col]] <- mxFactor(espt[[col]], levels=prepare.levels)
  }

  for (col in c('m.training', 'm.regular', 'wave', 'ip.continent',
                'ip.country', 'ip.region', 'ip.city')) {
    espt[[col]] <- factor(espt[[col]])
  }
#  espt$ppool <- factor(espt$wave == "ppool-20121230",
#                       levels=c(TRUE,FALSE), labels=c("Subjects Pool","Web Surfers"))
  
  if (!missing(scores)) {
    for (col in c("score","se")) {
      espt[[col]] <- NA
      espt[[col]][match(scores$id, espt$id)] <- scores[[col]]
    }
  }

  if (0) {
    when <- strptime(espt[['end']], "%m/%d/%Y %H:%M:%S")
    revision1 <- ymd("2013-02-20", quiet=TRUE)
    obsolete <- difftime(when, revision1)
    head(espt[obsolete, 'msAccident'])
  }
  
  ethical.mask <- !is.na(espt$msPay) & !is.na(espt$msPaySure)
  ethical.levels <- apply(expand.grid(levels(espt$msPay), levels(espt$msPaySure)), 1, paste, collapse="+")
  espt$ethical[ethical.mask] <- apply(as.matrix(espt[ethical.mask, c("msPay", "msPaySure")]),
                                    1, paste, collapse="+")
  espt$ethical <- mxFactor(espt$ethical, levels=ethical.levels)

  return(espt)
}

prepare.items <- function(items) {
  items$name <- c('msNotAny','msNotSelf','msMet','msAccident','msShared','msCause',
                  'msTeach','msEvery','msPay','msTrainTeach')
  return(items)
}
