library(OpenMx)

mean.or.na <- function(mat, n.min) {
  size <- apply(mat, 1, function(r) sum(!is.na(r)))
  score <- apply(mat, 1, function(r) sum(r, na.rm=TRUE))
  score[size < n.min] <- NA
  score / size
}

lax.ordered <- function(df, col, levels, old.levels=c()) {
  if (is.null(df[[col]])) return(NULL)
  df[[col]] <- tolower(df[[col]])
  df[[col]] <- mxFactor(df[[col]], levels=tolower(levels),
                        exclude = c('',tolower(old.levels)))
}

cause.teach.testlet <- function(df) {
  items <- c("msCause",'msTeach','msTrainTeach')
  col <- rep(NA, dim(df)[1])
  col[df$msCause == 'disagree'] <- 'disagree'
  col[df$msCause == 'disagree somewhat'] <- 'disagree somewhat'
  col[df$msCause == 'not sure'] <- 'not sure'

  # Round down and reduce to 3 categories:
  msTeach <- df$msTeach
  msTeach[msTeach == 'disagree somewhat'] <- 'disagree'
  msTeach[msTeach == 'agree somewhat'] <- 'not sure'
  df$msTeach <- msTeach
  msTteach <- df$msTrainTeach
  msTteach[msTteach == 'disagree somewhat'] <- 'disagree'
  msTteach[msTteach == 'agree somewhat'] <- 'not sure'
  df$msTrainTeach <- msTteach
  
  mask <- is.na(col) & df$msCause < msTeach
  mask <- mask & !is.na(mask)
  col[mask] <- as.character(df$msCause[mask])
  mask <- is.na(col) & !is.na(df$msCause) & msTeach < msTteach
  mask <- mask & !is.na(mask)
  col[mask] <- apply(as.matrix(df[mask, c("msCause",'msTeach')]), 1, paste, collapse="+")
  mask <- is.na(col) & !is.na(df$msCause) & (msTeach == 'disagree' | msTeach == 'disagree somewhat')
  mask <- mask & !is.na(mask)
  col[mask] <- apply(as.matrix(df[mask, c("msCause",'msTeach')]), 1, paste, collapse="+")
  # deal with NAs
  mask <- is.na(col) & !is.na(df$msCause) & is.na(msTeach)
  col[mask] <- as.character(df$msCause[mask])
  mask <- is.na(col) & !is.na(df$msCause) & !is.na(msTeach) & is.na(msTteach)
  col[mask] <- apply(as.matrix(df[mask, c("msCause",'msTeach')]), 1, paste, collapse="+")
  # everything else
  mask <- is.na(col)
  col[mask] <- apply(as.matrix(df[mask, items]), 1, paste, collapse="+")
  col[is.na(df$msCause)] <- NA
  #  length(table(col))
  #  sort(table(col[ver.mask]))
  
  layer <- c('', levels(df$msCause))
  levels <- apply(expand.grid(layer, layer, layer)[3:1], 1, function(wh) {
    while (length(wh) && wh[length(wh)] == '') { wh <- wh[-length(wh)] }
    paste(wh, collapse="+")
  })
  lev.order <- levels[sort(match(unique(col), levels))]
  
  mxFactor(col, levels=lev.order)
}

prepare.espt <- function(espt, scores) {
  for (col in c('edu','sex','rel')) {
    espt[[col]] <- factor(tolower(espt[[col]]))
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
                'msTrainTeach', 'msAfraid', 'msEmo', 'msLife', 'msFast', 'msEffort',
                'msDescarte', 'msIdentity')) {
    if (!any(colnames(espt) == col)) {
#      warning(paste(col, "not found, skipping"))
      next
    }
    levels <- agreement.levels
    revraw <- FALSE
    if (is.na(match(col, c('msNotAny', 'msNotSelf', 'msPay',
                         'msAfraid', 'msEmo', 'msLife', 'msFast', 'msEffort',
                         'msDescarte', 'msIdentity')))) {
      levels <- rev(levels)
    }
    espt[[col]] <- lax.ordered(espt, col, levels)
  }

  if (any(colnames(espt) == 'msNotion')) {
    espt$msNotion <- lax.ordered(espt, 'msNotion', notion.levels)
  }
  if (any(colnames(espt) == 'msPaySure')) {
    espt$msPaySure <- lax.ordered(espt, 'msPaySure', ethics.levels)
  }
  
  cause0.levels <- c('Yes', 'No, but working on it', 'No, I do not')
  espt$msCause0 <- lax.ordered(espt, 'msCause0', rev(cause0.levels))

  oldlearn.levels <- c('Yes',
                       "I know how to cause myself to experience complete mental silence.")
  # Don't ask them if they know how to cause here MS because we ask them later.
  learn.levels <- c('No', 'Not Sure',
                    'Yes, if it was easy to learn',
                    'Yes, I am moderately curious',
                    'Yes, I am keenly curious')
  espt$wantLearn <- lax.ordered(espt, 'wantLearn', learn.levels, oldlearn.levels)

  freqCause.levels <- c("I don't try to cause myself to experience complete mental silence",
                        'Infrequently', 'Weekly', 'Daily')
  espt$freqCause <- lax.ordered(espt, 'freqCause', freqCause.levels)

  msFreq.levels <- c("i haven't experienced complete mental silence.",
                     'Infrequently', 'Weekly', 'Daily')
  espt$msFreq <- lax.ordered(espt, 'msFreq', msFreq.levels)

  maxDuration.levels <- c('A moment (e.g., a second or shorter)',
                          'Longer than a moment but shorter than 10 seconds',
                          'Between 10 seconds and 1 minute',
                          'Between 1 minute and 10 minutes',
                          'More than 10 minutes')
  maxDuration.oldlevels <- c('10 seconds',
                             '1-2 minutes',
                             'I have not experienced complete mental silence')
  espt$maxDuration <- lax.ordered(espt, 'maxDuration', maxDuration.levels, maxDuration.oldlevels)

  durChar.levels <- c('Mostly moments (e.g. an isolated second or two)',
                      'Sometimes moments and sometimes continuous (e.g. isolated seconds and sometimes ten seconds or longer)',
                      'Mostly continuous (e.g. usually ten seconds or longer)')
  espt$durationCharacter <- lax.ordered(espt, 'durationCharacter', durChar.levels,
                                        'I have not experienced complete mental silence')
  
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
      if (is.null(espt[[col]])) next
      espt[[col]] <- mxFactor(espt[[col]], levels=freq.levels, exclude = '')
    }
    col <- paste(context,'subjTime',sep='.')
    if (is.null(espt[[col]])) next
    espt[[col]] <- mxFactor(espt[[col]], levels=subj.time.levels, exclude = '')
    for (col in c(paste(context,'ms.gi',sep='.'),
                  paste(context,'b.gi',sep='.'))) {
      if (is.null(espt[[col]])) next
      espt[[col]] <- mxFactor(espt[[col]], levels=importance.levels, exclude = '')
    }
  }

  if (!is.null(espt[["flow.think"]])) {
    thinking.levels <- c('None', 'A little thinking',
                         'Some moderate thinking',
                         'Rigorous, intensive thinking')
    espt$flow.think <- mxFactor(espt$flow.think, levels=thinking.levels, exclude = '')
  }

  prepare.levels <- c('Yes', 'Yes, somewhat', 'Not sure / maybe',
                      'Probably not', 'No')
  for (col in c('ms.flow', 'flow.ms')) {
    if (is.null(espt[[col]])) next
    espt[[col]] <- mxFactor(espt[[col]], levels=prepare.levels, exclude = '')
  }

  for (col in c('m.training', 'm.regular', 'ip.continent',
                'ip.country', 'ip.region', 'ip.city')) {
    if (is.null(espt[[col]])) next
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
  
  if (!is.null(espt$msPay)) {
    ethical.mask <- !is.na(espt$msPay) & !is.na(espt$msPaySure)
    ethical.levels <- apply(expand.grid(levels(espt$msPay), levels(espt$msPaySure)), 1, paste, collapse="+")
    espt$ethical[ethical.mask] <- apply(as.matrix(espt[ethical.mask, c("msPay", "msPaySure")]),
                                        1, paste, collapse="+")
    espt$ethical <- mxFactor(espt$ethical, levels=ethical.levels, exclude = '')
  }
  
  espt$causeTeach <- cause.teach.testlet(espt)
  
  espt$skipInt <- NA
  espt$skipExp <- NA
  
  mask <- (espt$instrument == "2013-02-19" |
             espt$instrument == "2013-02-13" |
             espt$instrument == "2013-02-12")
#  if (all(!mask)) stop("Can't find instrument")   # obsolete
  espt$skipInt[mask] <-
    apply(is.na(espt[mask,c('msAfraid', 'msEmo', 'msLife',
                            'msFast', "msDescarte", 'msIdentity')]), 1, all)
  espt$skipExp[mask] <-
    apply(is.na(espt[mask, c('freqCause', 'maxDuration', 'msMet',
                             'msEnv', 'msAllow', 'msCause', 'msShared', 'msTeach',
                             'msTrainTeach')]), 1, all)
  
  mask <- espt$instrument == "2013-07-25"
  if (all(!mask)) stop("Can't find instrument")
  espt$skipInt[mask] <-
    apply(is.na(espt[mask,c('msAfraid', 'msEmo', 'msLife',
                            'msFast', "msDescarte", 'msIdentity')]), 1, all)
  espt$skipExp[mask] <-
    apply(is.na(espt[mask, c('freqCause', 'maxDuration', 'msYearn','msMet',
                             'msEnv', 'msAllow', 'msCause', 'msShared', 'msTeach',
                             'msTrainTeach')]), 1, all)
  
  mask <- espt$instrument == "2013-08-15"
  if (all(!mask)) stop("Can't find instrument")
  espt$skipInt[mask] <-
    apply(is.na(espt[mask,c('msAfraid', 'msEmo', 'msLife',
                            'msFast', "msDescarte", 'msIdentity')]), 1, all)
  espt$skipExp[mask] <-
    apply(is.na(espt[mask, c('freqCause', 'maxDuration', 'msYearn','msMet',
                             'msEnv', 'msAllow', 'msCause', 'msShared', 'msTeach',
                             'msTrainTeach')]), 1, all)
  
  mask <- espt$instrument == "2013-09-12"
  if (all(!mask)) stop("Can't find instrument")
  espt$skipInt[mask] <-
    apply(is.na(espt[mask,c('msAfraid', 'msEffort', 'msEmo', 'msLife',
                            'msFast', "msDescarte", 'msIdentity')]), 1, all)
  espt$skipExp[mask] <-
    apply(is.na(espt[mask, c('freqCause', 'maxDuration', 'msYearn','msMet',
                             'msEnv', 'msAllow', 'msCause', 'msShared', 'msTeach',
                             'msTrainTeach')]), 1, all)
  
  mask <- espt$skipInt & espt$skipExp
  espt$skipExp[mask] <- NA
  espt$skipInt[mask] <- NA

  espt$skipInt <- mxFactor(espt$skipInt, levels=c(FALSE, TRUE))
  espt$skipExp <- mxFactor(espt$skipExp, levels=c(TRUE, FALSE))
  
  # testlets
  espt$msIdAfraid <- ordered(10 - unclass(espt$msAfraid) - unclass(espt$msIdentity), levels=seq(8,0,-1))
  espt$msIdAfraidLearn <- ordered(unclass(espt$wantLearn) - (10 - unclass(espt$msAfraid) - unclass(espt$msIdentity)),
                                  levels=seq(-7,5,1))
  espt$msFastEffort <- ordered(10 - unclass(espt$msFast) - unclass(espt$msEffort), levels=seq(8,0,-1))
  espt$msFastEffortLife <- ordered(15 - unclass(espt$msFast) - unclass(espt$msEffort) - unclass(espt$msLife),
                                   levels=seq(12,0,-1))
  freqItems <- cbind(unclass(espt$msFreq), unclass(espt$freqCause))
  espt$msFreqTestlet <- ordered(mean.or.na(freqItems, 1), levels=seq(1,4,.5))
  espt$msMetShared <- ordered(unclass(espt$msMet) + unclass(espt$msShared) - 1, levels=1:9)
  espt$msFreqDur <- ordered(unclass(espt$freqCause) + unclass(espt$maxDuration) - 1, levels=1:8)
  espt$msYearnEnv <- ordered(unclass(espt$msYearn) + unclass(espt$msEnv) - 1, levels=1:9)
  
  # try testlet split between social and non-social
  metShared <- round(mean.or.na(cbind(unclass(espt$msMet), unclass(espt$msShared)), 1))
  adjTeach <- apply(cbind(unclass(espt$msTeach), metShared), 1, min)
  adjTrain <- apply(cbind(unclass(espt$msTrainTeach), adjTeach), 1, min)
  espt$msMetSharedTeach <- ordered(metShared + adjTeach + adjTrain - 2, levels=1:13)
  espt$msYEC <- ordered(unclass(espt$msYearn) + unclass(espt$msEnv) + unclass(espt$msCause) -2, levels=1:13)
  
  return(espt)
}

# for snapshot of sit21c data
prepare.items <- function(items) {
  items$name <- c('msNotAny','msNotSelf','msMet','msAccident','msShared','msCause',
                  'msTeach','msEvery','msPay','msTrainTeach')
  return(items)
}
