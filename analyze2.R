library(stringr)

espt <- read.csv("raw.csv", stringsAsFactors=FALSE)

for (col in c('end')) {
  espt[[col]] <- strptime(espt[[col]], "%m/%d/%Y %H:%M:%S")
}
#espt$elapsed <- espt$end - espt$start

for (col in c('edu','sex','rel')) {
  espt[[col]] <- factor(espt[[col]])
}

agreement.levels <- c('Agree','Agree somewhat','Not sure','Disagree somewhat','Disagree')
for (col in c('msNotAny','msNotSelf','msMet','msAccident',
              'msShared','msCause','msTeach','msEvery','msPay',
              'msTrainTeach')) {
  espt[[col]] <- factor(espt[[col]], levels=agreement.levels)
}

boredom.levels <- c('True','Not sure','False')
for (col in c('boreFidget', 'boreCheer', 'boreLone')) {
  espt[[col]] <- factor(espt[[col]], levels=boredom.levels)
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
    espt[[col]] <- factor(espt[[col]], levels=freq.levels)
  }
  col <- paste(context,'subjTime',sep='.')
  espt[[col]] <- factor(espt[[col]], levels=subj.time.levels)
  for (col in c(paste(context,'ms.gi',sep='.'),
                paste(context,'b.gi',sep='.'))) {
    espt[[col]] <- factor(espt[[col]], levels=importance.levels)
  }
}

thinking.levels <- c('None', 'A little thinking',
                     'Some moderate thinking',
                     'Rigorous, intensive thinking')
espt$flow.think <- factor(espt$flow.think, levels=thinking.levels)

prepare.levels <- c('Yes', 'Yes, somewhat', 'Not sure / maybe',
                    'Probably not', 'No')
for (col in c('ms.flow', 'flow.ms')) {
  espt[[col]] <- factor(espt[[col]], levels=prepare.levels)
}

for (col in c('m.training', 'm.regular', 'wave', 'ip.continent',
              'ip.country', 'ip.region', 'ip.city')) {
  espt[[col]] <- factor(espt[[col]])
}

########################################################
# demographics

# do participants accurately self report their location? looks good:
# espt[(tolower(as.character(espt$ip.country)) != tolower(espt$country)),c('ip.country', 'country')]

table(espt$wave, grepl("\\bstudent\\b", espt$work, ignore.case=TRUE))

table(espt$wave, espt$sex)

table(espt$born)
table(espt$wave, espt$rel)
table(espt$edu)
table(espt$wave, espt$m.training)

########################################################
# Check for crazy stuff. The vast majority of the data looks reasonable.
# No need to exclude anything.

dis.logic <- cbind(
  msOpposite1=unclass(espt$msEvery) + unclass(espt$msNotAny)-6 < -2,
  msOpposite2=unclass(espt$msNotSelf) + unclass(espt$msCause)-6 < -2,
  msCrazy1=pmax(unclass(espt$msTeach) - unclass(espt$msTrainTeach),0)>1,
  boreCrazy=espt$boreFidget=='No' & espt$boreCheer=='Yes' & espt$boreLone=='False',
  msCrazy2=((espt$wantLearn=='Not sure' | espt$wantLearn=='No') &
    (espt$freqCause=='Daily' | espt$freqCause=='Weekly' | espt$freqCause=='Infrequently')),
  msCrazy3=xor(espt$maxDuration=='I have not experienced complete mental silence',
      espt$durationCharacter=='I have not experienced complete mental silence')
  )
table(apply(dis.logic, 1, sum, na.rm=TRUE))

# msCause -- It is not clear who is causing mental silence.
# table(pmax(unclass(espt$msCause) - unclass(espt$msTeach),0))

dis.flow <- cbind(
  pmax(4-unclass(espt$fl.b.pf),0),
  pmax(unclass(espt$fl.subjTime)-3,0),
  pmax(3-unclass(espt$fl.b.gi),0))
table(apply((dis.flow), 1, sum, na.rm=TRUE))
