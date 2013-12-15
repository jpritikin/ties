library(ggplot2)

is.true <- function(v) v & !is.na(v)

manocha2013 <- read.csv("2013combined.csv", stringsAsFactors=FALSE)
rawcols <- colnames(manocha2013[9:length(manocha2013)])
#mask <- apply(is.na(manocha2013[,rawcols]), 1, sum)
#manocha2013$id[manocha2013$time==2 & mask > 60]
haveboth <- match(manocha2013$id, manocha2013[manocha2013$time==2,'id'])

# store todo list for mkreport
write.table(manocha2013[manocha2013$time==2,'id'], file="report-todo.csv",
              row.names=FALSE, col.names=FALSE)

mean.or.na <- function(mat, n.min) {
  size <- apply(mat, 1, function(r) sum(!is.na(r)))
  score <- apply(mat, 1, function(r) sum(r, na.rm=TRUE))
  score[size < n.min] <- NA
  score / size
}

######################################### RRQ
RRQItem <- c('Strongly disagree',
             'Disagree',
             'Neutral',
             'Agree',
             'Strongly Agree')

for (col in 14:37) {
  rrq.index <- col - 13
  levels <- RRQItem
  if (any(rrq.index %in% c(6,9,10,13,14,17,20,24))) {
    levels <- RRQItem[5:1]
  }
  manocha2013[[col]] <- ordered(manocha2013[[col]], levels=levels)
}

rum <- sapply(manocha2013[,14:25], unclass)
manocha2013$rumination <- mean.or.na(rum, 8)

ref <- sapply(manocha2013[,26:37], unclass)
manocha2013$reflection <- mean.or.na(ref, 8)

if (0) {
  rrq <- manocha2013[which(!is.na(haveboth)), c('id', 'time', 'reflection', 'rumination')]
  rrq$id <- factor(rrq$id)
  rrq$time <- factor(rrq$time)
  ggplot(rrq, aes(time, reflection, group=id, color=id)) + geom_line()
  ggplot(rrq, aes(time, rumination, group=id, color=id)) + geom_line()
}

##################################################### PSQI
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

#colnames(manocha2013)[38:56]

for (col in 42:56) {
  psqi.index <- col - 37
  levels <- SleepItem
  if (psqi.index == 16) {
    levels <- SleepQualityItem
  } else if (psqi.index == 19) {
    levels <- SleepEnthItem
  }
  manocha2013[[col]] <- ordered(manocha2013[[col]], levels=levels)
}

psqi.1 <- unclass(manocha2013[[37 + 16]])-1
psqi.time.to.sleep <- cut(manocha2013[[37 + 2]], breaks=c(0, 15, 30, 60, 3600), ordered_result=TRUE)
psqi.2 <- (unclass(manocha2013[[37 + 5]]) + unclass(psqi.time.to.sleep) - 1) %/% 2
psqi.sleep.duration <- cut(manocha2013[[37 + 4]], c(0,5,6,7,24), right=FALSE, ordered_result=TRUE)
psqi.3 <- 4 - unclass(psqi.sleep.duration)
psqi.efficiency <- cut(manocha2013[[37 + 4]] / manocha2013[[37 + 1]], c(0,.65,.75,.85, 2), ordered_result=TRUE)
psqi.4 <- 4 - unclass(psqi.efficiency)
psqi.disturb <- sapply(manocha2013[c((37+6):(37+13), 37+15)], unclass) - 1
psqi.disturb.mean <- mean.or.na(psqi.disturb, 8)
psqi.5 <- unclass(cut(psqi.disturb.mean, c(-1, .9, 9, 18, 30)/9)) - 1
psqi.6 <- unclass(manocha2013[[37 + 17]]) - 1
psqi.daytime <- sapply(manocha2013[c(37 + 18, 37 + 19)], unclass)
psqi.7 <-ceiling(mean.or.na(psqi.daytime, 1) - 1)

# 0 indicates no difficulty
# 21 indicates severe difficulty
manocha2013$psqi <- 
  apply(cbind(psqi.1, psqi.2, psqi.3, psqi.4, psqi.5, psqi.6, psqi.7), 1, sum)

if (0) {
  psqi <- manocha2013[which(!is.na(haveboth)), c('id', 'time', 'psqi')]
  psqi$id <- factor(psqi$id)
  psqi$time <- factor(psqi$time)
  ggplot(psqi, aes(time, psqi, group=id, color=id)) + geom_line()
}

##################################################### DASS-21

DASSItem = c('Did not apply to me at all',
                 'Applied to me to some degree, or some of the time',
                 'Applied to me to a considerable degree, or a good part of time',
                 'Applied to me very much, or most of the time')

for (col in 57:(56+21)) {
  manocha2013[[col]] <- ordered(manocha2013[[col]], levels=DASSItem)
}

#colnames(manocha2013)[57:(56+21)]

# based on Henry & Crawford (2005), Figure 1
dass.d <- sapply(manocha2013[56+c(3,5,10,13,16,17,21)], unclass)  # depression
dass.a <- sapply(manocha2013[56+c(2,4,7,9,15,19,20)], unclass)  # anxiety
dass.s <- sapply(manocha2013[56+c(1,6,8,11,12,14,18)], unclass)  # stress
manocha2013$dass.d <- mean.or.na(dass.d, 6)
manocha2013$dass.a <- mean.or.na(dass.a, 6)
manocha2013$dass.s <- mean.or.na(dass.s, 6)
manocha2013$dass.na <- mean.or.na(cbind(dass.d, dass.a, dass.s), 18)

if (0) {
  df <- manocha2013[which(!is.na(haveboth)), c('id', 'time', 'dass.na', 'dass.d', 'dass.a', 'dass.s')]
  df$id <- factor(df$id)
  df$time <- factor(df$time)
  ggplot(df, aes(time, dass.na, group=id, color=id)) + geom_line()
}

######################################### SY practices

SYPracticeItem = c('more often than twice daily',
                       'twice daily',
                       'between twice and once a day',
                       'less than once a day but more than 3 times a week',
                       'between 3 times a week and once a week',
                       'between once a week and once a month',
                       'less than once a month')

for (col in 9:12) {
  manocha2013[[col]] <- ordered(manocha2013[[col]], levels=rev(SYPracticeItem))
}

manocha2013$sypractice <- mean.or.na(sapply(manocha2013[,9:12], unclass), 3)
manocha2013$sypractice[manocha2013$syMonths <= 2 & is.na(manocha2013$sypractice)] <- 1

######################################### Mental silence

load("../manocha2013sc.rda")
if (dim(manocha2013sc)[1] != length(manocha2013$id)) stop("scores are stale")
manocha2013$msInterest <- manocha2013sc[,1]
manocha2013$msExperience <- manocha2013sc[,2]

if (0) {
  df <- manocha2013[which(!is.na(haveboth)), c('id', 'time', 'msInterest', 'msExperience')]
  df$id <- factor(df$id)
  df$time <- factor(df$time)
  ggplot(df, aes(time, msExperience, group=id, color=id)) + geom_line()
}

######################################### Change scores

scores <- manocha2013
for (col in rawcols) { scores[[col]] <- NULL }

measures <- c("rumination", "reflection", "psqi", "dass.d", "dass.a", "dass.s",  "dass.na",
              "sypractice", "msInterest", "msExperience")

write.table(scores[!is.na(haveboth),], file="scores.csv", sep="\t", row.names=FALSE)

t1.order <- which(!is.na(haveboth) & scores$time==1)
t1.order <- t1.order[order(scores$id[t1.order])]
t2.order <- which(!is.na(haveboth) & scores$time==2)
t2.order <- t2.order[order(scores$id[t2.order])]
t1 <- scores[t1.order, measures]
t2 <- scores[t2.order, measures]
chg <- cbind(scores[t1.order,colnames(scores)[2:8]], t2-t1)

if (0) {
  write.table(chg, file="change.csv", sep="\t", row.names=FALSE)
  
  plot(chg$msInterest, chg$msExperience)
  options(width=60)
  success <- is.true(chg$msInterest >= 0 & chg$msExperience >= 0)
  chg[success,measures]
  
  whysuccess <- cbind(success=success, t1)
  m1 <- glm(success ~ ., data=whysuccess, family=binomial(link=logit))
  
  chg[(is.true(chg$dass.na > 0) | is.true(chg$psqi > 1)) & !success, measures]
}

