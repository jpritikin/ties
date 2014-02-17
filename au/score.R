library(ggplot2)
library(digest)
source("../measures.R")

EduItem = c('Less than high school degree',
		'High school degree or equivalent',
		'Some college but no degree',
		'Associate degree',
		'Bachelor degree',
		'Graduate degree')
RelaItem = c('Single',
		 'In a long-term relationship (i.e. together more than a year)',
		 'Other')
RelaItemShort = c('Single',
             'Together',
             'Other')

is.true <- function(v) v & !is.na(v)

manocha2013 <- read.csv("2013combined.csv", stringsAsFactors=FALSE)
manocha2013$edu <- ordered(manocha2013$edu, levels=EduItem)
manocha2013$rel <- ordered(manocha2013$rel, levels=RelaItem, labels=RelaItemShort)

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
base <- 15
got <- score.rrq(manocha2013[base:(base+23)])
manocha2013$rumination <- got$rumination
manocha2013$reflection <- got$reflection

if (0) {
  rrq <- manocha2013[which(!is.na(haveboth)), c('id', 'time', 'reflection', 'rumination')]
  rrq$id <- factor(rrq$id)
  rrq$time <- factor(rrq$time)
  ggplot(rrq, aes(time, reflection, group=id, color=id)) + geom_line()
  ggplot(rrq, aes(time, rumination, group=id, color=id)) + geom_line()
}

##################################################### PSQI
base <- 39
manocha2013$psqi <- score.psqi(manocha2013[base:(base+18)])

if (0) {
  psqi <- manocha2013[which(!is.na(haveboth)), c('id', 'time', 'psqi')]
  psqi$id <- factor(psqi$id)
  psqi$time <- factor(psqi$time)
  ggplot(psqi, aes(time, psqi, group=id, color=id)) + geom_line()
}

##################################################### DASS-21

base <- 58
got <- score.dass(manocha2013[base:(base+20)])
manocha2013$dass.d <- got$d
manocha2013$dass.a <- got$a
manocha2013$dass.s <- got$s
manocha2013$dass.na <- got$na

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

base <- 10
for (col in base:(base+3)) {
  manocha2013[[col]] <- ordered(manocha2013[[col]], levels=rev(SYPracticeItem))
}

manocha2013$sypractice <- mean.or.na(sapply(manocha2013[,base:(base+3)], unclass), 3)
manocha2013$sypractice[manocha2013$syMonths <= 2 & is.na(manocha2013$sypractice)] <- 1

######################################### Mental silence

load("../manocha2013sc.rda")
if (dim(manocha2013sc)[1] != length(manocha2013$id)) stop("scores are stale")
scmap <- match(manocha2013sc[,1], manocha2013$uid)
manocha2013$msInterest <- manocha2013sc[scmap,2]
manocha2013$msExperience <- manocha2013sc[scmap,3]

if (0) {
  df <- manocha2013[which(!is.na(haveboth)), c('id', 'time', 'msInterest', 'msExperience')]
  df$id <- factor(df$id)
  df$time <- factor(df$time)
  ggplot(df, aes(time, msExperience, group=id, color=id)) + geom_line()
}

######################################### Standardize

if (0) {
  # I did this for reports to get more similar ranges between measures
  manocha2013$rumination <- (manocha2013$rumination - 1)
  manocha2013$reflection <- (manocha2013$reflection - 1)
  manocha2013$psqi <- (21 - manocha2013$psqi) / 5.25
  manocha2013$dass.d <- manocha2013$dass.d - 1
  manocha2013$dass.a <- manocha2013$dass.a - 1
  manocha2013$dass.s <- manocha2013$dass.s - 1
  manocha2013$dass.na <- manocha2013$dass.na - 1
}

# rumination - 1=none, 5=extreme
# reflection - 1=none, 5=extreme
# psqi - 0 indicates no difficulty, 21 indicates severe difficulty
# dass.d - 1=light, 7=heavy
# dass.a - 1=light, 7=heavy
# dass.s - 1=light, 7=heavy
# dass.na - 1=light, 7=heavy

# cat(deparse(round(fivenum(manocha2013$rumination),3)))
expect_equal(fivenum(manocha2013$rumination), c(1.833, 3.273, 3.667, 4, 5), tolerance=.03)
expect_equal(digest(manocha2013$rumination), "2df787d0b6f70252bbb8ecfee9b72ba4")

# cat(deparse(round(fivenum(manocha2013$reflection),3)))
expect_equal(fivenum(manocha2013$reflection), c(2.167, 3.167, 3.583, 4, 5), tolerance=.03)
expect_equal(digest(manocha2013$reflection), "ddd1130db7c4ec9fc93d72ddba132057")

# cat(deparse(round(fivenum(manocha2013$psqi),3)))
expect_equal(fivenum(manocha2013$psqi), c(0, 4, 6, 9, 19), tolerance=.03)
expect_equal(digest(manocha2013$psqi), "a648e1d886e61c117da0f979bb5374b4")

# cat(deparse(round(fivenum(manocha2013$dass.d),3)))
expect_equal(fivenum(manocha2013$dass.d), c(1, 1.226, 1.643, 2.286, 4), tolerance=.03)
expect_equal(digest(manocha2013$dass.d), "8fb9b42a8fd88220b2aadd5046874ce1")

# cat(deparse(round(fivenum(manocha2013$dass.a),3)))
expect_equal(fivenum(manocha2013$dass.a), c(1, 1.286, 1.429, 2.071, 3.714), tolerance=.03)
expect_equal(digest(manocha2013$dass.a), "9a4a22e80018217c8eca5dc55f8e68c0")

# cat(deparse(round(fivenum(manocha2013$dass.s),3)))
expect_equal(fivenum(manocha2013$dass.s), c(1, 1.571, 2, 2.571, 4), tolerance=.03)
expect_equal(digest(manocha2013$dass.s), "bad0f212378801c145efda4b30c201ea")

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
chg <- cbind(scores[t1.order,colnames(scores)[3:8]], t2-t1)


if (0) {
  write.table(chg, file="change.csv", sep="\t", row.names=FALSE)
  
  plot(chg$msInterest, chg$msExperience)
  options(width=60)
  success <- is.true(chg$msInterest >= -1e-4 & chg$msExperience >= -1e-4)
  chg[success,measures]
  apply(scores[scores$time==1 & scores$id %in% chg[success,'id'], measures], 2, mean, na.rm=TRUE)
  
  lostInterest <- is.true(chg$msInterest < -1e-4 & chg$msExperience > 0) & !success
  apply(chg[lostInterest,measures], 2, mean, na.rm=TRUE)
  scores[scores$time==1 & scores$id %in% chg[lostInterest,'id'], measures]
  
  humbled <- is.true(chg$msInterest > 0 & chg$msExperience < -1e-4) & !success
  apply(chg[humbled,measures], 2, mean, na.rm=TRUE)
  
  poorFit <- !(humbled | lostInterest) & !success
  apply(chg[poorFit,measures], 2, mean, na.rm=TRUE)
  
  whysuccess <- cbind(success=success, t1)
  m1 <- glm(success ~ ., data=whysuccess, family=binomial(link=logit))
  
  chg[(is.true(chg$dass.na > 0) | is.true(chg$psqi > 1)) & !success, measures]
  
  scores$msComposite <- pmin(scores$msInterest, scores$msExperience)
  
  mask <- scores$time==2 & !is.na(scores$msInterest) & !is.na(scores$msExperience)
  df <- scores[mask, c(measures, "msComposite")]
  write.table(df, file="time2.csv", row.names =FALSE)
  
  mask <- scores$time==1 & !is.na(scores$msInterest) & !is.na(scores$msExperience)
  df <- scores[mask, c(measures, "msComposite")]
  write.table(df, file="time1.csv", row.names =FALSE)
  
  rank <- order(-scores$msComposite[mask])
  cor(df, use="pairwise.complete.obs")
  apply(df[rank[1:8],], 2, mean, na.rm=TRUE)
  apply(df[rank[24:32],], 2, mean, na.rm=TRUE)
}

