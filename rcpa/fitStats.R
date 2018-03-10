# R --no-save -f fitStats.R 2>&1 | tee fitStats.log

options(width=200)

source("modelUtil.R")

model1Par <- c('lp__', 'sigma', 'theta', paste0('threshold',1:2))
model2Par <- c('lp__', 'sigma', 'theta', paste0('threshold',1:2), 'thetaCor')
model3Par <- c('lp__', 'sigma', 'theta', paste0('threshold',1:2), 'flowLoadings', 'flow')
model4Par <- c('lp__', 'sigma', 'theta', paste0('threshold',1:2), 'thetaCor')

load(paste0(outputDir(), "fit1s1.rda"))
head(worstRhat(fit1s1, model1Par), n=20)
rm(fit1s1)

load(paste0(outputDir(), "fit1s2.rda"))
head(worstRhat(fit1s2, model1Par), n=20)
rm(fit1s2)

load(paste0(outputDir(), "fit2t1.rda"))
head(worstRhat(fit2t1, model2Par), n=20)
rm(fit2t1)

load(paste0(outputDir(), "fit2t2.rda"))
head(worstRhat(fit2t2, model2Par), n=20)

pval <- ppc(fit2t2, rcd)
print(sum(pval<.05) / length(pval))
print(apply(pval, 1, function(x) sum(x<.05)))
print(apply(pval, 2, function(x) sum(x<.05)))

rm(fit2t2)

load(paste0(outputDir(), "fit2t3.rda"))
head(worstRhat(fit2t3, model4Par), n=20)

pval <- ppc(fit2t3, rcd)
print(sum(pval<.05) / length(pval))
print(apply(pval, 1, function(x) sum(x<.05)))
print(apply(pval, 2, function(x) sum(x<.05)))

rm(fit2t3)

load(paste0(outputDir(), "fit2t4.rda"))
head(worstRhat(fit2t4, model1Par), n=20)
rm(fit2t4)

load(paste0(outputDir(), "fit2t5.rda"))
head(worstRhat(fit2t5, model3Par), n=20)

pval <- ppc(fit2t5, rcd)
print(sum(pval<.05) / length(pval))
print(apply(pval, 1, function(x) sum(x<.05)))
print(apply(pval, 2, function(x) sum(x<.05)))

rm(fit2t5)

load(paste0(outputDir(), "fit2t6.rda"))
head(worstRhat(fit2t6, model2Par), n=20)
rm(fit2t6)
