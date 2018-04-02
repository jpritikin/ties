Description of files

survey*.pdf -- Copies of the surveys used to collect the data

data-mturk.csv, data-public.csv -- Raw data

genCloud -- Coding, topological sort, generate demographics

rawData.csv, demogr.csv -- Output from genCloud

disconnected.csv -- List of disconnected activities (from genCloud)

qraw.R -- A small R program to query raw data

prepDemogr.R -- Data cleaning for demographics

modelUtil.R -- Various R code common to many script

model1.stan -- Independence model

model2.stan -- Saturated model

model3.stan -- Factor model model

model4.stan -- Saturated model with per-item thresholds

f?.R -- scripts to run Stan models

sim6.R -- Simulate factor model data

checkSim6.R -- Check correlation between true parameters and recovered
mean posterior parameters

fitStats.R -- Outputs R-hat statistics to check model convergence

cluster.R -- Analysis of the item correlation matrix

thresholdCmp.R -- Compare common vs per-item thresholds

threshplot.R -- Extract data to plot category response curves from item parameters

factorCmp.R -- Comparison of independence, factor, and saturated models

genFlowData.R -- Extract results from final factor model

plotAllItems.R -- Plot activities by item to byFacet.pdf

reportSampleSize.R -- Report sample size by activity

runStan -- Shell script to run all other scripts

genWebResults.R -- Extract reports for preliminary results website
