library(psych)
library(GPArotation)


dat = read.csv('~/master_research/fa_data.csv')
dat = dat[,-1]

dat = dat[, -18]


result = fa(dat, nfactors=6, rotate='promax')
print(result)
fa.diagram(result)
