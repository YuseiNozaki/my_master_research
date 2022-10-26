library(ltm)


dat = read.csv('~/master_research/irt_data.csv')

dat = dat[, -1]
dat = dat[, -5]


result = grm(dat)

print(result)


result_2 = factor.scores(result, method='EB', resp.patterns=dat)

print(result_2)
