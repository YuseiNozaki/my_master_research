library(psych)


dat = read.csv('~/master_research/sem_data5.csv')


result = cor.test(dat$confidence_in_science, dat$interest_in_technology)

print(result)

