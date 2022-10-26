library(psych)
library(lavaan)
library(semTools)
library(semptools)
library(semPlot)
library(ggplot2)


dat = read.csv('~/master_research/sem_data5.csv')
dat = dat[,-1]


model = '
  interest_in_nature ~~ interest_in_technology
  need_for_cognition ~ interest_in_nature + interest_in_technology
  
  confidence_in_science ~ favorability_to_science
  favorability_to_science ~ need_for_cognition + favorability_to_mathematics + interest_in_nature
  favorability_to_mathematics ~ need_for_cognition
  
  variable_identification ~ confidence_in_science
  recognition_of_causal_relationship ~ variable_identification
'


fit = sem(model, dat)


print(fitMeasures(fit, fit.measures = 'all'))

MI = modificationIndices(fit)

print(MI[order(MI$mi),])


layout = layout_matrix(
  interest_in_nature = c(1, 1),
  interest_in_technology = c(3, 1),
  need_for_cognition = c(2, 2),
  favorability_to_science = c(2, 3),
  favorability_to_mathematics = c(3, 3),
  confidence_in_science = c(1, 3),
  variable_identification = c(1, 4),
  recognition_of_causal_relationship = c(1, 5)
)


fit_path = semPaths(
  fit, whatLabels = 'stand',
  layout = layout,
  nCharNodes = 0,
  style = 'lisrel',
  shapeMan = 'rectangle',
  node.width = 2,
  node.height = 1,
  edge.color = 'black',
  label.cex = 1,
  edge.label.cex = 0.6,
  label.norm = 'OOOOOO'
)


fit_path_p = mark_sig(fit_path, fit, alphas = c('***'=.001, '**'=.01, '*'=.08))


label_list = c(
  interest_in_nature = '自然への\n興味・関心',
  interest_in_technology = '科学技術への\n興味・関心',
  need_for_cognition = '認知欲求',
  favorability_to_science = '理科への\n好感度',
  favorability_to_mathematics = '数学への\n好感度',
  confidence_in_science = '理科への自信',
  variable_identification = '変数の同定',
  recognition_of_causal_relationship = '因果関係\nの認識'
)

fit_path_p_label = change_node_label(fit_path_p, label_list = label_list)


par(family='Japan1')
plot(fit_path_p_label)


