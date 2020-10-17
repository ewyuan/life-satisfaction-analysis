#install.packages("survey")
library(janitor)
library(tidyverse)
library(survey)

data <- read_csv("gss.csv")

N = 30530800
n = length(data$feelings_life_binary)
fpc.srs = rep(N, n)

satisfied.design <- svydesign(id=~1, data=data, fpc=fpc.srs)
satisfied.glm = svyglm(feelings_life_binary ~ as.factor(vis_minority) + hours_worked,
                       satisfied.design, family="binomial")
# family_income + hh_type + education + self_rated_health + self_rated_mental_health
satisfied.glm %>% 
  broom::tidy() %>% 
  knitr::kable()