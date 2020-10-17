#install.packages("survey")
#install.packages("brms")
library(janitor)
library(tidyverse)
library(survey)
library(brms)

data <- read_csv("gss.csv")

# glm model
N = 30530800
n = length(data$feelings_life_binary)
fpc.srs = rep(N, n)

satisfied.design <- svydesign(id=~1, data=data, fpc=fpc.srs)

satisfied.glm <- svyglm(feelings_life_binary ~ as.factor(vis_minority) + as.factor(hours_worked) + as.factor(hh_type) + 
                         as.factor(family_income) + as.factor(self_rated_health) + as.factor(self_rated_mental_health) + 
                         as.factor(edudation),
                        design=satisfied.design, family="binomial")

satisfied.glm %>% 
  broom::tidy() %>% 
  knitr::kable()

glm_step_bic <- step(satisfied.glm, k=log(n), trace=0)
glm_step_bic

glm_step_bic %>% 
  broom::tidy() %>% 
  knitr::kable()

# bayesian model
satisfied.brm <- brm(feelings_life_binary ~ as.factor(vis_minority) + as.factor(hours_worked) + as.factor(hh_type) + 
                       as.factor(family_income) + as.factor(self_rated_health) + as.factor(self_rated_mental_health) + 
                       as.factor(edudation),
                     data=data,
                     family=bernoulli(),
                     seed=420)

