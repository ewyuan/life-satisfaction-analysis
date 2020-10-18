#install.packages("survey")
#install.packages("brms")
rm(list=ls())
library(janitor)
library(tidyverse)
library(survey)
library(brms)
library(pROC)

data <- read_csv("gss.csv")

# glm model
N = 30530800
n = length(data$feelings_life_binary)
fpc.srs = rep(N, n)

satisfied.design <- svydesign(id=~1, data=data, fpc=fpc.srs)

satisfied.glm <- svyglm(feelings_life_binary ~ age + as.factor(vis_minority) + as.factor(hours_worked) + as.factor(hh_type) + 
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

#Goodness of Fit: ROC curve
roc_curve <- function(glm){
  p2 <- fitted(glm)
  y <- data$feelings_life_binary
  roc_logit2 <- roc(y ~ p2)
  TPR <- roc_logit2$sensitivities
  FPR <- 1 - roc_logit2$specificities
  plot(FPR, TPR, xlim =c(0,1), ylim =c(0,1), type ='l', lty = 1, lwd = 2,col ='red', bty = "n", main="ROC")
  abline(a = 0, b = 1, lty = 2, col ='blue')
  text(0.7,0.4,label =paste("AUC = ",round(auc(roc_logit2),4)))
  auc(roc_logit2) 
}

par(mfrow=c(1,2)) # put two graphs on same line
roc_curve(satisfied.glm)
roc_curve(glm_step_bic)

# plot model estimates
library(sjPlot)

plot_model(satisfied.glm, type = "pred", terms = c("hh_type"))
plot_model(satisfied.glm, type = "pred", terms = c("vis_minority"))
plot_model(satisfied.glm, type = "pred", terms = c("hours_worked"))
plot_model(satisfied.glm, type = "pred", terms = c("family_income"))
plot_model(satisfied.glm, type = "pred", terms = c("self_rated_health"))
plot_model(satisfied.glm, type = "pred", terms = c("self_rated_mental_health", "vis_minority"), value.offset = .9, axis.labels = "")
plot_model(satisfied.glm, type = "pred", terms = c("edudation"))

library(visreg)
visreg(satisfied.glm)

# bayesian model
#satisfied.brm <- brm(feelings_life_binary ~ as.factor(vis_minority) + as.factor(hours_worked) + as.factor(hh_type) + 
#                       as.factor(family_income) + as.factor(self_rated_health) + as.factor(self_rated_mental_health) + 
#                       as.factor(edudation),
#                     data=data,
#                     family=bernoulli(),
#                     seed=420)

#satisfied.brm %>% 
#  broom::tidy() %>% 
#  knitr::kable()

