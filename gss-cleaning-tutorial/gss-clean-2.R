#install.packages("survey")
library(janitor)
library(tidyverse)
library(survey)

data <- read_csv("gss.csv")

# find a mean so we can create a threshold
thresh = round(mean(gss$feelings_life, na.rm=TRUE))
# quantile(gss$feelings_life, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

data = data %>% 
  mutate(is_satisfied = if_else(feelings_life >= thresh, 1, 0), na.rm = TRUE) 

N = 30530800
n = length(data$is_satisfied)
fpc.srs = rep(N, n)

satisfied.design <- svydesign(id=~1, data=data, fpc=fpc.srs)
satisfied.glm = svyglm(is_satisfied ~ marital_status + time_off_work_birth, 
                       satisfied.design, family="binomial")

