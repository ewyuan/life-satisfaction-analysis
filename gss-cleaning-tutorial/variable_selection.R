library(tidyverse)
library(car)
gss_ <- gss %>% select(!feelings_life)

model <- lm(feelings_life_binary ~., data = gss_)

car::vif(model)
