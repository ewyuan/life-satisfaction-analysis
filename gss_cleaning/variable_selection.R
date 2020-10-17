library(tidyverse)
library(car)

model <- lm(feelings_life ~ ., data = select(gss_raw, !feelings_life_binary))

car::vif(model)
