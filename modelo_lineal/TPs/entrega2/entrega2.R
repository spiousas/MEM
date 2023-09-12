pacman::p_load(tidyverse, here, openintro)

data(bdims)
bdims

modelo <- lm(data = bdims,
             wgt ~ bic_gi + kne_di + ank_gi)
summary(modelo)

# a ####
round(modelo$coefficients, digits = 2)

# b ####
sum_modelo <- summary(modelo)
sum_modelo

# c ####
round(sum_modelo$coefficients, digits = 2)

# d ####
round(sum_modelo$sigma, digits = 2)

# e y f ####
round(confint(modelo, level = .9), digits = 2)

# g ####
new_data <- tibble(bic_gi = 31.2, kne_di = 18.8, ank_gi = 22.2)
round(predict(modelo, newdata = new_data), digits = 2)

# h ####
which.max(abs(sum_modelo$residuals))

# i ####
round(max(abs(sum_modelo$residuals)), digits = 2)
