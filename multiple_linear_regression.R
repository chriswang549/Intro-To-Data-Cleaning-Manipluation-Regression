# Goal is to find the model used to generate our data.



group164data <- read.csv('Group_164.csv')

library(knitr)
library(MASS)

# Fitting a model with environmental variables
M_E <- lm(DV_Y~ E1+E2+E3+E4+E5, data=group164data)
summary(M_E) # r^2 OF .5788
# What does adding the genetic variables do?
M_raw <- lm(DV_Y ~ (E1+E2+E3+E4+E5+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15)^3, data=group164data )
print(M_raw)

plot(resid(M_raw) ~ fitted(M_raw), main='Residual Plot')
# determine a transformation
boxcox(M_raw)

M_trans <- lm( I(log(DV_Y)) ~ (.)^3, data=group164data )
print(M_trans)
summary(M_raw)$adj.r.square #.6698277
summary(M_trans)$adj.r.square #.6597006
# Better residuals vs fitted
plot(resid(M_trans) ~ fitted(M_trans), main='New Residual Plot')





library(leaps)
#Stepwise Regression
M <- regsubsets( model.matrix(M_trans)[,-1], z, nbest = 1 , nvmax=5, method = 'forward', intercept = TRUE )
temp <- summary(M)

# Proposes a suggested model
Var <- colnames(model.matrix(M_trans))
print(Var)
M_select <- apply(temp$which, 1, function(x) paste0(Var[x], collapse='+'))
print(M_select)
kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),caption='Model Summary')
# small increase 3 to 4 and even nsmaller increase 4 to 5
#decreasing Bayesian Information Criterion








M_main <- lm( I(log(DV_Y)) ~ ., data=group164data)
# . here means include all variable from E1 to E5 and from G1 to G15 to the model
temp <- summary(M_main)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients')
#G4 G3 not signiciant
# E2,E3,E5,G9,G13 signficant and most likely to be in our final model



M_2nd <- lm( I(log(DV_Y)) ~ (.)^2, data=group164data)
temp  <- summary(M_2nd)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='2nd Interaction')
# E3 and G5 appear in 2 different interactions
M_2stage <- lm( I(log(DV_Y)) ~ (E2+E3+E5+G5+G9+G13+G12+G11+E1+E4+G3+G7+G8)^3, data=group164data)
temp <- summary(M_2stage)
temp$coefficients[ abs(temp$coefficients[,3]) >= 4, ]
#E2 has a high t value which means this variable is associated with the logarithmn of the outcome variable
# y~ E2+G5E3 + G5E5 + G9G13 is our final model