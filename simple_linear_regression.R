dependentv <- read.csv('Group-164_Dependent Var.csv')
independentv <- read.csv('Group-164_Independent Var.csv')


fulldataset <- merge(dependentv,independentv,by=c("colID"))


library(VIM)
library(mice)
library(lattice)


#y= 101 missing  82 missing x 
#1000-101-82 = 817 has either x or y
# Multiple Imputation Chained Equations

mice_imputes = mice(fulldataset, m=5, maxit = 1000)
Imputed_data=complete(mice_imputes,5)
md.pattern(Imputed_data) #check for imputation

M <- lm(y ~ x, data=Imputed_data)
print(M)
summary(M)
plot(Imputed_data$y ~ Imputed_data$x, main='Scatter : y ~ x', pch=19)
abline(M, col='red', lty=5, lwd=5)
legend('topright', legend='Estimated Regression Line',
       lty=3, lwd=2, col='red')
anova(lm(y~x, data= Imputed_data))
print(anova(lm(y~x, data= Imputed_data)))

#confidence interval of the intercept
ci <- 0.139564 + 0.003181*c(-1.96,1.96)
print(ci)
#confidence itnerval of slope
ci2 <- 37640.375636 + 4.152977              *c(-1.96,1.96)
print(ci2)





library(dplyr)




#PART B PART B PART B PART B PART B PART B PART B PART B PART B PART B PART B PART B PART B PART B PART B PART B PART B PART B



library(PerformanceAnalytics)
library(rcompanion)
library(moments)

library(MASS)



datapartb <- read.csv('Dat for Group_164.csv')
# lambda around 1.15 which means no transformation
#confirm that below
boxcox <- boxcox(datapartb$Y ~datapartb$X)
lambda <- boxcox$x[which.max(boxcox$y)]
plot(datapartb)
skewness(datapartb) # left skewed y and x by a small amount
summary(lm(Y~X, data=datapartb)) #R^2 = 0.6879

#transformations
#1/dv  1/iv = .6267
#1/dv  1/sqrtiv = .657
#1/dv   logiv= .6774
#1/dv   sqrtiv= .6867
#1/dv     same iv= .6852
#1/dv      iv^2= .6573

#1/sqrtdv  1/iv= .6239
#1/sqrtdv 1/sqrtiv= .6551
#1/sqrtdv logiv = . .6765
#1/sqrtdv  sqrtiv= .6869
#1/sqrtdv sameiv= .6865
#1/sqrtdv iv^2= .6603


#log DV 1/iv = .6208
#log DV 1/sqrtiv = .6529
#log DV log iv = .6753
#log DV sqrt iv= .6867
#log DV same iv = .6873
#log DV iv^2 = .663

#sqrt DV 1/IV = .6174
#sqrt DV  1/sqrtiv .6503
#sqrt DV  log iv= .6737
#sqrt DV  sqrtIV = .6861
#sqrt DV Same IV = .6852
#sqrt DV IV^2 = .6652
# same DV  1/IV = .6137
# same DV   1/sqrtiv = .6475
# same DV   log iv = .6718
# same DV    sqrtIV = .6852
# same DV    Same IV = .6879
# same DV     IV^2  = .6672
# DV^2 1/IV= ,6054
# DV^2 1/sqrt(IV) =.6408
# DV^2 logiv= .6669
# DV^2 sqrtiv= 0.6823
# DV^2 same IV = .687
# DV^2 IV^2 =.67
newy = (datapartb$Y)^2
samex = (datapartb$X)
summary(lm(newy~samex, data=datapartb))



#Residuals vs Fitted to see the linearity, variance, 
#Residuals appear to bounce around the 0 line which suggests the relationship is fairly linear
t <- lm(datapartb$Y~datapartb$X,data=datapartb)
plot(t)
library(MASS)
library(alr3)
library(car)
library(carData)
pureErrorAnova(T)

#Lets see if binning our data has any effect on our model
# Binning Conceptually
# Take Near Repeated IV (X) values and bin them to an interval
# Take the average of the numbers in the interval
# Replace our data (numbers in each interval) with the average of the interval
# Replaces data , doesn't add or remove extra rows
grp <- cut(datapartb$X, breaks = c(-Inf, seq(500.8804, 1997.968, by = 1), Inf))
lst1 <- split(datapartb$X, grp)

length(lst1)
library(dplyr)
bin_data <- ave(datapartb$X, grp) %>% 
  as.data.frame() %>%
#ave spits out a vector of x values so we want the y's back
bin_data$y <- datapartb$Y
binnedModel <- lm(bin_data$y ~ bin_data$.)
pureErrorAnova(binnedModel)
plot(binnedModel)
# Binning the data has little effect on our model
# p value of lack of fit > .1 (alpha)
  

