


# Q 10.18


rm(list=ls())
library(tidyverse)
library(mosaic)

#' This data frame contains data on wages of married and unmarried women... 
#'  
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/mroz.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"))
head(mroz)

#' women working
work <- mroz %>% filter(lfp==1)

##########################################
# OLS
fit_ols <- lm(log(wage) ~ educ+exper+I(exper^2), data = work)
summary(fit_ols)
# education could be endogenous, as Education is 
#positively correlated with the omitted variable "ability", meaning   
#that the estimated rate of return of educations, 10.86% may be overstate the true value.
# Hence, Use IV estimation method 
# But what might we use as an IV?
# Education of mother and father might be used as an instrument.
cor(educ ~ mothereduc, data = work)
cor(educ ~ fathereduc, data = work)

# So use these instruments to estimate the model using IVreg
#' Example 10.5
#' Here we assume exper is exogeneous 
library(AER)
fit.iv <- ivreg(log(wage) ~ educ+exper+I(exper^2) |exper+I(exper^2)+mothereduc+fathereduc, data = work)
summary(fit.iv)

#' Alternatively, using sem package 
library(sem) 
fit_2sls <- tsls(log(wage) ~ educ+exper+I(exper^2),~exper+I(exper^2)+mothereduc+fathereduc, data = work)
summary(fit_2sls)
##################################################


# a)

mroz <- mroz %>% filter(lfp==1) %>% 

  mutate(mothercoll = ifelse(mothereduc >12,1,0), 
         fathercoll = ifelse(fathereduc >12,1, 0))
dim(mroz)

# If we look at the percentage of parents that have some college education 
mroz  %>%
  summarise(
    pct_mothercoll = mean(mothercoll, na.rm = TRUE) * 100,
    pct_fathercoll = mean(fathercoll, na.rm = TRUE) * 100
  )
#In this sample 12.15% of mothers have some college education, while
# 11.68% of fathers have some college education.

# b)
mroz %>%
  summarise(
    corr_educ_mother = cor(educ,mothercoll),
    corr_educ_father = cor(educ,fathercoll)
  )
# The correlation between EDUC and MOTHERCOLL is 0.3595. 
# The correlation between EDUC and FATHERCOLL is 0.3985. 

# These variables are better instruments than mothereduc and fathereduc since 
#they have larger correlation with educ, the endogeneous variable.  
cor(educ ~ mothereduc, data = mroz)
cor(educ ~ fathereduc, data = mroz)

# c) 
iv <- ivreg(log(wage) ~ educ+exper+I(exper^2)|exper+I(exper^2)+mothercoll, data = mroz)
summary(iv)

#' 2SLS Estimation for Demand
library(sem)
fit_2sls_c <- tsls(log(wage) ~ educ+exper+I(exper^2),~exper+I(exper^2)+mothercoll, data = mroz)
summary(fit_2sls_c)

confint(iv)[2,]
confint(fit_2sls_c)[2,]
#The 95% interval estimate for the coefficient of EDUC is [−0.000858, 0.1528939].


# d) Estimate the first-stage equation?
#    Is mothercoll a stong instrument?
first_stage_d <- lm(educ ~exper+I(exper^2)+mothercoll, data = mroz)
summary(first_stage_d)

# using F-test 
library(car)
linearHypothesis(first_stage_d, ("mothercoll=0"))

# F= 63.563.  
# The t-statistic for the coefficient
# of MOTHERCOLL is 7.97 and the corresponding F-value is 63.56. 
# This is far greater than the rule-of-thumb value of 10,
# so we reject the notion that the IV is weak on this basis. 

# e) Estimate the wage equation using both
# MOTHERCOLL and FATHERCOLL as IVs.
fit_2sls_e <- tsls(log(wage) ~ educ+exper+I(exper^2),~exper+I(exper^2)+mothercoll+fathercoll, data = mroz)
summary(fit_2sls_e)

# What is the 95% interval estimate for the coefficient of EDUC?
# It
confint(fit_2sls_e)[2,]

# #The 95% interval estimate for the
# coefficient of EDUC is [0.027801, 0.1478943]. 
#This is slightly narrower than the interval
# estimate using only MOTHERCOLL as an IV as shown in part (c).


# f)Do the instruments, mothercoll and fathercoll adquetly strong?
first_stage_f <- lm(educ ~exper+I(exper^2)+mothercoll+fathercoll, data = mroz)
summary(first_stage_f)

# Test the joint significance of of mothereduc & fathereduc
linearHypothesis(first_stage_f, c("mothercoll=0", "fathercoll=0"))

#The F-test statistic of the joint significance of the two IV is 56.96. Far greater than
# the rule of thumb value of 10 for a weak IV. Thus we reject the null hypothesis that the instruments
# are weak.

# g) check the validity of the surplus instruments. What do you conclude?
mroz$res <- resid(fit_2sls_e)

surplus_inst <- lm(res ~ exper+I(exper^2)+mothercoll+fathercoll, data = mroz)
summary(surplus_inst)

# compute NR2, The Sargan test,
sargan_test <- nobs(surplus_inst)*summary(surplus_inst)$r.squared
sargan_test

# critical value 
#Compare to chi-square
qchisq(0.95, df = 1)
# so we fail to reject the surplus instrument as valid. 




###################################################
# Q 10.20
##################################################

rm(list=ls())
library(mosaic)
library(tidyverse)


#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/capm5.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"))
head(capm5)

names(capm5)

# The data contains risk premium on security of different companies/stocks
# and their market portfolio. 

# The CAPM (Capital Asset Pricing Model) is a model that describes the
# relationship between asset return and market return,

#' rj-rf = aj + b(rm-rf) + ej
#' 
#' where rj= is the returns to security j, 
#'       rf= is the risk-free rate.
#'       rm = is the return to the market portfolio.

# b of the asset, measuring its sensitivity to market movements.
# A value of b greater than 1 indicate an "aggressive stock" or volatile stock/risky 
# A value of b less than 1 indicates that the stock is "defensive", since its variation 
# is less than the market's.

# Is this model has an endogeneity issue?
# Yes, the CAPM model has an endogeneity issue because the 
#market return rm is measured with error 
# (e.g., you don’t perfectly capture the true market portfolio),
# The market portfolio in CAPM is the value-weighted portfolio of all risky assets 
# in the world, not just stocks
# Hence, b estimates become biased and inconsistent

# create the required variables 
capm5 <- capm5 %>% 
  mutate(expvar = mkt-riskfree, 
         depvar = msft-riskfree)
names(capm5)

# a) Estimate the CAPM model for Microsoft (MSFT) using OLS.

fit_msft <- lm(depvar ~ expvar, data = capm5) 
summary(fit_msft)  

confint(fit_msft)[2,] 
#'The estimated
# Microsoft beta is 1.2018, with a 95% interval estimate of [0.9607882, 1.442891]. 
#"Most of the interval estimate is greater than 1, indicating that 
#' the stock is relatively risky compared to the market portfolio. 
# Note however that the null hypothesis that Microsoft’s beta is equal to 1
# cannot be rejected at the 5% level.

# b) 

#construct IV by ranking the values of the explanatory variable,from smallest to largest
capm5  <- capm5 %>% 
          arrange(expvar) %>% mutate(rank= 1:length(expvar)) #%>% View()


### Does the variable rank satisfy the condition IV1-IV3?

# The variable rank is has no obvious causal effect on Microsoft's return, so it satisfies IV1
  
# First stage regression 
first_stage <- lm(expvar ~ rank, capm5)
summary(first_stage)
#The R2 = 0.9126 is very large.
#The t-value is 43.10,and greater than the rule of thumb value of 3.36 which is huge. Thus it
#seems to be an extremely strong IV, if it is valid.
  
# Alternative, using the F-statistics.
library(car)
linearHypothesis(first_stage, c("rank=0"))  
# The F-statistic is 1856.2, which is also much greater than the rule of thumb value of 10 for a weak IV.


# c) The Hausman test for endogeneity.
# Compute the the first-stage -residual & add in the CAPM 
capm5$res <- resid(first_stage)
 
fit2 <- lm(depvar ~ expvar+res, data = capm5) 
summary(fit2) 
#' The t-statistic on the residuals, i.e., *res* is −2.04 with a p = 0.043. 
#' It is not significant at the 1% level but it is at the 5% level. 
#' So, at the 1% level we cannot reject the null hypothesis that the market return is exogenous. 
 

# d)  Estimate the CAPM using 2SLS using Rank as an IV 

#' 2SLS Estimation 
require(sem) 
fit_2sls <- tsls(depvar ~ expvar, ~rank, data=capm5)
summary(fit_2sls)

# & compare the estimates with the  OLS estimates in part (a)
summary(fit_msft)
#' If there is a measurement error problem, the OLS estimator suffers attenuation bias, 
#' that is, it is biased downward. The IV estimate is slightly larger, 
#' which is what we would expect. 

#Confidence Interval, CI
Confint(fit_2sls)[2,]
Confint(fit_msft)[2,]
#' The 95% interval estimate for the 2SLS is [1.028819, 1.527817].
#' All values are greater than 1 and thus we would reject the null hypothesis
#' that Microsoft’s beta equals 1.


# e) Creat a new variable pos= 1 if the explain  is positive, 0 otherwise
capm5 <- capm5 %>% 
  mutate(pos = ifelse(expvar >0,1,0))

# First stage regression, using both rank + pos as IVs
first_stage_e <- lm(expvar ~ rank+pos, data=capm5)
summary(first_stage_e)

# Test the joint significance of the IVs. 
# can we conclude that we have adequately strong IV?
linearHypothesis(first_stage_e, c("rank=0", "pos=0"))
#' The F-test
#' statistic for the joint significance of these variables is 924.28. Obviously RANK remains
#' strongly significant but POS is not significant. The R2 = 0.9126 is very high.
#' Based on the Ftest
#' we would conclude the IV are not weak. Also, the F > 10.  If we are willing to accept a test Type I error
#' of 10% for a 5% test then we can conclude the IV are not weak.


# f) Carry out the Hausman test for endogeneity using the residual from the first-stage 
res <- resid(first_stage_e)
fit3 <- lm(depvar ~ expvar+res, data = capm5) 
summary(fit3) 
#' The t-value on the first stage residuals,
#' which we have called VHAT2, is −2.27 with a p-value of 0.025. Thus at the 1%
#' level we cannot reject the null hypothesis that the market return is exogenous.

# g) IV/2SLS estimates of the CAPM model using RANK and POS as IVs

fit_2sls <- tsls(depvar ~expvar, ~rank+pos, data=capm5)
summary(fit_2sls)

#' #'The coefficient
#' estimate 1.2866 is larger than the OLS estimate from part (a). If there is a measurement error
#' problem the OLS estimator suffers attenuation bias, that is, it is biased downward. The IV
#' estimate is slightly larger, which is what we would expect. 

confint(fit_2sls)[2,]
#' The 95% Confidence interval estimate is
#' [1.037005, 1.536111]. All values are greater than 1 and thus we would reject the null hypothesis
#' that Microsoft’s beta equals 1

# h) Testing Validity of surplus IV (also known as Sargan test)

# Obtain the IV/2SLS residuals from (g) 
res <- resid(fit_2sls) 

surplus_inst<- lm(res ~ rank+pos, data = capm5)  
summary(surplus_inst)  
  
# compute NR2, The Sargan test,
sargan_test <- nobs(surplus_inst)*summary(surplus_inst)$r.squared
sargan_test

# critical value 
#Compare to chi-square
qchisq(0.95, df = 1)

#' #'The test statistic
#' NR2 = 63.03 is greater than the 95th percentile of the chi-square (1) value which is  3.841459.
#'Thus we reject the validity of the surplus IV. 
#'We conclude that something is wrong and must go in search of new IV.




# Q 10.17 
rm(list=ls())

library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/mroz.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"))
head(mroz)


#' women working
work <- mroz %>% filter(lfp==1)

#a)

fit <- lm(educ ~ exper+I(exper^2)+mothereduc, data = work)
summary(fit)

REDUCHAT <- resid(fit)

# The Sums of squared residuals, SSE
SSE <-sum(REDUCHAT^2)
# or more simply 
SSE <- deviance(fit)   

# b)
fit2 <- lm(educ ~ exper+I(exper^2), data = work)

# residual 
REDUC <- resid(fit2)
SSE2 <- deviance(fit2)


# c)
fit3 <- lm(mothereduc ~ exper+I(exper^2), data = work)
#residual
RMOM <- resid(fit3)
SSE3 <- deviance(fit3)

# d)
fit4 <- lm(REDUC ~ RMOM, data = work)
summary(fit4)
SSE4 <- deviance(fit4)

# e)
SSE3
SSE2*(coef(fit4)[2])^2








