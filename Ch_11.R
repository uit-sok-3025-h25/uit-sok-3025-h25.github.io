
# Chapter 11


#'  Example 11.1 Estimation of demand and supply for truffles

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/truffles.def")

# truffles.def
# 
# p  q  ps  di  pf
# 
# Obs:   30
# 
# p = price of premium truffles, $ per ounce
# q = quantity of truffles traded in a market period, in ounces
# ps = price of choice truffles (a substitute), $ per ounce
# di = per capita disposable income, in units of $1000 per month
# pf = hourly rental price of truffle pig, $ per hour 
# (the price of a factor of production )

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/truffles.rdata"))


require(sem) || {install.packages("sem");require(sem)}
#-------------------------------------------------------

# Table 11.1

head(truffles,5)

summary(truffles)

library(mosaic)
favstats(~p, data = truffles)

#' Try OLS first
demand <- lm(q~p+ps+di, data = truffles)
summary(demand)
#' Note wrong sign on p, and insignificant

supply <- lm(q~p+pf, data = truffles)
summary(supply)


# Table 11.2a (reduced form for quantity of truffles, q)
summary(lm(q~ps+di+pf, data = truffles))

# Table 11.2b (reduced form for price of truffles, p)
summary(lm(p~ps+di+pf, data = truffles))


#-------------------------------------------------------
#' 2SLS Estimation for truffle Demand, Table 11.3a
summary(tsls(q ~ p + ps + di, ~ps + pf + di, data = truffles))

#' 2SLS Estimation for truffle Supply, Table 11.3b 
summary(tsls(q ~ p + pf, ~ps + pf + di, data = truffles))



#' Estimated as a system, systemfit

#' Specify the two equations
eqDemand <- q~ p+ps+di
eqSupply <- q ~ p+pf

#' Put them in a list
eqSystem <- list(demand = eqDemand, supply = eqSupply)

??systemfit

browseURL("https://www.jstatsoft.org/article/view/v023i04") 

require(systemfit) || {install.packages("systemfit");require(systemfit)}

#' Estimate the system
fit2sls <- systemfit(eqSystem, method = "2SLS", inst=~ps+di+pf, data = truffles)
print(fit2sls)
summary(fit2sls)


#install.packages("AER")
library(AER)
#' IV estimation demand
summary(ivreg(q ~ p + ps + di, ~ps + pf + di, data = truffles))

#' IV estimation supply
summary(ivreg(q ~ p + pf, ~ps + pf + di, data = truffles))

# Any difference between 2SLS and IV?
# demand
coef(tsls(q ~ p + ps + di, ~ps + pf + di, data = truffles))-coef(ivreg(q ~ p + ps + di, ~ps + pf + di, data = truffles))
# supply
coef(tsls(q ~ p + pf, ~ps + pf + di, data = truffles))-coef(ivreg(q ~ p + pf, ~ps + pf + di, data = truffles))



#' Do a Hausman test for endogeneity of price in the demand and supply equation

#' Reduced form for price
redf.price <- lm(p~ps+di+pf, data=truffles)
# save residuals
vhat <- resid(redf.price)

#' Hausman test, p. 505, artificial regression
#' Demand
summary(lm(q~p+ps+di+vhat, data=truffles))
#' If we reject the null hypothesis that the coeff on vhat is zero, 
#' we conclude that price is endogenous.
#' vhat is significant at a 5% level.

#' Supply
summary(lm(q~p+pf+vhat, data=truffles))
#' vhat is not significant at a 5% level, hence price is 
#' exogenous in the supply equation.


#----------------------------------------------------------------


#' Example 11.2: Supply and demand at the Fulton Fish Market (in New York) 

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/fultonfish.def")

# fultonfish.def
# 
# obs:  111
# 
# date lprice quan lquan mon tue wed thu stormy mixed rainy cold totr diff change
# 
# date            date
# lprice          log(Price) of whiting( a common type of fish) per pound
# quan            Quantity of whiting sold, pounds
# lquan           log(Quantity)
# mon             Monday
# tue             Tuesday
# wed             Wednesday
# thu             Thursday
# stormy          High wind and waves
# mixed           Mixed wind and waves
# rainy           Rainy day on shore
# cold            Cold day on shore
# totr            Total received
# diff            Inventory change = totr-quan
# change          = 1 if diff large

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/fultonfish.rdata"))

head(fultonfish)

#' Table 11.4a Reduced form for ln(Quantity) Fish
summary(lm(lquan ~ stormy+mon+tue+wed+thu, data = fultonfish))

#' Table 11.4b Reduced form for ln(Price) Fish
summary(lm(lprice ~ stormy+mon+tue+wed+thu, data = fultonfish))
#' None of the daily indicator variables are statistically significant. 

#check their joint significance
library(car)
fit_price= lm(lprice ~ stormy+mon+tue+wed+thu, data = fultonfish)
linearHypothesis(fit_price , c("mon=0","tue=0","wed=0","thu=0"))
#' Also the joint F-test of significance of the daily  indicator variables
#'  indicate that they are not jointly significant.
#' This implies the supply equation is not identified. 

#' Table 11.5 2SLS Estimates for Fish Demand
summary(tsls(lquan ~ lprice+mon+tue+wed+thu , ~mon+tue+wed+thu+stormy, data = fultonfish))




############################################
# Solution to the Exercises
########################################

# 11.18)

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/klein.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/klein.rdata"))

head(klein)

# a)
#' Identification requires 1 omitted variable from each equation. Lagged capital stock appears in
#' the investment equation, but it is not in the consumption equation. Therefore the consumption
#' equation is identified. The investment equation excludes TIME which does appear in the
#' consumption equation, therefore the necessary equation is met.

# b) solve manually

# c)

summary(lm(cn ~ i + time, data = klein))
summary(lm(i ~ cn +klag, data = klein))
# The OLS estimates show that the coefficients on
# investment and time are positive and significant. In the investment equation the coefficient of
# consumption is positive and significant and that for lagged capital stock is negative and
# significant. The signs are as one would expect.

# d)

# consumption function
library(sem)
summary(tsls(cn ~ i + time, ~ klag+time, data = klein))

# investment function 
summary(tsls(i ~ cn+klag,~klag+time, data = klein))
# The results for the consumption
# equation are quite different. The estimated coefficient of investment is negative but
# insignificant. The coefficient on time remains positive and significant. For the investment
# equation both coefficients are negative and insignificant.

# e) Reduced form equation 
summary(lm(cn ~ klag+ time, data = klein))
summary(lm(i ~ klag+time, data = klein))
# In the reduced form equation
# for consumption the lagged capital stock is insignificant. 
# That means that despite the necessary
# condition being satisfied in theory for the consumption equation, 
# it is not in practice.
# We cannot conclude that the lagged capital stock is not a weak instrument. 
# Thus the 2SLS results are not reliable.
# In the investment reduced form TIME is not significant so we have the same situation as for
# the consumption equation. Without TIME being very significant there is no real omitted
# variable in the investment equation, making 2SLS unreliable.



# 11.19

 rm(list=ls())
library(tidyverse)

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/mroz.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"))

head(mroz)

mroz <- mroz %>% mutate(nwifeinc = faminc-wage*hours) #%>% drop_na()

worked <- mroz %>% filter(lfp==1)
notworked <- mroz %>% filter(lfp==0)


#a)

# women worked in the previous years 
worked %>% select(age,kidsl6,faminc, hours,wage,nwifeinc)  %>% summary()

# not worked 
notworked %>% select(age,kidsl6,faminc,hours,wage,nwifeinc)  %>%  summary()
#Women not in the labor force are on average slightly older, and have on average more kids
#under 6 years of age, and a higher income from their partners.

# b) b2 >0, b3 unclear, b4 is also unclear as the age group in the data is between 30 and 60, 
#    b5,b6,b7 <0
#nwifeinc = measures the sum of all family income excluding the wife’s income

# c) 
supp_eq <- lm(hours ~ log(wage)+educ + age+kidsl6+kids618+nwifeinc, data=worked)
summary(supp_eq)
# Did the signs obtained are as expected?
# Well we certainly did not expect the
# coefficient of ln(WAGE) to be negative and insignificant. 
# It is meant to be a supply equation
# so that coefficient should be positive and significant.

# d) Reduced form equation, work experience added as an additional instrument
reduced_eq <- lm(log(wage) ~ educ + age+kidsl6+kids618+nwifeinc+exper, data=worked)
summary(reduced_eq)
#' #'The coefficient of education implies an
#' additional year of education will increase wages by approximately 10.11% holding other
#' factors constant. And the estimate is statistically significant a the 1% level.

# e)
# In order for the supply equation to be identified the coefficient on EXPER should be very
# significant, with an F – test statistic value of at least 10. The t-value is 3.60 which translates
# into F = 12.96, satisfying the rule of thumb threshold for an instrument that is not weak.

# f)
summary(tsls(hours ~ log(wage)+educ + age+kidsl6+kids618+nwifeinc, ~exper+educ + age+kidsl6+kids618+nwifeinc, data = worked))
# We see that ln(WAGE) now has a positive and statistically significant coefficient. 
#The coefficient of EDUC is negative and
# significant, but the rest of the coefficients are insignificant.




# 11.20

rm(list=ls())
library(tidyverse)

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/newbroiler.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/newbroiler.rdata"))

head(newbroiler)

# a)
# The endogenous variables are the logs of Q = per capita consumption of chicken and P = real
# price of chicken. The exogenous variables are logs of Y = real per capita income and PB = real
# price of beef, and POPGRO = rate of population growth

# b)
newbroiler_m <- newbroiler %>% filter(year >=1960, year <=1999)
fit <- lm(log(q) ~ log(p)+log(y)+log(pb)+popgro, data = newbroiler_m)
summary(fit)
# We see that the coefficient of log price (LP)
# is negative but not significant. 
# The coefficient of log income (LY) is positive and significant. It
# suggests that a 1% increase in income increases the consumption of chicken by 0.99%. The
# coefficient of the log of the price of beef is negative and insignificant. 
# Population growth is positive and significant. 
# We estimate that a 1% increase in population growth increases per
# capita chicken consumption by 0.0017 pounds

# C)
# Check for serial correlation in the residuals
res <- resid(fit)
library(forecast)
Acf(res)
# The correlogram shows the first two autocorrelations, 0.5663 and 0.4038, to be
# significant.

# TR^2
newbroiler_m$res <- res
newbroiler_m$res_lag1 <- dplyr::lag(newbroiler_m$res, 1)

aux_model <- lm(res ~ res_lag1, data = newbroiler_m )

#Compute test statistic, T * R2, LM test 
nobs(aux_model)*summary(aux_model)$r.squared

#Compare to chi-square
qchisq(0.95, df = 1)
# We reject the null hypothesis of no serial correlation


# d) 2SLS, using as instruments.....
newbroiler_m <- newbroiler_m %>% 
  mutate(time = year -1949, 
         qprod_lag1 = dplyr::lag(qprod,1),
         lexpts_lag1 = dplyr::lag(lexpts))

fit_2sls <- tsls(log(q) ~ log(p)+log(y)+log(pb)+popgro,~log(y)+log(pb)+popgro+log(pf)+time+log(qprod_lag1)+lexpts_lag1, data = newbroiler_m)
summary(fit_2sls)
# We find the price elasticity estimated to
# be −0.255 and significant at the 5% level. The other results are essentially unchanged. But the
# effect of using 2SLS is have a demand curve that is estimated to be downward sloping, as we
# would expect.

# e)
 # The reduced form 
first_stage <- lm(log(p)~ log(y)+log(pb)+popgro+log(pf)+time+log(qprod_lag1)+lexpts_lag1, data = newbroiler_m)
summary(first_stage)

library(car)
linearHypothesis(first_stage, c("log(pf)=0", "time=0","log(qprod_lag1)=0","lexpts_lag1=0"))
linearHypothesis(first_stage, c( "time=0","lexpts_lag1=0"))
# The coefficient of TIME is statistically
# significant at the 5% level, and lag EXPTS − is significant at the 1% level. The F-test of joint
# significance yields F = 3.81, with a p-value of 0.0124 using the (4,31) F distribution. Thus the
# variables are jointly significant at the 5% level, but that is not really the important point. In
# order to conclude that the instruments are not weak we require F > 10. This condition does not
# hold and we should not conclude that our instruments are adequately strong. Thus we cannot
# have too much confidence in the 2SLS estimates.

# f) see c) above 


# g)
library(lmtest)
library(sandwich)
# OLS estimates and White HAC standard errors, not it is not HC
coeftest(first_stage)
coeftest(first_stage, vcov=vcovHAC, type = c("HAC") )  # ????
# White HAC standard errors, adjust the following tets using HAC standard errors
# F-test of joint significance of the instruments
library(car)
# F-test of joint significance of the instruments
# White standard errors
linearHypothesis(first_stage, c("log(pf)=0", "time=0","log(qprod_lag1)=0","lexpts_lag1=0"), vcov = hccm(first_stage, type = "hc1"))
# In order to conclude that the instruments are not weak we require F > 10. This condition does not
# hold and we should not conclude that our instruments are adequately strong. Thus we cannot
# have too much confidence in the 2SLS estimates.

# h)

resid <- resid(fit_2sls)
library(forecast)
Acf(resid) #The correlogram for the 2SLS residuals shows one significant autorcorrelation.



# 2SLS estimates with HAC standard errors
coeftest(fit_2sls, vcov=vcovHAC, type = c("HAC") )  # ????

#To do this, use IV 
library(AER)        # For ivreg()
library(sandwich)   # For vcovHAC()
library(lmtest)     # For coeftest()

fit_iv <- ivreg(
  log(q) ~ log(p) + log(y) + log(pb) + popgro |
    log(y) + log(pb) + popgro + log(pf) + time + log(qprod_lag1) + lexpts_lag1,
  data = newbroiler_m
)

#Get 2SLS Estimates with HAC Standard Errors

# HAC robust VCOV matrix
hac_vcov <- vcovHAC(fit_iv, lag= 1)  # or NeweyWest(fit_iv, lag = 4)

# Robust summary
coeftest(fit_iv, vcov. = hac_vcov)


