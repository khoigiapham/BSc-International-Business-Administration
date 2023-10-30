library(wooldridge)
library(zoo)
library(xts)
library(dynlm)
library(lmtest)
library(car)
library(Hmisc)
library(stargazer)
library(sandwich)
library(forecast)
library(fpp2)
library(ggplot2)
library(plm)
library(Rcpp)
library(car)
library(dplyr) 


# Load the data set 'intdef'
data(intdef, package='wooldridge')
intdef755 <- intdef
# Rename variables
intdef755 <- rename(intdef755, year755 = year, i3755 = i3, inf755 = inf, def755 = def)

# Load the data set 'Gasoline'
data(Gasoline, package = 'plm')
Gasoline755 <- Gasoline
# Rename variables
Gasoline755 <- rename(Gasoline755, country755 = country, year755 = year, lgaspcar755 = lgaspcar, lincomep755 = lincomep, lrpmg755 = lrpmg, lcarpcap755 = lcarpcap) 
rm(Gasoline)
rm(intdef)

##PART A*****************
#Q3
# Define variable def755from the data "intdef755" as a ts object 
def755_ts <- ts(intdef755$def755, start = 1948)

# Plot time series
plot(def755_ts)

#Q2
# Define a "zoo" object containing all data
intdef755_zoo <- zoo(intdef755, order.by = intdef755$year755)

# Time series plot of variable def755
plot(intdef755_zoo$def755)

#Q3---------------------------------------
# Generate the lags of variable def755 manually 
# Create a variable for def755-1: lagged by one time unit  
intdef755['Ldef755'] <- Lag(intdef755$def755, +1)
# Create a variable for price_t-2: lagged by two time units  
intdef755['Ldef7552'] <- Lag(intdef755$def755, +2)
# Create a variable for price_t-3: lagged by three time units  
intdef755['Ldef7553'] <- Lag(intdef755$def755, +3)

# A FDL model: Run a linear regression using lm command
fdl_lm <- lm(i3755 ~ inf755 + def755 + Ldef755 + Ldef7552 + Ldef7553, data=intdef755)
summary(fdl_lm)

#Q4-----------------------------
# Define yearly time series as a ts object
intdef755_ts <- ts(intdef755, start=1948)

# A FDL model: Run a linear regression using dynlm command
fdl_dyn <- dynlm(i3755 ~ inf755 + def755 + L(def755) + L(def755, 2) + L(def755, 3), 
                 data=intdef755_ts)
summary(fdl_dyn)
#Table of regression result
stargazer(fdl_lm, fdl_dyn, type="text")

#Q5--------------------------------
#FDL model with time trend
fdl_dyn_t <- dynlm(i3755 ~ inf755 + def755 + L(def755) + L(def755, 2) + L(def755, 3) + trend(intdef755_ts), 
                 data=intdef755_ts)
summary(fdl_dyn_t)

#Compare the two models using anova
anova(fdl_dyn, fdl_dyn_t)

#Q6---------------------------------
# Calculate the estimated value of long run propensity (LRP) of variable def755  
b <- coef(fdl_dyn)
b["def755"]+b["L(def755)"]+b["L(def755, 2)"]+b["L(def755, 3)"]

# Test whether LRP is significant
# F test H0: LRP=0
linearHypothesis(fdl_dyn,"def755 + L(def755) + L(def755, 2) + L(def755, 3) = 0")
# It is significant (F test, p value = 2.749e-05 < 0.05)
# The estimated long run propensity (LRP) is 0.6317375 

#PartB************************
#Q1
head(Gasoline755)

#Q2--------------------
# For every country755, calculate the mean of gross lincomep755 across different years  
d1 <- aggregate(Gasoline755$lincomep755, list(Gasoline755$country755), FUN = mean)

# Rename the columns of the data frame d1
colnames(d1) <- c("country755", "m_lincomep755")

# Combine the data frame d1 with the original data set Gasoline755
Gasoline755 <- left_join(d1, Gasoline755)

#Q3------------
# Define panel data frame
Gasoline755_pdata <- pdata.frame(Gasoline755, index = c("country755", "year755"))
# What are panel dimensions?
pdim(Gasoline755_pdata)
# What time-invariant and individual-invariant variables? 
pvar(Gasoline755_pdata)

#Q4------------
#Plot 1: A plot of dependent variable lgaspcar755 and year755 for every country
ggplot(data = Gasoline755, aes(x = year755, y = lgaspcar755)) + 
  geom_line(aes(colour = as.factor(country755))) + 
  labs(x = "Year",  y = "Log(gaspcar)")

#Plot 2: A plot for fixed effects: Heterogeneity across entities
#For every country, calculate the mean of gross lgaspcar755 across different country  
d1 <- aggregate(Gasoline755$lgaspcar755, list(Gasoline755$country755), FUN = mean)

#Rename the columns of the data frame d1
colnames(d1) <- c("country755", "m_lgaspcar755")

#Combine the data frame d1 with the original data set Grunfeld
d2 <- left_join(d1, Gasoline755)

#Use ggplot to make a plot 
ggplot(data = d2, aes(x = as.character(country755), y = lgaspcar755)) +
  scale_x_discrete(labels = as.character(Gasoline755$country755), 
                     breaks = Gasoline755$country755) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point() +
  geom_line(aes(x = as.numeric(country755), y = m_lgaspcar755), col = "red") +
  labs(x = "Country", y = "Log(gaspcar)")

#Q5------------------------------
#LSDV:
fe_lsdv <- lm(lgaspcar755 ~ lincomep755 + lrpmg755 + lcarpcap755 + factor(country755), data = Gasoline755)
summary(fe_lsdv)

# Fixed effects (FE) estimator (or within estimator) 
fe_plm <- plm(lgaspcar755 ~ lincomep755 + lrpmg755 + lcarpcap755, data = Gasoline755, 
              index = c("country755", "year755"), effect = "individual", 
              model = "within")
summary(fe_plm)

#stargazer
stargazer(fe_lsdv,fe_plm,
          type="text", 
          column.labels=c("LSDV", "FE"),
          keep=c("lincomep755", "lrpmg755", "lcarpcap755"))

#Q7--------------------
#Fixed effects (FE) estimator (or within estimator) with time fixed effect
fe_plm_time <- plm(lgaspcar755 ~ lincomep755 + lrpmg755 + lcarpcap755, data = Gasoline755, 
                   index = c("country755", "year755"), effect = "twoways", 
                   model = "within")
summary(fe_plm_time)
# Test whether we should add time fixed effects for FE estimator
pFtest(fe_plm_time, fe_plm)

#Q8------
#Includes time fixed effect (adding year dummies as IV)
#Pooled OLS model
pooled_plm_time <- plm(lgaspcar755 ~ lincomep755 + lrpmg755 + lcarpcap755 
                       + factor(year755), data = Gasoline755, index = c("country755", "year755"), 
                       model = "pooling")
summary(pooled_plm_time) 

#FE model
fe_plm_time_1 <- plm(lgaspcar755 ~ lincomep755 + lrpmg755 + lcarpcap755
                     + factor(year755), data = Gasoline755, 
                   index = c("country755", "year755"), effect = "individual", 
                   model = "within")
summary(fe_plm_time_1)

#RE model
re_plm_time <- plm(lgaspcar755 ~ lincomep755 + lrpmg755 + lcarpcap755
                     + factor(year755), data = Gasoline755, 
                     index = c("country755", "year755"), effect = "individual", 
                     model = "random")
summary(re_plm_time)


# Pooled OLS vs. FE model (pFtest)
pFtest(fe_plm_time_1, pooled_plm_time)

# FE model vs. RE model (Hausman Test)
phtest(fe_plm_time_1, re_plm_time)

# Stargazer       
stargazer(pooled_plm_time, re_plm_time, fe_plm_time_1, 
          type="text", 
          column.labels=c("Pooled OLS", "RE", "FE"),
          keep=c("lincomep755", "lrpmg755", "lcarpcap755")) 



# p-value < 2.2e-16, 
# F statistic = 113.35, p-value < 2.2e-16 < 0.05, the null hypothesis that 
# there is no individual fixed effects is rejected. We should use the FE 
# estimator rather than the pooled OLS.

# p-value < 2.2e-16 < 0.05, the null hypothesis that the preferred model is RE 
# model is rejected. We should choose the FE model rather than the RE model. 

# Therefore, in conclusion, we should choose the FE model. 

# To conclude, the above two tests have already told us FE model is preferred than
# both pooled OLS and RE model.

#Q9------
# Test for serial correlation: 
pbgtest(fe_plm_time_1)

#Robust standard errors
coeftest(fe_plm_time_1, vcovHC)

# Calculate robust standard errors for FE model  
fe_plm_time_cov <- vcovHC(fe_plm_time_1)
fe_robust_se <- sqrt(diag(fe_plm_time_cov))

#Stargazer
stargazer(fe_plm_time_1, fe_plm_time_1,
          type="text", 
          column.labels=c("FE", "FE r.se."),
          se = list(NULL, fe_robust_se),
          keep=c("lincomep755", "lrpmg755", "lcarpcap755")) 

#Breusch-Godfrey/Wooldridge test: P-value = 1.856e-07 < 0.05: the null hypothesis that there is no serial correlation is rejected. There is strong evidence for serial correlation. We should use robust standard errors








