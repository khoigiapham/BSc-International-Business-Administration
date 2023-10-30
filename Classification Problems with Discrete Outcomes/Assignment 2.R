library(readr)
library(tidyverse)
library(dplyr)
library(lmtest)
library(car)
test<- read_csv("testing data set.csv")
train <- read_csv("training data set.csv")

#Q1:
train %>%
  filter(y == 1) %>%
  count()

#Q2:
test %>%
  filter(y == 1) %>%
  count()

#Q4:
mod1 <- glm(y ~ ., data = train, family = binomial)
summary(mod1)

mod2 <- glm(y ~ ., data = test, family = binomial)
summary(mod2)

#Q5:
vif(mod1)

#Q7:
fit_to_test <- predict(mod1, data = train)
fit_preds <- exp(fit_to_test) / (1 + exp(fit_to_test))
predict(mod1, data = train, type = "response")
cutoff <- 0.5
factor_levels <- levels(as.factor(train$y))
train$pred <- factor_levels[(fit_preds > cutoff)+1]
# Build the confusion matrix  #rowx is reality
conf_mat <- xtabs(~ y + pred, data=train)
conf_mat
accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
recall <- conf_mat["1","1"] / (conf_mat["1","1"] + conf_mat["1","0"])

#Q8:
fit_to_test <- predict(mod1, newdata = test)
fit_preds <- exp(fit_to_test) / (1 + exp(fit_to_test))
predict(mod1, newdata = test, type = "response")
cutoff <- 0.5
factor_levels <- levels(as.factor(test$y))
test$pred <- factor_levels[(fit_preds > cutoff)+1]
## Build the confusion matrix  #row is reality
conf_mat <- xtabs(~ y + pred, data = test)
conf_mat
accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
recall <- conf_mat["1","1"] / (conf_mat["1","1"] + conf_mat["1","0"])

#Q12
plot(test$x1, resid(mod2), xlab="X", ylab="Residuals",
     main = "Plot residuals against X1") 
abline(0, 0, col = "red")
plot(test$x2, resid(mod2), xlab="X", ylab="Residuals",
     main = "Plot residuals against X2") 
abline(0, 0, col = "red")
plot(test$x3, resid(mod2), xlab="X", ylab="Residuals",
     main = "Plot residuals against X3") 
abline(0, 0, col = "red")
plot(test$x4, resid(mod2), xlab="X", ylab="Residuals",
     main = "Plot residuals against X4") 
abline(0, 0, col = "red")
plot(test$x5, resid(mod2), xlab="X", ylab="Residuals",
     main = "Plot residuals against X5") 
abline(0, 0, col = "red")

#Q14
df_train_under <- train %>%
  filter(x5 < 0)
df_train_over <- train %>%
  filter(x5 > 0)
mod1a <- glm(y ~ ., data = df_train_under, family = binomial)
summary(mod1a)

#Q15
fit_to_test <- predict(mod1, newdata = df_train_under)
fit_preds <- exp(fit_to_test) / (1 + exp(fit_to_test))
predict(mod1, newdata = df_train_under, type = "response")
cutoff <- 0.5
factor_levels <- levels(as.factor(df_train_under$y))
df_train_under$pred <- factor_levels[(fit_preds > cutoff)+1]
# Build the confusion matrix  #rowx is reality
conf_mat <- xtabs(~ y + pred, data=df_train_under)
conf_mat
accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
recall <- conf_mat["1","1"] / (conf_mat["1","1"] + conf_mat["1","0"])

#Q16
fit_to_test <- predict(mod1a, data = df_train_under)
fit_preds <- exp(fit_to_test) / (1 + exp(fit_to_test))
predict(mod1a, data = df_train_under, type = "response")
cutoff <- 0.5
factor_levels <- levels(as.factor(df_train_under$y))
df_train_under$pred <- factor_levels[(fit_preds > cutoff)+1]
# Build the confusion matrix  #rowx is reality
conf_mat <- xtabs(~ y + pred, data=df_train_under)
conf_mat
accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)
recall <- conf_mat["1","1"] / (conf_mat["1","1"] + conf_mat["1","0"])

#Q17
mod1a <- glm(y ~ ., data = df_train_under, family = binomial)
summary(mod1a)

mod1b <- glm(y ~ ., data = df_train_over, family = binomial)
summary(mod1b)


plot(df_train_under$x5, resid(mod1a), xlab="X5", ylab="Residuals",
     main = "Plot residuals mod1a against X5")
abline(0, 0, col = "red")
plot(df_train_over$x5, resid(mod1b), xlab="X5", ylab="Residuals",
     main = "Plot residuals mod1b against X5")
abline(0, 0, col = "red")


#Q19:
#1a
df_test_under <- test %>%
  filter(x5 < 0)
df_test_over <- test %>%
  filter(x5 > 0)


fit_to_test1a <- predict(mod1a, newdata = df_test_under)
fit_preds1a <- exp(fit_to_test1a) / (1 + exp(fit_to_test1a))
predict(mod1a, data = df_test_under, type = "response")
cutoff <- 0.5
factor_levels <- levels(as.factor(df_test_under$y))
df_test_under$pred <- factor_levels[(fit_preds1a > cutoff)+1]

#1b
fit_to_test1b <- predict(mod1b, newdata = df_test_over)
fit_preds1b <- exp(fit_to_test1b) / (1 + exp(fit_to_test1b))
predict(mod1b, data = df_test_over, type = "response")
cutoff <- 0.5
factor_levels <- levels(as.factor(df_test_over$y))
df_test_over$pred <- factor_levels[(fit_preds1b > cutoff)+1]

Q19 <- rbind(df_test_under, df_test_over)
plot(Q19$x5, resid(mod1a), xlab="X5", ylab="Residuals",
     main = "Plot residuals mod1 against X5")
abline(0, 0, col = "red")

conf_mat <- xtabs(~ y + pred, data=Q19)
conf_mat
accuracy <- (conf_mat["0","0"] + conf_mat["1","1"]) / sum(conf_mat)

#Q21
library(rpart)
library(rpart.plot)

tree <- rpart(y ~ ., data = train, method = "class", maxdepth = 3)
summary(tree)
rpart.plot(tree)
rpart.rules(tree, cover = TRUE)

#Q23
test$preds <- predict(tree, newdata = test, type = "class")
cfm1 <- xtabs(~ y + preds, data = test)
cfm1
accuracy <- (cfm1["0","0"] + cfm1["1","1"]) / sum(cfm1)
accuracy

#Q24
tree6 <- rpart(y ~ ., data = train, method = "class", maxdepth = 6)
summary(tree6)
rpart.plot(tree6)
rpart.rules(tree6, cover = TRUE)

test$preds <- predict(tree6, newdata = test, type = "class")
cfm1 <- xtabs(~ y + preds, data = test)
cfm1
accuracy <- (cfm1["0","0"] + cfm1["1","1"]) / sum(cfm1)
accuracy

#Q25
rpart.control(maxdepth=10)

tree10 <- rpart(y ~ ., data = train, method = "class", maxdepth = 10)
summary(tree10)
rpart.plot(tree10)
rpart.rules(tree10, cover= TRUE)


test$preds <- predict(tree10, newdata = test, type = "class")
cfm1 <- xtabs(~ y + preds, data = test)
cfm1
accuracy <- (cfm1["0","0"] + cfm1["1","1"]) / sum(cfm1)
accuracy



















