### Dependencies

library(tidyverse)
library(MASS)
library(ROCR)
library(ipred)
library(GGally)
library(boot)

raw_data = read.csv("cbb19.csv")



### EDA

data <- raw_data %>% mutate(tournament=!is.na(SEED), wr=W/G) %>%
  dplyr::select(wr, ADJOE, ADJDE, FTR, EFG_O, EFG_D, X3P_O, ADJ_T, tournament)
names(data) <- tolower(names(data))

set.seed(4630)

samp_data <- sample.int(nrow(data), nrow(data)*0.75)
train <- data[samp_data,]
test <- data[-samp_data,]

train %>% dplyr::select(-c(tournament)) %>% 
  ggpairs(
    aes(colour = train$tournament, alpha = 0.1),
    showStrips = F,
    upper = list(continuous = wrap("cor", size = 3))
  ) +
    labs(title = "Predictors by Tournament Participation", colour = "In Tournament") + 
    theme(text = element_text(size = 10))



### LDA

# ROC/AUC
set.seed(4630)
contrasts(train$tournament)
lda.mod <- MASS::lda(tournament ~ ., data=train)
lda.mod

lda.test <- predict(lda.mod,test)
preds<-lda.test$posterior[,2]
rates<-ROCR::prediction(preds, test$tournament)
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for Whether or Not Team Made Playoffs")
lines(x = c(0,1), y = c(0,1), col="red")

auc<-ROCR::performance(rates, measure = "auc")
paste("AUC:", auc@y.values, sep=" ")

# kF-CV
cv.da <- function(object, newdata) {
  return(predict(object, newdata = newdata)$class)
}
k5 <- ipred::errorest(tournament ~ ., data=data, model=lda, estimator="cv",
                      est.para=control.errorest(k=5), predict=cv.da)$err
k10 <- ipred::errorest(tournament ~ ., data=data, model=lda, estimator="cv",
                       est.para=control.errorest(k=10), predict=cv.da)$err
c(k5, k10)

# True Error Rate
conf<-table(test$tournament,lda.test$class)
conf
err<-1-mean(test$tournament == lda.test$class)
paste("Error Rate:", err, sep=" ")



### LOGISTIC

######## FULL LOG. REG. MODEL ########
full<-glm(tournament~., family=binomial, data=train)
summary(full)

## ROC
preds<-predict(full,newdata=test, type="response")
rates<-prediction(preds, test$tournament)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

## AUC
auc<-performance(rates, measure = "auc")
auc@y.values

## Cross Validation
set.seed(4630)
k5=boot::cv.glm(train,full, K=5)
set.seed(4630)
k10=boot::cv.glm(train,full, K=10)

k5$delta[2] # 5 k-fold
k10$delta[2] # 10 k-fold

## Test Accuracy
logreg_table = table(test$tournament,preds > 0.5)
1-sum(diag(logreg_table))/nrow(test) # Error Rate

######## REDUCED LOG. REG. MODEL ########
reduced=glm(tournament~wr+adjoe+ftr, family=binomial, data=train)
summary(reduced)

## ROC
preds<-predict(reduced,newdata=test, type="response")
rates<-prediction(preds, test$tournament)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

## AUC
auc<-performance(rates, measure = "auc")
auc@y.values[[1]]

## Cross Validation
set.seed(4630)
k5=boot::cv.glm(train,reduced, K=5)
set.seed(4630)
k10=boot::cv.glm(train,reduced, K=10)

k5$delta[2] # 5 k-fold
k10$delta[2] # 10 k-fold

logreg_table = table(test$tournament,preds > 0.5)
logreg_table
1-sum(diag(logreg_table))/nrow(test) # Error Rate

## LRT
resdev_Diff = reduced$dev - full$dev
1-pchisq(resdev_Diff, 5)
