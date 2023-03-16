### Dependencies

library(tidyverse)
library(MASS)
library(ROCR)
library(ipred)
library(GGally)
library(boot)
library(dplyr)
library(glmnet)
library(tree)
library(randomForest)

raw_data = read.csv("cbb19.csv")



### EDA

data <- raw_data %>% mutate(tournament=!is.na(SEED), wr=W/G) %>%
  dplyr::select(wr, ADJOE, ADJDE, FTR, EFG_O, EFG_D, X3P_O, ADJ_T, tournament) %>%
  dplyr::mutate(tournament=as.factor(tournament))
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





## Prepare data
raw_data = read.csv("cbb19.csv")

data <- raw_data %>% mutate(tournament=!is.na(SEED), wr=W/G, tournament=factor(tournament)) %>%
  dplyr::select(wr, ADJOE, ADJDE, FTR, EFG_O, EFG_D, X3P_O, ADJ_T, tournament)
names(data) <- tolower(names(data))

### EDA
ggpairs(data[,-9], lower = list(continuous = wrap("smooth", alpha = 0.1, size=0.1))) +
  labs(title = "Correlation Scatterplot Matrix (see WR column/row)")

# Remove response columns
x <- model.matrix(wr~., data=data %>% dplyr::select(-c(tournament)))[, -1]

# Store the response variable
y <- data %>% pull(wr)

## Take sample
set.seed(4630)

samp_data <- sample.int(nrow(data), nrow(data)*0.75)

train = data[samp_data,]
test = data[-samp_data,]
x.train<-x[samp_data, ]
x.test<-x[-samp_data, ]
y.train<-y[samp_data]
y.test<-y[-samp_data]


####################
##Ridge Regression##
####################
# Use CV to find optimal lambda based on training set
set.seed(4630)

cv.out.ridge <- glmnet::cv.glmnet(x.train, y.train, alpha=0, thresh = 1e-23)
bestlam.ridge <- cv.out.ridge$lambda.min
bestlam.ridge
plot(cv.out.ridge)

# Fit model using training data
ridge.mod <- glmnet::glmnet(x.train, y.train, alpha=0, lambda=bestlam.ridge, thresh = 1e-25)
ridge.mod$beta
#7 preds, b/c ridge doesn't remove any
#adjoe, adjde, ftr, efg_o, efg_d, x3p_o, adj_t

# Test MSE with best lambda
ridge.pred <- predict(ridge.mod, newx=x.test)
ridge.mse <- mean((ridge.pred-y.test)^2)


#########
##Lasso##
#########
# Use CV to find optimal lambda based on training set
set.seed(4630)

cv.out.lasso <- glmnet::cv.glmnet(x.train, y.train, alpha=1, thresh = 1e-23)
bestlam.lasso <- cv.out.lasso$lambda.min
bestlam.lasso
plot(cv.out.lasso)

# Fit model using training data
lasso.mod<-glmnet::glmnet(x.train, y.train, alpha=1, lambda=bestlam.lasso, thresh = 1e-23)
lasso.mod$beta
#5 preds
#adjoe, ftr, efg_o, efg_d, adj_ts

# Test MSE with best lambda
lasso.pred<-predict(lasso.mod, newx=x.test)
lasso.mse <- mean((lasso.pred-y.test)^2)


#######
##OLS##
#######
# Fit model using training data
ols.mod <- lm(wr~., data=data.frame(x.train, wr=y.train))

# Test MSE
ols.pred <- predict(ols.mod, newdata=data.frame(x.test))
ols.mse <- mean((ols.pred-y.test)^2)


####################
##Model Comparison##
####################
mse.table <- c(Ridge=ridge.mse, Lasso=lasso.mse, OLS=ols.mse)
mse.table

coef.mat <- cbind(coefficients(ridge.mod), coefficients(lasso.mod), coefficients(ols.mod))
colnames(coef.mat) <- c("Ridge", "Lasso", "OLS")
coef.mat




###################
##Regression Tree##
###################

##############################
##Recursive Binary Splitting##
##############################
tree_model = tree(wr~., data=train)
summary(tree_model)

plot(tree_model)
text(tree_model, cex=1)


###########
##Pruning##
###########
set.seed(4630)
pruned_tree_cv = cv.tree(tree_model, K=10)
pruned_tree_cv

min_dev_size = pruned_tree_cv$size[which.min(pruned_tree_cv$dev)]
min_dev_size

pruned_tree = tree::prune.tree(tree_model, best = min_dev_size)
summary(pruned_tree)

plot(pruned_tree)
text(pruned_tree, cex=1)


############
##Test MSE##
############
tree_pred = predict(tree_model, test)
mean((tree_pred-test$wr)^2)


##################
##Random Forests##
##################
set.seed(4630)
rf_tree = randomForest(wr~., data=train, mtry=3, importance=TRUE)
importance(rf_tree)

rf_tree_pred = predict(rf_tree, test)
mean((rf_tree_pred-test$wr)^2)



#######################
##Classification Tree##
#######################

###########################
##Fit classification tree##
###########################
ctree<-tree::tree(tournament~., data=train)

summary(ctree)

plot(ctree)
text(ctree, cex=0.5, pretty=0)


##############
##Prune tree##
##############
set.seed(4630)
ctree_p<-tree::cv.tree(ctree, K=10)

summary(ctree_p)

plot(ctree_p$size, ctree_p$dev,type='b')
x<-ctree_p$size[which.min(ctree_p$dev)]

ctree_p0<-tree::prune.tree(ctree, best=x)

summary(ctree_p0)

plot(ctree_p0)
text(ctree_p0, cex=0.6, pretty=0)


######################
##Confusion matrices##
######################
tournament<-test$tournament
pred_ctree<-predict(ctree, newdata=test, type="class")
pred_ctree_p0<-predict(ctree_p0, newdata=test, type="class")
t1<-table(tournament, pred_ctree)
t2<-table(tournament, pred_ctree_p0)
t1
t2

# Error Rate
1-mean(tournament==pred_ctree)
1-mean(tournament==pred_ctree_p0)

# FPR
t1[1, 2]/sum(t1[1,])
t2[1, 2]/sum(t2[1,])

# FNR
t1[2, 1]/sum(t1[2,])
t2[2, 1]/sum(t2[2,])

####Changing threshold to 0.24####
ctree_prob<-predict(ctree, test)
ctree_p0_prob<-predict(ctree_p0, test)
t3<-table(tournament, ctree_prob[,2]>0.2)
t4<-table(tournament, ctree_p0_prob[,2]>0.2)
t3
t4

# Error Rate
1-mean(tournament==(ctree_prob[,2]>0.2))
1-mean(tournament==(ctree_p0_prob[,2]>0.2))

# FPR
t3[1, 2]/sum(t3[1,])
t4[1, 2]/sum(t4[1,])

# FNR
t3[2, 1]/sum(t3[2,])
t4[2, 1]/sum(t4[2,])


##################
##Random Forests##
##################
set.seed(4630)

rp<-floor(sqrt(ncol(train)-1))
ctree_rf<-randomForest::randomForest(tournament~., data=train, mtry=rp, importance=TRUE)

importance(ctree_rf)

randomForest::varImpPlot(ctree_rf)

ctree_rf
1-(t5[1, 1]+t5[2, 2])/sum(t5)
t5<-ctree_rf$confusion[,-3]
t5[1, 2]/sum(t5[1,])
t5[2, 1]/sum(t5[2,])
