library(dplyr)
library(glmnet)
library(tree)
library(randomForest)

## Prepare data
raw_data = read.csv("cbb19.csv")

data <- raw_data %>% mutate(tournament=!is.na(SEED), wr=W/G, tournament=factor(tournament)) %>%
  dplyr::select(wr, ADJOE, ADJDE, FTR, EFG_O, EFG_D, X3P_O, ADJ_T, tournament)
names(data) <- tolower(names(data))

# Remove response columns
x <- model.matrix(wr~., data=data %>% select(-c(tournament)))[, -1]

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
