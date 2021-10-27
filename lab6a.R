#########
##Ridge##
#########

Data<-mtcars
library(glmnet)

##model.matrix automatically transform categorical variables into dummy codes, which is needed as the glmnet function cannot handle categorical variables
x<-model.matrix(mpg~.,data=Data)
head(x)

##remove first column
x<-model.matrix(mpg~.,data=Data)[,-1]
head(x)

##store the response variable
y<-Data$mpg

##alpha=0 for ridge, alpha=1 for LASSO
##threshold value should be very small if multicollinearity is present. see what happens if thresh was set to a larger value
##we know theoretically the coeffs should be the same as lm when lambda is 0
ridge.r<-glmnet::glmnet(x,y,alpha=0, lambda=0)
##compare with OLS
result<-lm(mpg~.,data=Data)
cbind(coefficients(result), coefficients(ridge.r))
##not the same. Need smaller threshold

ridge.r<-glmnet::glmnet(x,y,alpha=0, lambda=0, thresh = 1e-23)
##compare with OLS
cbind(coefficients(result), coefficients(ridge.r))

##split data
set.seed(12)
sample.data<-sample.int(nrow(Data), floor(.50*nrow(Data)), replace = F) ##observations to belong to the training data
x.train<-x[sample.data,]
x.test<-x[-sample.data,]
y.train<-y[sample.data]
y.test<-y[-sample.data]

##use CV to find optimal lambda based on training set
set.seed(12)
cv.out<-glmnet::cv.glmnet(x.train,y.train,alpha=0, thresh = 1e-23)
bestlam<-cv.out$lambda.min
bestlam
plot(cv.out)

##fit ridge regression using training data and bestlam
ridge.mod<-glmnet::glmnet(x.train,y.train,alpha=0,lambda=bestlam, thresh = 1e-25)

##Test MSE with best lambda
ridge.pred<-predict(ridge.mod,newx=x.test)
mean((ridge.pred-y.test)^2)

##fit OLS by setting lambda=0
ridge.mod.0<-glmnet::glmnet(x.train,y.train,alpha=0,lambda=0, thresh = 1e-23)

##test MSE with lambda=0
ridge.pred.0<-predict(ridge.mod.0,newx=x.test)
mean((ridge.pred.0-y.test)^2)

##Compare ridge with OLS using best lambda and all observations
out.ridge<-glmnet::glmnet(x,y,alpha=0,lambda=bestlam,thresh = 1e-23)
out.ols<-glmnet::glmnet(x,y,alpha=0, lambda=0, thresh = 1e-25)
cbind(coefficients(out.ridge), coefficients(out.ols))

##Create plot of ridge coeff against lambda
grid<-10^seq(10,-2,length=100)
out.all<-glmnet::glmnet(x,y,alpha=0,lambda=grid,thresh = 1e-23)
plot(out.all, xvar = "lambda")
abline(v=log(bestlam), lty=2)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

#########
##Lasso##
#########

lasso.r<-glmnet::glmnet(x,y,alpha=1, lambda=0, thresh = 1e-23)
##compare with OLS
cbind(coefficients(result), coefficients(lasso.r))

set.seed(12)
cv.out.lasso<-glmnet::cv.glmnet(x.train,y.train,alpha=1, thresh = 1e-23)
bestlam.lasso<-cv.out.lasso$lambda.min
bestlam.lasso
plot(cv.out.lasso)

##fit ridge regression using training data
lasso.mod<-glmnet::glmnet(x.train,y.train,alpha=1,lambda=bestlam.lasso, thresh = 1e-23)

##Test MSE with best lambda
lasso.pred<-predict(lasso.mod,newx=x.test)
mean((lasso.pred-y.test)^2)

##Create plot of ridge coeff against lambda
grid<-10^seq(10,-2,length=100)
out.all<-glmnet::glmnet(x,y,alpha=1,lambda=grid,thresh = 1e-23)
plot(out.all, xvar = "lambda")
abline(v=log(bestlam.lasso), lty=2)
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

