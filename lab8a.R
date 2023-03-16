##Fitting Classification Trees
library(tree) ##to fit trees

library(MASS) ##for Boston dataset

########################################################
##Convert response to become binary and data splitting##
########################################################

##Convert medv to a binary variable called High. The response must be a factor for the tree() function to know to fit a classification tree. If coded as 0/1, tree() will fit a regression tree.
High<-ifelse(Boston$medv<median(Boston$medv), "No", "Yes")
High<-factor(High)
contrasts(High) ##yes is coded as 1

##add High to data frame, and remove the old response variable
Boston.new<-data.frame(Boston[,-14], High)

##convert chas from 0/1 dummy codes to factor. 
Boston.new$chas<-factor(Boston.new$chas)

##Split data
set.seed(1)
sample.data<-sample.int(nrow(Boston.new), floor(.50*nrow(Boston.new)), replace = F)
train<-Boston.new[sample.data, ]
test<-Boston.new[-sample.data, ]

##store the response variable for test data. Use later to evaluate test error rate.
pred.test<-test[,"High"]

##############################
##Recursive Binary Splitting##
##############################

##Use recursive binary splitting on training data
tree.class.train<-tree::tree(High~., data=train)
summary(tree.class.train) ##16 terminal nodes! difficult to interpret

##plot tree
plot(tree.class.train)
text(tree.class.train, cex=0.6, pretty=0) ##Note: If there are categorical predictors, should have an additional argument: pretty=0 so R will use the category names in the tree

##find predicted classes for test data
tree.pred.test<-predict(tree.class.train, newdata=test, type="class") ##type="class" to get predicted class based on threshold of 0.5
head(tree.pred.test)

##find predicted probabilities for test data
pred.probs<-predict(tree.class.train, newdata=test)
head(pred.probs)

##confusion matrix for test data
table(pred.test, tree.pred.test) ##actual classes in rows, predicted classes in columns

##overall accuracy
mean(tree.pred.test==pred.test) ##0.838

##confusion matrix with different threshold of 0.7 for example
table(pred.test, pred.probs[,2]>0.7) 

##############
##Prune tree##
##############

##use CV
set.seed(2)
cv.class<-tree::cv.tree(tree.class.train, K=10, FUN=prune.misclass) ##FUN=prune.misclass so error rate is used to guide CV and pruning, rather than the deviance which is the default (and should not be used in classification).
cv.class

##plot of dev against size
plot(cv.class$size, cv.class$dev,type='b')

##size of tree chosen by pruning
trees.num.class<-cv.class$size[which.min(cv.class$dev)]
trees.num.class ##4 terminal nodes. A lot smaller than recursive binary splitting

##fit tree with size chosen by pruning
prune.class<-tree::prune.misclass(tree.class.train, best=trees.num.class)
prune.class

##plot pruned tree
plot(prune.class)
text(prune.class, cex=0.75, pretty=0)

##prediction based on pruned tree for test data
tree.pred.prune<-predict(prune.class, newdata=test, type="class")
##confusion matrix for test data
table(pred.test, tree.pred.prune)

##overall accuracy
mean(tree.pred.prune==pred.test) ##0.849. small improvement over recursive binary splitting