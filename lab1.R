################
##Reading Data##
################

Data<-read.table("cereals.txt", header=TRUE)

##see how shelf is viewed
class(Data$shelf)

##make R view shelf as categorical
Data$shelf<-factor(Data$shelf)
is.factor(Data$shelf)

head(Data$shelf)

##give descriptive names to shelf
levels(Data$shelf)
levels(Data$shelf) <- c("low", "middle", "top") ##order needs to match

head(Data$shelf)

#######
##EDA##
#######

##scatterplot
plot(Data$sugars,Data$calories,xlab="Sugars per serving",	ylab="Calories per serving", main="Plot of Calories against Sugars")

##correlation
cor(Data$sugars, Data$calories)

##scatterplot matrix
pairs(Data[,3:10], lower.panel = NULL, main="Scatterplot of Quantitative Variables")

##correlation matrix
cor(Data[,3:10])
round(cor(Data[,3:10]),3)

##boxplots
boxplot(Data$calories~Data$mfr, main="Calories by Manufacturer")

##obtain median of calories across manufacturers
tapply(Data$calories,Data$mfr,median)

##2 way table
mytab<-table(Data$shelf, Data$mfr)
mytab

##2 way table with proportions
prop.table(mytab, 2)

##############
##Regression##
##############

result<-lm(calories~sugars+carbo+protein+fat+sodium+fiber+potass, data=Data)

###############
##Diagnostics##
###############

par(mfrow=c(2,2))
plot(result)

library(MASS)
par(mfrow=c(1,1))
MASS::boxcox(result)

#############
##Inference##
#############

summary(result)

##fit reduced model
reduced<-lm(calories~sugars+carbo+protein+fat, data=Data)

##partial F test
anova(reduced,result)  ##significant -> use original model; insignificant -> use reduced model

##CI for parameters
confint(reduced,level = 0.95)

newdata<-data.frame(sugars=5, carbo=15, protein=3, fat=1.5)
predict(reduced,newdata,level=0.95, interval="confidence")
predict(reduced,newdata,level=0.95, interval="prediction")

names(reduced)
reduced$coefficients

##########################
##Categorical predictors##
##########################

class(Data$mfr)
contrasts(Data$mfr)

##change reference class
Data$mfr<-relevel(Data$mfr, ref = "other")
contrasts(Data$mfr)

##add mfr to reduced model (coefficients represent given class versus reference class;
                          ##ex. in this model we have no information about General Mills vs. Kellogg's)
reduced2<-lm(calories~sugars+carbo+protein+fat+mfr, data=Data)
summary(reduced2)

##multiple comparisons
library(multcomp)
pairwise<-multcomp::glht(reduced2, linfct = mcp(mfr= "Tukey"))
summary(pairwise)


