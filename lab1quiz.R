library(ISLR2)
library(MASS)  
library(multcomp)

##1. How many rows and columns are in this data set?
nrow(Auto)
ncol(Auto)

##2. Should we remove the column containing the name of the cars from our analysis?

Data <- Auto[,1:8]
##Yes, because the names are almost exclusively unique, which means it will not give us much useful information.

##3. How many variables are there in this data set? How many of the variables are quantitative?
##How many of the variables are categorical? Are there some variables where
##this distinction may not be obvious? Briefly explain. Reading the documentation will be useful.

##9, 7 of which are quantitative (mpg, displacement, horsepower, weight, acceleration, year, cylyinders) and 2 of which
##are categorical (origin, name). The distinction isn't obvious for cylinders because the number of cylinders
##could have mathematical significance.

##4. Use the factor() function to convert the variable origin to be viewed appropriately as
##a categorical variable. Also give appropriate descriptive names for each of the levels.

Data$origin <- factor(Auto$origin)
levels(Data$origin) <- c("American", "European", "Japanese")
head(Data$origin)

##5. Produce a scatterplot matrix for all the quantitative variables in the data set.

pairs(Data[,c(1:7)], lower.panel = NULL, main="Scatterplot of Quantitative Variables")

##6. Produce a correlation matrix for all the quantitative variables in the data set.

round(cor(Data[,c(1:7)]),3)

##7. What relationships do you see, if any, among the variables based on your output from
##questions 5 and 6?

##Weight seems to have a high linear correlation with displacement (positive), horsepower (positive),
##and mpg (negative). Also, mpg seems to have a high non-linear correlation with displacement and horsepower
##(both negative). Finally, displacement and horsepeower seem to have a high positive linear correlation.

##8. Produce side by side boxplots for the gas mileage for American, European, and Japanese
##cars. Briefly comment on the boxplots.

boxplot(Data$mpg~Data$origin, main="Gas Mileage by Country of Origin")

##9. Fit a multiple linear regression model with mpg as the response variable and all the
##other variables (except name) as predictors. Create the necessary plots to assess if the
##model assumptions are met. Are there transformations you should try?

result<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin, data=Data)
par(mfrow=c(2,2))
plot(result)

par(mfrow=c(1,1))
boxcox(result)
##Should try transforming y 

##10. Your classmate suggests transforming the response by y^∗ = y^−0.5. Do you agree? Briefly explain.

##Yes, because -0.5 is within the lines of the boxcox plot.

##11. Transform the response variable with y^∗ = y^−0.5, and re-fit the regression model.

Data$mpg <- Data$mpg**-0.5

result2<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin, data=Data)

##Create the necessary plots to assess if the model assumptions are met.

par(mfrow=c(2,2))
plot(result2)

par(mfrow=c(1,1))
boxcox(result2)

summary(result2)

##For this model, answer the following:
##  (a) How do we interpret the result of the F statistic of 392.8?

##Our model is useful.

##  (b) Based on the t tests, what predictor(s) will you consider dropping from the model in order to simplify the model?

##Acceleration

##  (c) Drop the(se) predictor(s), re-fit the model, and compare this model with the one from part 11 using an
##      appropriate hypothesis test. Which of these two models should you use?

reduced<-lm(mpg~cylinders+displacement+horsepower+weight+year+origin, data=Data)

anova(reduced, result2)

##We would use the reduced model.

##12. Perform pairwise comparisons (using Tukey’s method) to see if the response variable differs by the origin of the car,
##while controlling for the other predictors. How do American, European, and Japanese cars differ in their fuel efficiency?

pairwise<-multcomp::glht(reduced, linfct = mcp(origin = "Tukey"))
summary(pairwise)

##American cars are slightly less fuel efficient on average than European or Japanese cars.

