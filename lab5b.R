#############
##bootstrap##
#############

library(boot) ##for boot function
library(ISLR2) ##for College dataset
Data<-College

##need to write own function to use with boot. Function calculates needed estimator. Function must have 2 arguments, the variable of interest, and an index for which parts of the dataframe to use
my.90th<-function(variable,index)
{
  return(quantile(variable[index],0.9))
}

set.seed(2003)
##supply vector containing the variable to sample with replacement, your function written earlier to compute the estimator of interest, and the number of bootstrap samples to obtain. The boot function calls this argument R (denoted as B in our textbook and by most people).
my.boot<-boot::boot(Data$Outstate,my.90th,R=10000)
my.boot

boot::boot.ci(my.boot, conf=0.95,type="basic")