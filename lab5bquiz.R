library(ISLR2)
library(moments)
library(boot)

## 1
hist(College$Outstate)

## 2
skew<-skewness(College$Outstate)

## 3
# Skewed right

## 4_0
set.seed(1819)

my.skew<-function(variable,index)
{
  return(skewness(variable[index]))
}

my.boot<-boot(College$Outstate,my.skew,R=10000)
my.boot

## 4
mean(my.boot$t)-skew

## 5
my.boot

## 6
boot.ci(my.boot, conf=0.95,type="basic")

## 7
# Yes, because 0 is not in the confidence interval
