###################################################
##Function to simulate y values from linear model##
###################################################

gety <- function(x,intercept,slope,eps.sigma)

{
  y <- intercept + slope*x + rnorm(length(x),0,eps.sigma)
  return(y)
}

####################
##Simulation study##
####################

##generate the values of x
x<-rep(seq(1,10,1),20)

##initialize values for simulation
beta0 <- 3 ##intercept
beta1 <- 0.2 ##slope
sig <- 1 ##sd of error term

##run simulation 10000 times
reps <- 10000

##create an array to store the estimated slope from each rep
store.slope<-array(0,reps)

##if you are curious about how long your loop runs
start_time <- Sys.time() ##start time of loop

##if you want to reproduce results
set.seed(4630)

for (i in 1:reps)

{
  y<-gety(x, intercept=beta0, slope=beta1, eps.sigma=sig) 
  
  ##use least squares to obtain regression equation on simulated data
  result<-lm(y~x)

  ##store the estimated slope from this rep
  store.slope[i]<-result$coeff[2]
  
  ##optional line if you want to see how fast your loop is going
  print(paste("Iteration", i))
}

end_time <- Sys.time() ## end time of loop
end_time - start_time ##time taken by loop

##bias of est slope
mean(store.slope)-beta1 

##empirical variance of est slope
var(store.slope) 

##theoretical formula for variance of estimated slope
sig^2/sum((x-mean(x))^2)
