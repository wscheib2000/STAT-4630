##############################################################
##some edits to the data set as suggested by R documentation##
##############################################################

UA.C <- USArrests
##Maryland's urban population transcribed wrongly
UA.C["Maryland", "UrbanPop"] <- 76.6

## also +/- 0.5 to restore the original  <n>.5  percentages
s5u <- c("Colorado", "Florida", "Mississippi", "Wyoming")
s5d <- c("Nebraska", "Pennsylvania")
UA.C[s5u, "UrbanPop"] <- UA.C[s5u, "UrbanPop"] + 0.5
UA.C[s5d, "UrbanPop"] <- UA.C[s5d, "UrbanPop"] - 0.5

##perform PCA, with scaling. centering is done by default, scaling not done by default
pr.out<-prcomp(UA.C, scale=TRUE)

##see what is stored from prcomp()
names(pr.out)

##obtain the loading vector for the PCs
pr.out$rotation
##notice the signs for the loadings for the first PC are all negative
##interpretation may be counterintuitive
##this means postive scores on first PC are associated with smaller than average values for the features

##obtain the scores for each observation
pr.out$x

##change signs to make interpretation more intuitive, so that positive scores are above average values
pr.out$rotation<- -pr.out$rotation
pr.out$x<- -pr.out$x

##recheck first PC
pr.out$rotation[,1]

##recheck California
pr.out$x["California"]

##create biplot
biplot(pr.out, scale=0)

##variance of each PC
pr.var<-pr.out$sdev^2
pr.var

##proportion of variance in features explained by each PC
pve<-pr.var/sum(pr.var)
pve

##Scree plot
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", main="Scree Plot", ylim=c(0,1),type='b')

##Cumulative plot
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", main="Cumulative Proportion", ylim=c(0,1),type='b')



