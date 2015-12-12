#Do a MH-MCMC Algorithm to estimate parameters alpha, beta, and gamma
library(dplyr)

mh=function(n,n0,x){
	#n is the desired sequence length
	#n0 is the "burn-in" parameter
	res=matrix(nrow=(n+n0),ncol=3)
	res[1,]=c(1,1,1)
	
	N <- length(x)
	C.alpha <- sum(log(x))
	C.beta <- sum(log(1-x))
	
	for(i in 2:(n+n0)){
			
		alpha.1 <- res[i-1,1]
		beta.1 <- res[i-1,2]
		gamma.1 <- res[i-1,3]
		
		u.alpha=1;u.beta=1;u.gamma=1
		test.alpha=0;test.beta=0;test.gamma=0;

		while(u.alpha > test.alpha)
		{
			alpha.prime <- rnorm(1,alpha.1,1)
			while(alpha.prime <= 0)
			{
				alpha.prime <- rnorm(1,alpha.1,1)
			}
			u.alpha <- log(runif(1))
			test.alpha <- C.alpha*(alpha.prime-alpha.1) + (alpha.1-alpha.prime) + ((beta(alpha.1,beta.1)-beta(alpha.prime,beta.1))*N) + log( dexp(alpha.1)/dexp(alpha.prime))
		}
		
		while(u.beta > test.beta)
		{
			beta.prime <- rnorm(1,beta.1,1)
			while(beta.prime <= 0)
			{
				beta.prime <- rnorm(1,beta.1,1)
			}
			u.beta <- log(runif(1))
			test.beta <- C.beta*(beta.prime-beta.1) + (beta.1-beta.prime) + ((beta(beta.1,beta.1)-beta(beta.prime,beta.1))*N) + log( dexp(beta.1)/dexp(beta.prime))
		}
		
		while(u.gamma > test.gamma)
		{
			gamma.prime <- rnorm(1,gamma.1,1)
			while(gamma.prime <= 0)
			{
				gamma.prime <- rnorm(1,gamma.1,1)
			}
			u.gamma <- runif(1)
			test.gamma <-  ((gamma.prime/gamma.1)^N) * exp(-(gamma.prime-gamma.1)) 
		}
		
		res[i,] <- c(alpha.prime, beta.prime, gamma.prime)
	}
	return(res[(n0+1):(n+n0),]) 
}

#Simulate data from uniform(0,1)
set.seed(1)
x <- runif(300)
#x <- rnorm(200,0.5,0.1)
hist(x, prob=T)

#Apply Metropolis-Hastings
re=mh(10000,5000,x)
params <- apply(re,2,mean)

#Plot results
#plot(dbeta(seq(0,1,by=0.01), shape1 = params[1], shape2 = params[2]), ylim = c(0,5))
#curve(dnorm(x,0.5,0.1), add=T)
curve(dbeta(x,shape1=params[1], shape2=params[2]), ylim=c(0,5), add=T)

#quantile(re[,3], probs=c(0.025,0.975))

#Load Goal Scoring dataset
dat<- read.csv("Goal_Scoring_Times_Clean.csv", header=T)

#Remove Stoppage Time
Time.of.Goal.ns <- vector(length=length(dat$Time.of.Goal))
for(i in 1:length(dat$Time.of.Goal))
{
	Time.of.Goal.ns[i] <- as.numeric(substr(dat$Time.of.Goal[i],1,2))
}

dat <- cbind(dat,Time.of.Goal.ns)

#Sample 300 points from data and apply MH
y <- sample(dat$Time.of.Goal.ns/90.01,300)
re=mh(10000,5000,y)
params <- apply(re,2,mean)

#Plot results
hist(y, probability = T)
curve(dbeta(x,shape1=params[1], shape2=params[2]), ylim=c(0,5), add=T)
#plot(dbeta(seq(0,1,by=0.01), shape1 = params[1], shape2 = params[2]), ylim = c(0,2))
