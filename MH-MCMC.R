#Do a MH-MCMC Algorithm to estimate parameters alpha, beta, and gamma

mh=function(n,n0,x){
	#n is the desired sequence length
	#n0 is the "burn-in" parameter
	res=matrix(nrow=(n+n0),ncol=3)
	res[1,]=c(1,1,1)
	
	N <- length(x)
	C.alpha <- prod(x)
	C.beta <- prod((1-x))
	
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
			u.alpha <- runif(1)
			test.alpha <- C.alpha^(alpha.prime-alpha.1) * exp(-(alpha.prime-alpha.1)) * ((beta(alpha.1,beta.1)/beta(alpha.prime,beta.1))^N) *( dexp(alpha.1)/dexp(alpha.prime))
		}
		
		while(u.beta > test.beta)
		{
			beta.prime <- rnorm(1,beta.1,1)
			while(beta.prime <= 0)
			{
				beta.prime <- rnorm(1,beta.1,1)
			}
			u.beta <- runif(1)
			test.beta <- C.beta^(beta.prime-beta.1) * exp(-(beta.prime-beta.1)) * ((beta(alpha.1,beta.1)/beta(alpha.1,beta.prime))^N) * (dexp(beta.1)/dexp(beta.prime))
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

set.seed(1)
x <- runif(100)
#x <- rnorm(50,0.5,0.1)
hist(x)
re=mh(10000,0,x)
params <- apply(re,2,mean)

plot(dbeta(seq(0,1,by=0.01), shape1 = params[1], shape2 = params[2]))
plot(dnorm(seq(-3,3, by=0.01)))
plot(dbeta(seq(0,1,by=0.01),shape1=2, shape2=2))

quantile(re[,3], probs=c(0.025,0.975))

