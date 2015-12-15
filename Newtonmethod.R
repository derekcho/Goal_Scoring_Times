#Load Goal Scoring dataset
dat<- read.csv("Goal_Scoring_Times_Clean.csv", header=T)

#Remove Stoppage Time
Time.of.Goal.ns <- vector(length=length(dat$Time.of.Goal))
for(i in 1:length(dat$Time.of.Goal))
{
	Time.of.Goal.ns[i] <- as.numeric(substr(dat$Time.of.Goal[i],1,2))
}

dat <- cbind(dat,Time.of.Goal.ns)

View(dat)

a <- 1
b <- 1
B.old <- matrix(c(a,b), nrow =2)
x <- (dat$Time.of.Goal.ns/90.001)
n <- length(x)

d = 1
i = 0

while(d > 1e-6){
	a <- B.old[1]
	b <- B.old[2]
	i = i+1
	print(i)
	g1 <- -digamma(a) + digamma(a+b) + mean(log(x))
	g2 <- -digamma(b) + digamma(a+b) + mean(log(1-x))

	g11 <- -trigamma(a) + trigamma(a+b)
	g22 <- -trigamma(b) + trigamma(a+b)
	g12 <- g21 <- trigamma(a+b)

	g <- matrix(c(g1,g2), nrow =2)
	G <- matrix(c(g11,g21,g12,g22), nrow = 2, ncol =2)

  B.new <- B.old - solve(G) %*% g

  d = abs(sqrt(sum((B.new - B.old)^2)))
  
  B.old <- B.new
}
 print(B.new)
 