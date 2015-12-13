gs <- read.csv("Goal_Scoring_Times_Clean.csv", header = TRUE)
View(gs)

a <- 4
b <- 5
B.old <- matrix(c(a,b), nrow =2)
x <- c(12,23,34,45,56,67,78,89,36)
n <- length(x)

d = 1
i = 0

while(d > 0.00000001){

	i = i+1
	g1 <- (digamma(a)/gamma(a)) - (digamma(a+b)/gamma(a+b)) + (1/n)*sum(log(x))
	g2 <- (digamma(b)/gamma(b)) - (digamma(a+b)/gamma(a+b)) + (1/n)*sum(log(x))

	g11 <- ((trigamma(a)/gamma(a)) - ((digamma(a)^2)/ (gamma(a)^2))) - ((trigamma(a+b)/gamma(a+b)) - ((digamma(a+b)^2)/ (gamma(a+b)^2)))
	g22 <- ((trigamma(b)/gamma(b)) - ((digamma(b)^2)/ (gamma(b)^2))) - ((trigamma(a+b)/gamma(a+b)) - ((digamma(a+b)^2)/ (gamma(a+b)^2)))
	g12 <-  (-((digamma(a+b)^2)/ (gamma(a+b)^2)))
	g21 <-  (-((digamma(a+b)^2)/ (gamma(a+b)^2)))

	g <- matrix(c(g1,g2), nrow =2)
	G <- matrix(c(g11,g21,g12,g22), nrow = 2, ncol =2)

  B.new = B.old - solve(G)%*%g

  d = abs(sqrt(sum((B.new - B.old)^2)))
  
  B.new = B.old
}
 print(B.new)
 