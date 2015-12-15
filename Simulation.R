library(dplyr)
library(plyr)

champ.adjust <- c((28 + 40 + 32)/ (83 + 72 + 60),(62 + 85 + 74)/ (43 + 37 + 44))
# sum of total goals for/against for the 3 relegated teams divided by 
# sum of total goals for/against for the 3 championship teams

HA.goals <- read.csv("2013-14 Total Goals.csv",header = T)

for (i in 1:nrow(HA.goals)) {
	if(is.na(HA.goals[i,]$Championship.Teams)){
		HA.goals[i,]$Championship.Teams <- FALSE
	}
	if(HA.goals[i,]$Championship.Teams == TRUE){
		HA.goals[i,c(2,4)] <- floor(HA.goals[i,c(2,4)] * champ.adjust[1])
		HA.goals[i,c(3,5)] <- floor(HA.goals[i,c(3,5)] * champ.adjust[2])
	}
}
HA.goals <- HA.goals[,-6]

total <- apply(HA.goals[,-1],FUN = sum,MARGIN = 2)

HA.goals <- HA.goals %>% mutate(HGF = Home.Goals.For*20/total[1],
																HGA = Home.Goals.Against*20/total[2],
																AGF = Away.Goals.For*20/total[2],
																AGA = Away.Goals.Against*20/total[1])

##########

simulate_poisson <- function(alpha,beta,home,team,opponent){
	stopifnot(is.logical(home))
	adjust <- numeric()
	if(home == TRUE){
		adjust <- HA.goals$HGF[HA.goals$Team == team]*
			HA.goals$AGA[HA.goals$Team == opponent]
	} else {
		adjust <- HA.goals$AGF[HA.goals$Team == team]*
			HA.goals$HGA[HA.goals$Team == opponent]
	}
	N <- rpois(1,lambda = 975/380/2*adjust)
	S <- numeric()
	if(N != 0){
		for (i in 1:N) {
			S <- append(S,rbeta(1,shape1 = alpha,shape2 = beta))
		}
	}
	return(floor(sort(S)*90))
}

simulate_bayesian <- function(alpha,beta,home,team,opponent){
	stopifnot(is.logical(home))
	adjust <- numeric()
	if(home == TRUE){
		adjust <- HA.goals$HGF[HA.goals$Team == team]*
			HA.goals$AGA[HA.goals$Team == opponent]
	} else {
		adjust <- HA.goals$AGF[HA.goals$Team == team]*
			HA.goals$HGA[HA.goals$Team == opponent]
	}
	N <- rpois(1,lambda = 975/380/2*adjust)
	S <- numeric()
	if(N != 0){
		for (i in 1:N) {
			S <- append(S,rbeta(1,shape1 = rexp(1,rate = 1/alpha),shape2 = rexp(1,rate = 1/beta)))
		}
	}
	return(floor(sort(S)*90))
}

# Example: Chelsea (Home) vs. Aston Villa (Away)
# Poisson Process
set.seed(1)
S_Chelsea <- simulate_poisson(alpha = 1.074,beta = 0.84,
															home = TRUE,team = "Chelsea",opponent = "Aston Villa")
S_Chelsea

S_Aston <- simulate_poisson(alpha = 1.074,beta = 0.84,
														home = FALSE,team = "Aston Villa",opponent = "Chelsea")
S_Aston

N <- 20
plot(x = NULL,y = NULL,xlim = c(1,N),ylim = c(0,90),
		 xlab = "Round of Simulation",ylab = "Scoring Times")
for (i in 1:N) {
	S_Chelsea <- simulate_poisson(alpha = 1.1225,beta = 0.8149,
																home = TRUE,team = "Chelsea",opponent = "Aston Villa")
	S_Aston <- simulate_poisson(alpha = 1.1225,beta = 0.8149,
															home = FALSE,team = "Aston Villa",opponent = "Chelsea")
	if(length(S_Chelsea) != 0){
		for(j in S_Chelsea) {
			points(x = i,y = j,col = "blue",pch = 4)
		}
	}
	if(length(S_Aston) != 0){
		for (k in S_Aston) {
			points(x = i,y = k,col = "red",pch = 4)
		}
	}
	abline(v = i)
}

# bayesian Process
S_Chelsea <- simulate_bayesian(alpha = 1.074,beta = 0.84,
															home = TRUE,team = "Chelsea",opponent = "Aston Villa")
S_Chelsea

S_Aston <- simulate_bayesian(alpha = 1.074,beta = 0.84,
														home = FALSE,team = "Aston Villa",opponent = "Chelsea")
S_Aston

N <- 20
plot(x = NULL,y = NULL,xlim = c(1,N),ylim = c(0,90),
		 xlab = "Round of Simulation",ylab = "Scoring Times",title = "Chelsea (H) vs. Aston Villa 20 simulations")
for (i in 1:N) {
	S_Chelsea <- simulate_bayesian(alpha = 1.1225,beta = 0.8149,
																home = TRUE,team = "Chelsea",opponent = "Aston Villa")
	S_Aston <- simulate_bayesian(alpha = 1.1225,beta = 0.8149,
															home = FALSE,team = "Aston Villa",opponent = "Chelsea")
	if(length(S_Chelsea) != 0){
		for(j in S_Chelsea) {
			points(x = i,y = j,col = "blue",pch = 4)
		}
	}
	if(length(S_Aston) != 0){
		for (k in S_Aston) {
			points(x = i,y = k,col = "red",pch = 4)
		}
	}
	abline(v = i)
}
