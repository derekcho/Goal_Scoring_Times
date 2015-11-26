library(ggplot2)

#Load data
dat <- read.csv("Goal_Scoring_Times_Clean.csv", header=T)
str(dat)

#Convert goal times with stoppage times (45 + 1 becomes 45, etc.)
Time.of.Goal.ns <- vector(length=length(dat$Time.of.Goal))
for(i in 1:length(dat$Time.of.Goal))
{
	Time.of.Goal.ns[i] <- as.numeric(substr(dat$Time.of.Goal[i],1,2))
}

dat <- cbind(dat,Time.of.Goal.ns)

hist(dat$Time.of.Goal.ns, breaks=20, main="Histogram of EPL Goal Scoring Times 2014/15", xlab="Time of Goal")
table(dat$Time.of.Goal.ns)


table(dat$Team.Scored)
